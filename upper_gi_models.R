# upper_gi_models.R
# Statistics for Upper GI risk factors
# Original: Ariel Israel - Feb 2025
# License: CC-BY-NC-4.0

# =========================
# 0) Reproducibility and Setup
# =========================
set.seed(12345)
options(width = 160)
options(max.print = 10000)

# Packages: prefer lightweight, CRAN-available
req_pkgs <- c(
  "survival", "broom", "dplyr", "forcats", "data.table",
  "readr", "DBI", "openxlsx", "stringr", "tidyr", "rlang"
)
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(req_pkgs, library, character.only = TRUE))

# -------------
# Project paths
# -------------
# Data directory is configurable via env var or defaults to "data/"
DATA_DIR <- Sys.getenv("DATA_DIR", unset = "data")
OUT_DIR  <- Sys.getenv("OUT_DIR",  unset = "out")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Output Excel name
SUBGROUP   <- Sys.getenv("SUBGROUP", unset = "stomach")  # options: "all", "stomach", "BMIgt27"
CUT_MEDIAN <- as.logical(Sys.getenv("CUT_MEDIAN", unset = "FALSE"))

OUT_XLSX <- file.path(
  OUT_DIR,
  sprintf("models_%s%s.xlsx", SUBGROUP, if (CUT_MEDIAN) "_cut" else "")
)

# Outcome coding
GROUP_CASE <- "CA"       # column name for case indicator after we create it
COND_CASE  <- "CA ~ "    # left side used to build formulas

# =========================
# 1) Helper stubs (public-safe)
# Replace these with your internal implementations if desired.
# =========================

# Safe NA fill
fillna <- function(vect,val) { vect2 <- vect; if(length(val)==1) vect2[is.na(vect2)] <- val else vect2[is.na(vect2)] <- val[is.na(vect2)]; return(vect2);}

# Set difference operator used later
"%w/o%" <- function(x,y) x[!x %in% y]

# Data loaders:
# Expected files:
#   - comparison_for_r.dta           (df_baseline)
#   - dump_measure.dta               (df_measures)
#   - dump_diags.dta                 (df_diags)
#   - dump_med.dta                   (df_meds)
#   - icd_categories.dta             (df_icd)
#   - acute_icd_categories.dta       (df_icd_acute)
load_data <- function(filename) {
  data <- tryCatch({
    # Attempt to read the file as a Parquet file
	library(arrow) 
    read_parquet(filename)
  }, error = function(e) {
    # If an error occurs (e.g., not a Parquet file), read it as a Stata file
    message("Failed to read as Parquet. Attempting to read as Stata file...")
	library(haven) 
    read_dta(filename)
  })
  return(data)
}

# Catalog/table fetchers: wire to DBI or CSV mirrors
get_table <- function(name) {
  # Example: medication catalog expected columns:
  # MedicationKey, ATC7Cd, ATC7Desc, ATC3Cd, MedicationNm, GivingFormCd1
  fname <- (name, ".csv")
  if (!file.exists(fname)) stop("Missing table CSV: ", fname)
  readr::read_csv(fname, show_col_types = FALSE, progress = FALSE)
}

# add_teur_to_df is domain-specific; here it is identity (no-op)
add_teur_to_df <- function(df) df

get_icd_categories <- function() {
  bind_rows(
    load_data("icd_categories.dta"),
    load_data("acute_icd_categories.dta")
  )
}
`%||%` <- function(x, y) if (!inherits(try(x, silent = TRUE), "try-error")) x else y

# Utility: choose last measure on/before index, else first after
get_last_entry_as_of_date_or_later_if_absent <- function(df_data, df_index, 
	col_date = "MeasureDt", col_index_date = "index_date", key = "cohort_id", or_later = TRUE) {
  # Convert to symbols for tidy evaluation
  col_date_sym <- sym(col_date)
  col_index_date_sym <- sym(col_index_date)
  key_syms <- syms(key)
  
  # Join and compute time direction
  df_combined <- df_data %>%
    inner_join(df_index, by = key) %>%
    arrange(!!!key_syms, !!col_date_sym) %>%
    mutate(before = !!col_date_sym <= !!col_index_date_sym)
  
  # Get last before
  df_before <- df_combined %>%
    filter(before) %>%
    group_by(!!!key_syms) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  if (or_later) {
    # Get first after for those without "before"
    df_no_before <- df_index %>%
      anti_join(df_before, by = key)
    
    df_after <- df_combined %>%
      semi_join(df_no_before, by = key) %>%
      filter(!before) %>%
      group_by(!!!key_syms) %>%
      slice_head(n = 1) %>%
      ungroup()
    
    df_result <- bind_rows(df_before, df_after)
  } else {
    df_result <- df_before
  }
  
  return(df_result)
}

# =========================
# 2) Parameters for this analysis
# =========================

MEDS_YEAR_LIMITS <- list(c(0,0.1), c(0.1,0.5), c(0.5,1), c(1,2), c(2,3), c(3,10), c(0,5))
DIAGS_YEAR_LIMIT <- c(7/365, 10)  # between ~1 week and 10 years prior to index

MEDS_TESTED <- unique(c("A02BC01_PO", "A02BC05_PO", "A02BC03_PO", "A02BA03_PO", "A12AA04_PO"))
DIAGS_TESTED <- unique(c("PUD","ABDOMINAL PAIN","CONSTIPATION","GERD","GASTRITIS","H PYLORI","DIARRHEA","ALCOHOLISM"))

COFACTORS <- c("age","genderF","SmokingNm","SES_category","HealthWorker","HadBeenPregnant","nb_visits","BMI.cat")

# =========================
# 3) Load data
# =========================

df_baseline <- load_data("comparison_for_r.dta")
df_measures <- load_data("dump/measure.dta") %>% filter(MeasureCd %in% c(15))  # 15=BMI

# Case indicator from "group" column
df_baseline[[GROUP_CASE]] <- df_baseline$group == GROUP_CASE
df_baseline$index <- NULL

# Smoking as factor with reference "Non-smoker"
df_baseline$SmokingNm <- forcats::fct_relevel(as.factor(df_baseline$SmokingNm), "Non-smoker")

# Choose last BMI before or first after index date
df_bmi_date <- get_last_entry_as_of_date_or_later_if_absent(
  df_data       = df_measures[df_measures$MeasureCd == 15, ],
  df_index      = df_baseline[, c("cohort_id","index_date")],
  col_date      = "MeasureDt",
  col_index_date= "index_date",
  key           = "cohort_id"
) %>%
  dplyr::select(cohort_id, BMI = MeasureValue1, BMIDt = MeasureDt) %>%
  mutate(
    BMI.cat  = cut(BMI, c(10,18.5,25,30,35,40,45,50,55,67), right = TRUE, include.lowest = TRUE),
    BMI.lower= as.numeric(sub("\\((.+),.*", "\\1", as.character(BMI.cat))),
    BMI.diff = ifelse(is.na(BMI.lower), 0, BMI - BMI.lower)
  )

# Diagnosis flags in lookback window
df_diags_categorization <- bind_rows(
  readr::read_csv(file.path(DATA_DIR, "icd_categories.csv"), show_col_types = FALSE),
  readr::read_csv(file.path(DATA_DIR, "acute_icd_categories.csv"), show_col_types = FALSE)
)
df_diags <- load_data("dump/diags.dta")
df_diags <- df_diags %>%
  inner_join(df_diags_categorization, by = "ICDCd") %>%
  inner_join(df_baseline[, c("cohort_id","index_date")], by = "cohort_id") %>%
  mutate(
    years_before = as.numeric(difftime(as.Date(index_date), as.Date(VisitDt), units = "days")) / 365.25
  ) %>%
  filter(years_before > 0)

diag_cols <- character(0)
for (cat_id in DIAGS_TESTED) {
  new_col <- make.names(cat_id)
  flag_ids <- df_diags$cohort_id[
    (df_diags$cat_id == cat_id) &
    (df_diags$years_before > DIAGS_YEAR_LIMIT[1]) &
    (df_diags$years_before <= DIAGS_YEAR_LIMIT[2])
  ]
  df_baseline[[new_col]] <- df_baseline$cohort_id %in% flag_ids
  diag_cols <- c(diag_cols, new_col)
}

# Visits count in the lookback window
df_visits <- df_diags %>%
  filter(years_before > DIAGS_YEAR_LIMIT[1], years_before <= DIAGS_YEAR_LIMIT[2]) %>%
  group_by(cohort_id) %>%
  summarise(nb_visits = n(), .groups = "drop")

# Build population frame
df_pop <- df_baseline %>%
  left_join(df_bmi_date, by = "cohort_id") %>%
  left_join(df_visits, by = "cohort_id")

df_pop$BMI.cat <- fct_na_value_to_level(df_pop$BMI.cat, level = "~missing~")
df_pop$BMI.cat <- relevel(df_pop$BMI.cat, ref = "(18.5,25]")
df_pop$BMI.diff <- fillna(df_pop$BMI.diff, 0)
df_pop$nb_visits <- fillna(df_pop$nb_visits, 0)

# Medication catalogs
df_meds_catalog <- get_table("medication")

df_meds <- load_data("dump/med.dta")
df_meds_count <- df_meds %>% group_by(MedicationKey) %>% summarise(nb = n(), .groups = "drop")
df_meds_count <- add_teur_to_df(
  dplyr::left_join(
    df_meds_count,
    df_meds_catalog[, c("MedicationKey","ATC7Cd","MedicationNm","GivingFormCd1")],
    by = "MedicationKey"
  )
)

med_names <- add_teur_to_df(df_meds_catalog[, c("MedicationKey","ATC3Cd","ATC7Cd","GivingFormCd1","ATC7Desc")])
med_names$med_id   <- make.names(paste(med_names$ATC7Cd, med_names$GivingFormCd1, sep = "_"))
med_names$med_name <- paste(med_names$ATC7Desc, med_names$GivingFormCd1)
med_names_by_med_id <- med_names %>% distinct(med_id, med_name, .keep_all = FALSE)

df_meds_analysis <- df_meds %>%
  inner_join(med_names[med_names$ATC7Cd %in% substring(MEDS_TESTED, 1, 7), c("MedicationKey","ATC7Cd","med_id")], by = "MedicationKey") %>%
  inner_join(df_pop[, c("cohort_id","index_date")], by = "cohort_id") %>%
  mutate(years_before = as.numeric(difftime(as.Date(index_date), as.Date(IssueDt), units = "days")) / 365.25)

# =========================
# 4) Medication exposure flags over time windows
# =========================
med_cols <- character(0)

for (lim in MEDS_YEAR_LIMITS) {
  for (med in MEDS_TESTED) {
    year_suffix <- sprintf("_%s_%sy", lim[1], lim[2])
    new_col <- paste0(med, year_suffix)
    df_sub <- df_meds_analysis %>%
      filter(med_id == med, years_before > lim[1], years_before <= lim[2])
    if (!CUT_MEDIAN) {
      df_pop[[new_col]] <- df_pop$cohort_id %in% df_sub$cohort_id
      med_cols <- c(med_cols, new_col)
    } else {
      df_med_summary <- df_sub %>% count(cohort_id, name = "count")
      median_count <- as.integer(median(df_med_summary$count))
      gt_col <- sprintf("%s_gt_median", new_col)
      le_col <- sprintf("%s_le_median", new_col)
      df_pop[[gt_col]] <- df_pop$cohort_id %in% df_med_summary$cohort_id[df_med_summary$count >  median_count]
      df_pop[[le_col]] <- df_pop$cohort_id %in% df_med_summary$cohort_id[df_med_summary$count <= median_count]
      med_cols <- c(med_cols, gt_col, le_col)
    }
  }
}

# =========================
# 5) Model specifications
# =========================
DIAGS_MODEL <- c("PUD","ABDOMINAL.PAIN","GERD","GASTRITIS","H.PYLORI","ALCOHOLISM","CONSTIPATION")

meds_5y     <- grep("0_5y", med_cols, value = TRUE)
meds_not5y  <- grep("0_5y", grep("A02BC", med_cols, value = TRUE), invert = TRUE, value = TRUE)
old_meds    <- grep("0[.][15]y|0[.][15]_", meds_not5y, value = TRUE, invert = TRUE)  # safer regex

FORMULAS <- list(
  base        = stats::as.formula(paste0(COND_CASE, paste(COFACTORS, collapse = "+"))),
  meds_5y     = stats::as.formula(paste0(COND_CASE, paste(c(COFACTORS, meds_5y), collapse = "+"))),
  diags       = stats::as.formula(paste0(COND_CASE, paste(c(COFACTORS, DIAGS_MODEL), collapse = "+"))),
  meds_not5y  = stats::as.formula(paste0(COND_CASE, paste(c(COFACTORS, meds_not5y), collapse = "+"))),
  meds_diags  = stats::as.formula(paste0(COND_CASE, paste(c(COFACTORS, med_cols, DIAGS_MODEL), collapse = "+"))),
  meds_diags_old = stats::as.formula(paste0(COND_CASE, paste(c(COFACTORS, old_meds, DIAGS_MODEL), collapse = "+")))
)

# Subgroup selection (ensure variables exist)
df_data <- dplyr::case_when(
  SUBGROUP == "all"      ~ TRUE,
  SUBGROUP == "stomach"  ~ if ("is_stomach" %in% names(df_pop)) df_pop$is_stomach > 0 else TRUE,
  SUBGROUP == "BMIgt27"  ~ if ("BMI" %in% names(df_pop)) df_pop$BMI >= 27 else TRUE,
  TRUE ~ TRUE
)
df_data <- df_pop[which(df_data), , drop = FALSE]

cat(sprintf("dataframe size: %d\n", nrow(df_data)))
print(table(df_data$group, useNA = "ifany"))

# =========================
# 6) Model runner and reporting
# =========================

stars_p_vals <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  ""
}

# Generic model formatter: exponentiate coefficients and add CI
format_exp_model <- function(model, coef_name = "Estimate", title = "OR") {
  sm <- broom::tidy(model, conf.int = TRUE, exponentiate = TRUE)
  names(sm)[names(sm) == "estimate"] <- title
  names(sm)[names(sm) == "term"]     <- "term"
  names(sm)[names(sm) == "conf.low"] <- "CI_L"
  names(sm)[names(sm) == "conf.high"]<- "CI_U"
  names(sm)[names(sm) == "p.value"]  <- "p"
  sm$stars <- vapply(sm$p, stars_p_vals, character(1))
  sm
}

wb <- openxlsx::createWorkbook()

for (model_name in names(FORMULAS)) {
  cat("\n------------------------- ", model_name, " -------------------------\n")
  formula_glm <- FORMULAS[[model_name]]

  # Logistic regression
  glm_model <- glm(formula_glm, data = df_data, family = binomial())
  print(summary(glm_model))
  model_glm <- format_exp_model(glm_model, coef_name = "Estimate", title = "OR")
  # Try to parse med_id and timing from term (robust to name length)
  model_glm$med_id <- stringr::str_extract(model_glm$term, "^[A-Z0-9]{5,7}_[A-Z0-9]+")
  model_glm$timing <- stringr::str_extract(model_glm$term, "[0-9.]+_[0-9.]+y")
  model_glm <- dplyr::left_join(model_glm, med_names_by_med_id, by = "med_id")
  model_glm$med_name <- dplyr::coalesce(model_glm$med_name, model_glm$term)

  openxlsx::addWorksheet(wb, model_name)
  openxlsx::writeData(wb, model_name, model_glm)

	# Conditional logistic regression (matched analysis)
	if ("strata_id" %in% names(df_data)) {
	  cat("\n--------------------- ", model_name, " (conditional) ---------------------\n")
	  
	  # Start from the glm formula (e.g., CA ~ predictors) and add strata(strata_id)
	  formula_logit <- update(formula(glm_model), . ~ . + strata(strata_id))
	  
	  # Fit conditional logistic regression
	  model <- survival::clogit(formula_logit, data = df_data)
	  print(summary(model))
	  
	  # Format like before (exponentiated ORs, CIs, p-values)
	  model_print <- format_exp_model(model, coef_name = "coef", title = "OR")
	  model_print$med_id <- stringr::str_extract(model_print$term, "^[A-Z0-9]{5,7}_[A-Z0-9]+")
	  model_print$timing <- stringr::str_extract(model_print$term, "[0-9.]+_[0-9.]+y")
	  model_print <- dplyr::left_join(model_print, med_names_by_med_id, by = "med_id")
	  model_print$med_name <- dplyr::coalesce(model_print$med_name, model_print$term)
	  
	  sh <- paste0(model_name, "_c")
	  openxlsx::addWorksheet(wb, sh)
	  openxlsx::writeData(wb, sh, model_print)
	} else {
	  message("Skipping conditional model for ", model_name, " (no strata_id column found).")
	}
}

# =========================
# 7) Fisher tests (2x2) for meds and diags
# =========================

perform_fisher_test <- function(df, col) {
  tab <- table(df[[col]], df[[GROUP_CASE]])
  if (all(dim(tab) == c(2, 2))) {
    ft <- fisher.test(tab)
    data.frame(
      variable    = col,
      case_Count  = tab[2, 2],
      ctrl_Count  = tab[2, 1],
      case_Percent= (tab[2, 2] / colSums(tab)[2]) * 100,
      ctrl_Percent= (tab[2, 1] / colSums(tab)[1]) * 100,
      P_Value     = ft$p.value,
      Odds_Ratio  = unname(ft$estimate),
      CI_L        = unname(ft$conf.int[1]),
      CI_U        = unname(ft$conf.int[2]),
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }
}

df_results_meds <- bind_rows(lapply(med_cols, perform_fisher_test, df = df_pop))
if (nrow(df_results_meds)) {
  df_results_meds$med_id <- stringr::str_extract(df_results_meds$variable, "^[A-Z0-9]{5,7}_[A-Z0-9]+")
  df_results_meds <- left_join(med_names_by_med_id, df_results_meds, by = "med_id")
  openxlsx::addWorksheet(wb, "meds_cnt")
  openxlsx::writeData(wb, "meds_cnt", df_results_meds)
}

df_results_diags <- bind_rows(lapply(DIAGS_MODEL, perform_fisher_test, df = df_data))
if (nrow(df_results_diags)) {
  openxlsx::addWorksheet(wb, paste0("diag_", DIAGS_YEAR_LIMIT[2], "y"))
  openxlsx::writeData(wb, paste0("diag_", DIAGS_YEAR_LIMIT[2], "y"), df_results_diags)
}

# ICD codes sheet (if you have those CSVs)
to_keep <- make.names(df_diags_categorization$cat_id) %in% DIAGS_MODEL
if (any(to_keep)) {
  openxlsx::addWorksheet(wb, "diag_codes")
  openxlsx::writeData(wb, "diag_codes", df_diags_categorization[to_keep, ])
}

openxlsx::saveWorkbook(wb, OUT_XLSX, overwrite = TRUE)
message("Analysis complete. Wrote: ", OUT_XLSX)

# Recommended to append a session info file for reproducibility
writeLines(capture.output(sessionInfo()), con = file.path(OUT_DIR, "sessionInfo.txt"))
