# Upper GI Models

Code to reproduce the analyses of upper gastrointestinal risk factors and medication exposures described in the manuscript.

Status: research code released for transparency and reproducibility.
License: CC-BY-NC-4.0 (code and docs).
Contact: aisrael@leumit.co.il

TL;DR (Quick Start)

Rscript upper_gi_models.R
or override defaults:
DATA_DIR=/path/to/data OUT_DIR=out SUBGROUP=stomach CUT_MEDIAN=FALSE Rscript upper_gi_models.R


Outputs land in out/ (default):

models_<SUBGROUP>.xlsx — model summaries (logistic and, if available, conditional logistic).

sessionInfo.txt — package versions for reproducibility.

# What this script does

Builds a cohort from baseline data and look-back windows.

Derives BMI and BMI categories, counts diagnosis visits, and flags medication exposures across predefined time windows.

Fits logistic regression models (and conditional logistic if a strata_id column exists).

Writes clean, exponentiated coefficients (ORs) with 95% CIs to Excel for downstream tables/figures.

Runs Fisher’s exact tests for simple 2×2 comparisons (medications, diagnoses).

# Configuration

You can control behavior using environment variables:

Var	Default	Allowed	Meaning
DATA_DIR	data	any path	Directory containing input files
OUT_DIR	out	any path	Where outputs are written
SUBGROUP	stomach	stomach | all | BMIgt27	Row filter
CUT_MEDIAN	FALSE	TRUE | FALSE	If TRUE, split exposure by median count

# Example:

DATA_DIR=/mnt/ehr_exports OUT_DIR=./out SUBGROUP=all CUT_MEDIAN=TRUE Rscript upper_gi_models.R

Input data & schema

Place files under DATA_DIR (default: ./data). File types accepted: Parquet (.parquet) or Stata (.dta). Names below are expected relative paths:

comparison_for_r.dta
dump/measure.dta
dump/diags.dta
dump/med.dta
medication.csv               # catalog
icd_categories.csv
acute_icd_categories.csv


Minimum columns (expected names):

comparison_for_r.dta (baseline)

cohort_id (unique id)

group (case/control indicator string; “CA” denotes case)

index_date (Date)

Covariates: age, genderF, SmokingNm, SES_category, HealthWorker, HadBeenPregnant, (optional) is_stomach, BMI

dump/measure.dta (labs/measures)

cohort_id, MeasureCd (BMI = 15), MeasureDt (Date), MeasureValue1 (numeric)

dump/diags.dta (diagnoses)

cohort_id, ICDCd, VisitDt (Date)

dump/med.dta (medications)

cohort_id, MedicationKey, IssueDt (Date)

medication.csv (catalog)

MedicationKey, ATC7Cd, ATC7Desc, ATC3Cd, MedicationNm, GivingFormCd1

icd_categories.csv, acute_icd_categories.csv

Must include: ICDCd, cat_id (category label used for flags)

Protected data: Do not include PHI or raw EHR in the public repo. Provide synthetic or minimally de-identified examples if needed. Reviewers can still run the pipeline on their side with similarly structured files.

# Outputs

Excel workbook: out/models_<SUBGROUP>.xlsx
Sheets include model summaries (logistic; _c for conditional), meds counts (meds_cnt), diagnosis tests (diag_<years>y), and diagnosis code mappings (diag_codes).

Log: out/sessionInfo.txt (R and package versions).

Reproducibility notes

Script installs and loads: survival, broom, dplyr, forcats, data.table, readr, openxlsx, stringr, tidyr, rlang, haven, arrow.

For fully pinned environments, consider adding renv::init() and committing renv.lock, or supply a simple Dockerfile.

Troubleshooting

“Missing data file”: confirm DATA_DIR and paths (see schema).

Date parsing warnings: files should store index_date, VisitDt, IssueDt, MeasureDt as real dates (or ISO8601 strings).

Conditional model skipped: requires a strata_id column (matched set id). Without it, only logistic models run.

Java errors: not applicable; we use openxlsx (no Java dependency).

# How to cite

If you reuse this code, please cite:

Israel A. (2025). upper_gi_models: Analysis code for Upper GI risk factors. GitHub repository: https://github.com/arielisr/upper_gi_models

BibTeX

@misc{israel_upper_gi_models_2025,
  author       = {Israel, Ariel},
  title        = {upper\_gi\_models: Analysis code for Upper GI risk factors},
  year         = {2025},
  howpublished = {\url{https://github.com/arielisr/upper_gi_models}},
  license      = {CC-BY-NC-4.0}
}


For journal archiving, consider creating a Zenodo DOI and adding the badge here.

# Ethical & AI disclosure

During manuscript preparation, we used a large-language-model assistant (ChatGPT) for code refactoring, commenting, and README editing only.
All analyses, results, and interpretations are the authors’ responsibility; the model did not perform or alter scientific analyses.
# License

This repository is released under CC-BY-NC-4.0. You may reuse and adapt with attribution for non-commercial purposes. For commercial use, please contact the author.

# Contact

Questions or issues: open a GitHub Issue or email aisrael@leumit.co.il
