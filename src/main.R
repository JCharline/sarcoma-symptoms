# ============================================================
# main.R - full pipeline orchestration
# ============================================================
# Prerequisites before running:
#   1) config/params.yml filled in (SNOMED / Aurum paths, versions)
#   2) cache/snomed_categories.csv built (category filter for embeddings)
#   3) Symptom-config filled in, including the embedding_references column
#   4) 03_build_embeddings_cache.R run at least once (expensive, don't repeat)
# ============================================================

setwd(here::here())  # adapt, or run from the project root

source("src/01_match_snomed.R")   # -> {cache_dir}/snomed_<symptom>.rds
source("src/02_match_aurum.R")    # -> {cache_dir}/aurum_<symptom>.rds
# source("src/03_build_embeddings_cache.R")  # uncomment for the first run only
source("src/04_match_embeddings_per_symptom.R")  # -> {cache_dir}/missed_<symptom>.rds
source("src/05_build_output_excel.R")  # -> {output_dir}/<symptom>_codelist.xlsx
