# ============================================================
# 05_build_output_excel.R
# Assembles, for each symptom, an Excel file with:
#   - "codelist regex" sheet          : SNOMED + Aurum regex/hierarchy, source documented
#   - "potentially missed codes" sheet : embeddings suggestions, with score, to validate
# ============================================================

library(dplyr)
library(openxlsx)
library(yaml)
source("src/utils_config.R")

params <- yaml::read_yaml("config/params.yml")

build_output_for_symptom <- function(symptom, params) {
  slug <- make.names(symptom)

  snomed_file <- file.path(params$paths$cache_dir, paste0("snomed_", slug, ".rds"))
  aurum_file  <- file.path(params$paths$cache_dir, paste0("aurum_",  slug, ".rds"))
  missed_file <- file.path(params$paths$cache_dir, paste0("potentially_missed_", slug, ".rds"))

  snomed_res <- if (file.exists(snomed_file)) readRDS(snomed_file) else NULL
  aurum_res  <- if (file.exists(aurum_file))  readRDS(aurum_file)  else NULL
  missed_res <- if (file.exists(missed_file)) readRDS(missed_file) else NULL

  # ---- "codelist regex" sheet: SNOMED + Aurum, source documented row by row ----
  codelist_regex = bind_rows(
    if (!is.null(snomed_res)) snomed_res %>%
      transmute(conceptId, term, symptom, source = "SNOMED", matched_via) else NULL,
    if (!is.null(aurum_res)) aurum_res %>%
      transmute(conceptId = SnomedCTConceptId, term = Term, symptom,
                source = "Aurum", matched_via = match_status) else NULL
  ) %>% distinct(conceptId, .keep_all = TRUE)

  # ---- "potentially missed codes" sheet: embeddings suggestions with score ----
  potentially_missed = if (!is.null(missed_res)) {
    missed_res %>% select(conceptId, snomed_term, best_reference, max_similarity, symptom, source)
  } else {
    tibble()
  }

  wb <- createWorkbook()
  addWorksheet(wb, "codelist regex")
  writeData(wb, "codelist regex", codelist_regex)
  addWorksheet(wb, "potentially missed codes")
  writeData(wb, "potentially missed codes", potentially_missed)

  out_path <- file.path(params$paths$output_dir, paste0(slug, "_codelist.xlsx"))
  dir.create(params$paths$output_dir, showWarnings = FALSE, recursive = TRUE)
  saveWorkbook(wb, out_path, overwrite = TRUE)
  message("Written: ", out_path)
  out_path
}

if (sys.nframe() == 0) {
  cfg = read_symptom_config(params$paths$input_excel)
  cfg = cfg[cfg$symptom %in% c("Hemiparesis", "Bone pain"),] # For testing purposes
  for (s in cfg$symptom) build_output_for_symptom(s, params)
}
