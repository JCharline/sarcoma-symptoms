# ============================================================
# 02_match_aurum.R
# Regex + hierarchy matching on the Aurum (CPRD) medical dictionary's
# OWN terms, independently of SNOMED, followed by reconciliation:
#   - Aurum matches with a valid SnomedCTConceptId -> compared against
#     the SNOMED codelist for the same symptom (already found / missed)
#   - Aurum matches WITHOUT a SNOMED mapping -> "Aurum-only"
#
# NB: adapt the column names below to the actual format of your Aurum
# dictionary extract (typically: Term, MedCodeId, SnomedCTConceptId,
# Release, EmisCodeCategoryID...).
# ============================================================

library(dplyr)
library(stringr)
library(yaml)
source("R/utils_config.R")

params = yaml::read_yaml("config/params.yml")

load_aurum_dictionary = function(path) {
  df = read.csv(path, sep="\t", stringsAsFactors = FALSE, colClasses=c("SnomedCTConceptId"="character"))
  stopifnot(all(c("Term", "SnomedCTConceptId") %in% names(df)))  # adapt if column names differ
  return(df)
}

#' Matches one config component against the Term field of the Aurum dictionary
match_component_aurum = function(excel_path, spec, aurum_dict) {
  terms = extract_terms(excel_path, spec)
  if (length(terms) == 0) return(NULL)

  regex = build_regex(terms)
  hits = aurum_dict[str_detect(aurum_dict$Term, regex(regex, ignore_case = TRUE)), ]
  if (nrow(hits) == 0) return(NULL)
  return(hits)
}

build_symptom_codelist_aurum = function(row, excel_path, aurum_dict) {

  hits_A = match_component_aurum(excel_path, row$source_a_sheet_filter, aurum_dict)
  hits_B = match_component_aurum(excel_path, row$source_b_sheet_filter, aurum_dict)

  if (row$mode == "single") {
    final = hits_B
  } else if (row$mode == "intersect") {
    if (is.null(hits_A) || is.null(hits_B)) return(NULL)
    # Intersect at the SnomedCTConceptId level (consistent with the SNOMED
    # approach), NOT at the MedCodeId level, to stay comparable with the
    # SNOMED codelist for the symptom.
    shared_ids = intersect(hits_A$SnomedCTConceptId, hits_B$SnomedCTConceptId)
    final = hits_B[hits_B$SnomedCTConceptId %in% shared_ids, ]
  } else {
    stop(sprintf("Unknown mode: %s", row$mode))
  }

  if (is.null(final) || nrow(final) == 0) return(NULL)
  final$symptom = row$symptom
  final$source = "Aurum"
  return(final)
}

#' Reconciles the Aurum codelist with the SNOMED codelist for the same symptom
#' (requires 01_match_snomed.R to have already run and cached its .rds)
reconcile_aurum_snomed = function(symptom, aurum_result, params) {
  snomed_file = file.path(params$paths$cache_dir, paste0("snomed_", make.names(symptom), ".rds"))
  snomed_ids = if (file.exists(snomed_file)) readRDS(snomed_file)$conceptId else integer(0)

  aurum_result %>%
    mutate(
      has_snomed_mapping = !is.na(SnomedCTConceptId),
      already_in_snomed_codelist = SnomedCTConceptId %in% snomed_ids,
      match_status = case_when(
        !has_snomed_mapping ~ "aurum_only_no_snomed_mapping",
        already_in_snomed_codelist ~ "confirmed_in_both",
        TRUE ~ "aurum_only_missed_by_snomed_regex"
      )
    )
}

run_aurum_matching_all_symptoms = function(excel_path, params) {
  aurum_dict = load_aurum_dictionary(params$paths$aurum_dictionary)
  cfg = read_symptom_config(excel_path)
  cfg = cfg[cfg$symptom %in% c("Hemiparesis", "Bone pain"),] # For testing purposes
  names(cfg)[grepl("^source_a", names(cfg))] <- "source_a_sheet_filter"
  names(cfg)[grepl("^source_b", names(cfg))] <- "source_b_sheet_filter"

  results = list()
  for (i in seq_len(nrow(cfg))) {
    row = cfg[i, ]
    message(sprintf("[Aurum] %s (mode=%s)", row$symptom, row$mode))
    res = tryCatch(
      build_symptom_codelist_aurum(row, excel_path, aurum_dict),
      error = function(e) {
        warning(sprintf("Aurum matching failed on '%s': %s", row$symptom, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(res)) {
      res = reconcile_aurum_snomed(row$symptom, res, params)
      out_file = file.path(params$paths$cache_dir, paste0("aurum_", make.names(row$symptom), ".rds"))
      saveRDS(res, out_file)
      results[[row$symptom]] = res
    }
  }
  return(results)
}

if (sys.nframe() == 0) {
  run_aurum_matching_all_symptoms(params$paths$input_excel, params)
}
