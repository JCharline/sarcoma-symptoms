# ============================================================
# 01_match_snomed.R
# Generic engine: for each row in Symptom-config, matches
# source_A / source_B components against SNOMED (UK + International)
# via Rdiagnosislist, expands descendants per component, then
# intersects at the level of the final conceptId ("intersect" mode)
# or takes the single component directly ("single" mode).
# ============================================================

library(Rdiagnosislist)
library(dplyr)
library(stringr)
library(yaml)
source("src/utils_config.R")

message("1. Launch SNOMED regex matching")

params = yaml::read_yaml("config/params.yml")

SNOMED = loadSNOMED(params$paths$snomed_releases)

#' Matches one component (source spec) against SNOMED and returns a
#' "simple" SNOMEDcodelist with the hierarchical closure (descendants included).
match_component_snomed = function(path, spec, SNOMED, params) {
  terms = extract_terms(path, spec)
  if (length(terms) == 0) return(NULL)

  regex = build_regex(terms)

  concepts = SNOMEDconcept(
    regex,
    exact_match = FALSE,
    active_only = params$matching$active_only,
    SNOMED = SNOMED
  )
  if (length(concepts) == 0) return(NULL)

  return(concepts)
}

#' Builds the final codelist for one config row (one symptom)
build_symptom_codelist_snomed = function(row, excel_path, SNOMED, params) {

  concepts_A = match_component_snomed(
    excel_path,
    row$source_a_sheet_filter,
    SNOMED,
    params
  )
  concepts_B = match_component_snomed(
    excel_path,
    row$source_b_sheet_filter,
    SNOMED,
    params
  )

  if (row$mode == "single") {
    result = concepts_B
    matched_via = "single_component"
  } else if (row$mode == "intersect") {
    if (is.null(concepts_A) || is.null(concepts_B)) {
      warning(sprintf(
        "Symptom '%s': one of the two components matched no concept.",
        row$symptom
      ))
      return(NULL)
    }
    result = intersect(concepts_A, concepts_B)
    if (length(result) == 0) return(NULL)
    matched_via = "intersect_A_B"
  } else {
    stop(sprintf("Unknown mode in config: '%s'", row$mode))
  }
  res_codelist = SNOMEDcodelist(
    result,
    include_desc = params$matching$include_desc,
    format = "simple",
    SNOMED = SNOMED
  )

  # Filter out irrelevant categories to shrink codelist size
  res_codelist = res_codelist %>%
    mutate(
      snomed_cat  = str_extract(term, "\\(([^)]+)\\)\\s*$", group = 1),
      snomed_term = str_trim(str_remove(term, "\\s*\\([^)]+\\)\\s*$"))
    )
  # file to build once manually: list of SNOMED semantic tags with an
  # include column (0/1)
  snomed_categories = read.csv(params$paths$snomed_category_include)
  cat_to_include = snomed_categories$snomed_cat[snomed_categories$include == 1]

  final = res_codelist %>% filter(snomed_cat %in% cat_to_include)

  message(sprintf(
    "Final codelist after category filtering: %s codes (out of %s total)",
    nrow(final), nrow(res_codelist)
  ))


  final$conceptId = as.character(res_codelist$conceptId)
  final$symptom = row$symptom
  final$matched_via = matched_via
  final$source = "SNOMED"
  return(final)
}

#' Loops over the whole config and exports one intermediate .rds per symptom
run_snomed_matching_all_symptoms = function(
  excel_path,
  config_path = excel_path,
  params) {

  cfg = read_symptom_config(config_path)
  # normalise column names coming from the header 'source_A (sheet, filter)' etc.
  names(cfg)[grepl("^source_a", names(cfg))] = "source_a_sheet_filter"
  names(cfg)[grepl("^source_b", names(cfg))] = "source_b_sheet_filter"

  save_path = file.path(params$paths$cache_dir,"snomed")

  results = list()
  for (i in seq_len(nrow(cfg))) {
    row = cfg[i, ]
    message(sprintf("[SNOMED] %s (mode=%s)", row$symptom, row$mode))
    res = tryCatch(
      build_symptom_codelist_snomed(row, excel_path, SNOMED, params),
      error = function(e) {
        warning(sprintf("Failed on '%s': %s", row$symptom, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(res)) {
      out_file = file.path(save_path, paste0("snomed_", make.names(row$symptom), ".rds"))
      saveRDS(res, out_file)
      results[[row$symptom]] = res
    }
  }
  return(results)
}

save_path = file.path(params$paths$cache_dir,"snomed")
dir.create(save_path, showWarnings = FALSE, recursive = TRUE)
run_snomed_matching_all_symptoms(params$paths$input_excel, params = params)
