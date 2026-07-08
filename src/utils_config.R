# ============================================================
# utils_config.R
# Reads the "Symptom-config" sheet and generically extracts the
# candidate terms for each component (source_A / source_B)
# ============================================================

library(readxl)
library(dplyr)
library(stringr)
library(rlang)

#' Reads a sheet from the symptoms file, normalising known column-name
#' typos (e.g. "terms" instead of "term" in Joint terms)
read_symptom_sheet = function(path, sheet) {
  df = suppressMessages(read_excel(path, sheet = sheet))
  names(df) = str_trim(names(df))
  return(df)
}

#' Parses a source spec string such as:
#'   "Bone terms (all)"
#'   "Lump-Swelling-Pain (cat==\"pain\")"
#'   "-"   -> no component (returns NULL)
parse_source_spec = function(spec) {
  spec = str_trim(spec)
  if (is.na(spec) || spec == "-" || spec == "") return(NULL)

  m = str_match(spec, "^(.*)\\s\\(([^)]*)\\)\\s*$")
  if (is.na(m[1, 1])) {
    stop(sprintf("Could not parse source spec: '%s'", spec))
  }
  list(sheet = str_trim(m[1, 2]), filter = str_trim(m[1, 3]))
}

#' Extracts the pool of candidate terms (character strings) for a given source spec.
#' Generic logic, aligned with what was done manually in snomed_extraction_bone_pain.R:
#'   - take the most granular category column present (cat_2, else cat)
#'   - split the "term" column on commas (synonyms)
#'   - exclude cross-reference placeholder rows such as "cf 'Generic terms'"
extract_terms = function(path, spec) {
  parsed = parse_source_spec(spec)
  if (is.null(parsed)) return(character(0))

  df = read_symptom_sheet(path, parsed$sheet)

  if (parsed$filter != "all" && parsed$filter != "") {
    df = dplyr::filter(df, !!rlang::parse_expr(parsed$filter))
  }

  cat_cols = intersect(c("cat_2", "cat"), names(df))
  cat_terms = if (length(cat_cols) > 0) df[[cat_cols[length(cat_cols)]]] else character(0)

  free_terms = if ("term" %in% names(df)) unlist(str_split(df$term, ",")) else character(0)

  all_terms = c(cat_terms, free_terms)
  all_terms = str_trim(all_terms)
  all_terms = all_terms[!is.na(all_terms) & all_terms != ""]

  # Exclude cross-reference placeholders (e.g. "cf 'Generic terms'")
  all_terms = all_terms[!str_detect(str_to_lower(all_terms), "^cf[ .']")]

  return(unique(all_terms))
}

#' Escapes regex special characters before building the OR pattern
escape_regex = function(terms) {
  str_replace_all(terms, "([\\\\\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}])", "\\\\\\1")
}

#' Builds the OR regex pattern from a pool of terms ("loosened" exact match:
#' no word boundary enforced, per discussion - some terms are deliberately
#' stems/suffixes)
build_regex = function(terms) {
  terms = terms[!is.na(terms) & terms != ""]
  if (length(terms) == 0) return(NA_character_)
  res = paste0("(?i)", paste(unique(escape_regex(terms)), collapse = "|"))
  return(res)
}

#' Reads the config sheet and returns a clean data.frame
read_symptom_config = function(path, sheet = "Symptom-config") {
  cfg = read_symptom_sheet(path, sheet)
  cfg = cfg %>% filter(!is.na(symptom))
  names(cfg) = names(cfg) %>%
    str_replace_all("[^A-Za-z0-9_]", "_") %>%
    str_to_lower()
  return(cfg)
}
