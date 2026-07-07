# ============================================================
# 04_match_embeddings_per_symptom.R
# Reuses the corpus cache (03_build_embeddings_cache.R): encodes only
# the symptom's few "references" (fast), computes cosine similarity,
# aggregates at conceptId level (max across all descriptions), and
# removes concepts already found by regex+hierarchy
# (01_match_snomed.R) to keep only the "potentially missed codes".
# ============================================================

library(dplyr)
library(stringr)
library(reticulate)
library(yaml)
library(ggplot2)
source("src/utils_config.R")

params = yaml::read_yaml("config/params.yml")

use_virtualenv(params$embeddings$python_env, required = TRUE)
st = import("sentence_transformers")
embedder = st$SentenceTransformer(params$embeddings$model)

corpus_emb   = readRDS(file.path(params$paths$cache_dir, "snomed_corpus_embeddings.rds"))
corpus_terms = readRDS(file.path(params$paths$cache_dir, "snomed_corpus_terms.rds"))

#' references: vector of reference phrases for the symptom
#' (documented in Symptom-config, embedding_references column)
compute_symptom_similarity = function(references, corpus_emb, corpus_terms, batch_size = 128L) {
  ref_emb = py_to_r(embedder$encode(
    references, batch_size = batch_size,
    show_progress_bar = FALSE, normalize_embeddings = TRUE
  ))

  sim_matrix = corpus_emb %*% t(ref_emb)
  colnames(sim_matrix) = references

  best_idx = max.col(sim_matrix, ties.method = "first")
  best_score = sim_matrix[cbind(seq_len(nrow(sim_matrix)), best_idx)]

  out = corpus_terms %>%
    mutate(
      best_reference  = references[best_idx],
      max_similarity  = best_score
    ) %>%
    # aggregate by conceptId: one description may match better than another
    # for the same concept (FSN vs synonym) -> keep the best score
    group_by(conceptId) %>%
    slice_max(max_similarity, n = 1, with_ties = FALSE) %>%
    ungroup()

  return(out)
}

#' For a given symptom: computes similarity, applies the threshold, and
#' removes concepts already present in the regex+hierarchy codelist
find_potentially_missed_codes = function(symptom, sim, params, threshold = NULL) {
  if (is.null(threshold)) threshold = params$embeddings$default_similarity_threshold

  snomed_path = file.path(params$paths$cache_dir,"snomed")
  snomed_file = file.path(snomed_path, paste0("snomed_", make.names(symptom), ".rds"))
  already_found = if (file.exists(snomed_file)) readRDS(snomed_file)$conceptId else integer(0)

  missed = sim %>%
    filter(max_similarity >= threshold, !(conceptId %in% already_found)) %>%
    arrange(desc(max_similarity)) %>%
    mutate(symptom = symptom, source = "embeddings_gemma_medical")

  return(missed)
}

# ---- Threshold calibration diagnostic (do at least once per symptom) ----
# Reprises the density analysis from compute_embedding_similarity_snomed.R:
# compares the similarity distribution for concepts already in the codelist
# vs the rest, to choose/validate the threshold rather than blindly using
# default_similarity_threshold.
plot_similarity_calibration = function(symptom, sim, params) {
  snomed_path = file.path(params$paths$cache_dir,"snomed")
  snomed_file = file.path(snomed_path, paste0("snomed_", make.names(symptom), ".rds"))
  already_found = if (file.exists(snomed_file)) readRDS(snomed_file)$conceptId else integer(0)
  sim$already_in_codelist <- sim$conceptId %in% already_found

  ggplot(sim, aes(x = max_similarity, group = already_in_codelist, fill = already_in_codelist)) +
    geom_density(adjust = 1.5, alpha = .4) +
    labs(title = paste("Similarity threshold calibration -", symptom))
}

if (sys.nframe() == 0) {
  cfg = read_symptom_config(params$paths$input_excel)
  cfg = cfg[cfg$symptom %in% c("Hemiparesis", "Bone pain"),] # For testing purposes

  # Create saving folders
  missed_dir = file.path(params$paths$cache_dir, "missed")
  dir.create(missed_dir, showWarnings = FALSE, recursive = TRUE)

  dir.create(params$paths$output_dir, showWarnings = FALSE, recursive = TRUE)
  plot_dir = file.path(params$paths$output_dir, "calibration_plots")
  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

  for (i in seq_len(nrow(cfg))) {
    row = cfg[i, ]
    refs = str_split(row$embedding_references, ",")[[1]] %>% str_trim()
    refs = refs[refs != ""]
    if (length(refs) == 0) {
      warning(sprintf("No embedding_references provided for '%s' - step skipped.", row$symptom))
      next
    }
    message(sprintf("[Embeddings] %s", row$symptom))
    sim = compute_symptom_similarity(refs, corpus_emb, corpus_terms)
    missed = find_potentially_missed_codes(row$symptom, sim, params)
    saveRDS(missed, file.path(missed_dir, paste0("potentially_missed_", make.names(row$symptom), ".rds")))

    message(sprintf("[Plot embedding calibration] %s", row$symptom))
    p = plot_similarity_calibration(row$symptom, sim, params)
    plot_path = 
    ggplot2::ggsave(
        filename = file.path(plot_dir, paste0("calibration_", make.names(row$symptom), ".png")),
        plot = p, width = 6, height = 4, dpi = 150
      )
  }
}
