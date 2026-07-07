# ============================================================
# 03_build_embeddings_cache.R
# RUN ONCE ONLY (then again whenever the SNOMED release changes).
# Encodes all relevant SNOMED descriptions (FSN + synonyms, filtered
# categories) and saves the embeddings matrix to disk. The per-symptom
# scripts (04_match_embeddings_per_symptom.R) reuse this cache and
# never re-encode the corpus.
# ============================================================
# OPEN QUESTION: should the embeddings be computed on Aurum or on SNOMED?

library(Rdiagnosislist)
library(dplyr)
library(stringr)
library(reticulate)
library(yaml)

params = yaml::read_yaml("config/params.yml")

message("3. Compute embeddings for the SNOMED corpus - WARNING, can be long.")

# Create a virtual environment (once only)
#virtualenv_create("st-env")
#virtualenv_install("st-env", packages = c("sentence-transformers","numpy"))

use_virtualenv(params$embeddings$python_env, required = TRUE)
st = import("sentence_transformers")
embedder = st$SentenceTransformer(params$embeddings$model)

SNOMED = loadSNOMED(params$paths$snomed_releases)

# ---- Build the corpus of descriptions to encode ----
data.desc = SNOMED$DESCRIPTION
data.desc$conceptId = as.character(data.desc$conceptId)

if (isTRUE(params$embeddings$embed_all_descriptions)) {
  # FSN + all active synonyms (covers the TODO left in the original script:
  # a synonym can match better than the FSN)
  data.concepts = data.desc %>% filter(active == 1)
} else {
  data.concepts = data.desc %>% filter(typeId == "900000000000003001")  # FSN only
}

data.concepts = data.concepts %>%
  mutate(
    snomed_cat  = str_extract(term, "\\(([^)]+)\\)\\s*$", group = 1),
    snomed_term = str_trim(str_remove(term, "\\s*\\([^)]+\\)\\s*$"))
  )

# ---- Filter out irrelevant categories to speed up computation ----
# file to build once manually: list of SNOMED semantic tags with an
# include column (0/1), per discussion "most frequent categories we're
# confident won't contain any symptom" (medicinal product, organism,
# administrative procedure, etc.)
snomed_categories = read.csv(params$paths$snomed_category_include)
cat_to_include = snomed_categories$snomed_cat[snomed_categories$include == 1]

data.concepts.f = data.concepts %>% filter(snomed_cat %in% cat_to_include)

message(sprintf("Corpus to encode: %s descriptions (out of %s total)",
                nrow(data.concepts.f), nrow(data.desc)))

# For testing purposes
data.concepts.f = data.concepts.f[1:10000,]
data.concepts.f$conceptId = as.character(data.concepts.f$conceptId)

# ---- Encoding (to run only once) ----
corpus_emb = py_to_r(embedder$encode(
  data.concepts.f$snomed_term,
  batch_size = params$embeddings$batch_size,
  show_progress_bar = TRUE,
  normalize_embeddings = TRUE
))

dir.create(params$paths$cache_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(corpus_emb, file.path(params$paths$cache_dir, "snomed_corpus_embeddings.rds"))
saveRDS(data.concepts.f, file.path(params$paths$cache_dir, "snomed_corpus_terms.rds"))

message("Embeddings cache saved to: ", params$paths$cache_dir)
