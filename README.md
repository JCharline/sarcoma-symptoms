# Symptom codelists — SNOMED-CT & Aurum Medical Dictionary

A 2-step pipeline to build symptom codelists:
1. Loosened exact match + hierarchical closure (`Rdiagnosislist`), applied separately to
   SNOMED (UK + International) and to the Aurum medical dictionary.
2. Semantic embeddings (Gemma medical) to spot codes potentially missed by step 1.

## Structure

```
config/
  params.yml            # paths, versions, thresholds, embedding model parameters
data/
  <symptoms>.xlsx        # source file (Symptom-config sheet + term sheets)
src/
  utils_config.R          # Symptom-config parsing + generic term extraction
  01_match_snomed.R        # generic SNOMED engine (single / intersect)
  02_match_aurum.R         # same on the Aurum dictionary, + reconciliation with SNOMED
  03_build_embeddings_cache.R      # ONE-TIME encoding of the SNOMED corpus (only
                                    # re-run when the release changes)
  04_match_embeddings_per_symptom.R # reuses the cache, computes similarity per symptom
  05_build_output_excel.R  # assembles the output Excel file per symptom
  main.R                    # full orchestration
cache/                    # intermediate .rds files (per-symptom results + embeddings cache)
output/                   # <symptom>_codelist.xlsx (final deliverable, one file per symptom)
```

## Generic engine logic (`Symptom-config`)

Each row describes a symptom via:
- `source_A (sheet, filter)` and `source_B (sheet, filter)`: components to combine,
  in the format `"Sheet name (R filter or 'all')"`, e.g. `Lump-Swelling-Pain (cat=="pain")`.
  `-` = no component.
- `mode`: `single` (one component only, = source_B) or `intersect` (both components are
  matched **separately with descendant expansion**, then intersected **at the level of
  the final conceptId** — not before expansion).
- `embedding_references` *(column to fill in)*: short reference phrases used for
  semantic similarity in step 2, comma-separated.


## To complete before the first run

- [x] Fix the `terms` → `term` column in the `Joint terms` sheet (or leave the
      defensive automatic rename, with warning).
- [ ] Adapt the Aurum dictionary column names in `02_match_aurum.R`
      (`Term`, `SnomedCTConceptId` assumed — check against your actual extract).
- [ ] Build `cache/snomed_categories.csv` (semantic tags to include/exclude).
- [ ] Fill in the `embedding_references` column in `Symptom-config`.
- [ ] Check the two "Growing lump" rows from the original message (different
      definitions) and confirm the correct version in `Symptom-config`.
- [ ] Decide and document `default_similarity_threshold` per symptom if the
      distribution varies too much from one symptom to another.
