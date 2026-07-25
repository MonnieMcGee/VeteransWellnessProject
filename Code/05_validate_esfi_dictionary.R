###############################################################
## Project: Dallas Veteran Wellbeing Analysis
## Script: 05_validate_esfi_dictionary.R
## Purpose: Validate the document-level behavior and known-groups
##          validity of the frozen Experienced System
##          Fragmentation Index (ESFI).
## July 2026
###############################################################

source("Code/00_project_setup.R")

##-------------------------------------------------------------
## 0. File locations
##-------------------------------------------------------------

dfm_file <- file.path(
  deriveddata_dir,
  "reddit_dfm_analysis_with_esfi.rds"
)

dictionary_file <- file.path(
  deriveddata_dir,
  "esfi_dictionary.rds"
)

corpus_file <- file.path(
  deriveddata_dir,
  "reddit_corpus_analysis_corpus.rds"
)

validation_output_dir <- file.path(
  output_dir,
  "esfi_validation"
)

dir.create(
  validation_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

required_inputs <- c(
  dfm_file,
  dictionary_file,
  corpus_file
)

if (!all(file.exists(required_inputs))) {
  missing_inputs <- required_inputs[!file.exists(required_inputs)]

  stop(
    paste0(
      "Required input file(s) not found:\n",
      paste(missing_inputs, collapse = "\n")
    ),
    call. = FALSE
  )
}

##-------------------------------------------------------------
## 1. Import and verify frozen objects
##-------------------------------------------------------------

dfm_vet_analysis_with_esfi <- readRDS(dfm_file)
esfi_dictionary <- readRDS(dictionary_file)
corp_vet_analysis <- readRDS(corpus_file)

stopifnot(
  quanteda::is.dfm(dfm_vet_analysis_with_esfi),
  is.character(esfi_dictionary),
  inherits(corp_vet_analysis, "corpus")
)

required_docvars <- c(
  "source",
  "year",
  "esfi_hits",
  "esfi_present"
)

missing_docvars <- setdiff(
  required_docvars,
  names(quanteda::docvars(dfm_vet_analysis_with_esfi))
)

if (length(missing_docvars) > 0L) {
  stop(
    paste0(
      "The scored DFM is missing required document variable(s): ",
      paste(missing_docvars, collapse = ", ")
    ),
    call. = FALSE
  )
}

stopifnot(
  quanteda::ndoc(dfm_vet_analysis_with_esfi) == 3066L,
  quanteda::nfeat(dfm_vet_analysis_with_esfi) == 1681L,
  identical(
    quanteda::docnames(corp_vet_analysis),
    quanteda::docnames(dfm_vet_analysis_with_esfi)
  )
)

source_counts <- table(
  quanteda::docvars(
    dfm_vet_analysis_with_esfi,
    "source"
  )
)

stopifnot(
  unname(source_counts["Dallas"]) == 857L,
  unname(source_counts["Finance"]) == 146L,
  unname(source_counts["Veteran"]) == 2063L
)

##-------------------------------------------------------------
## 2. Independently recalculate and verify ESFI scores
##-------------------------------------------------------------

dfm_esfi_recalculated <- quanteda::dfm_select(
  dfm_vet_analysis_with_esfi,
  pattern = esfi_dictionary,
  valuetype = "glob",
  selection = "keep"
)

recalculated_esfi_hits <- as.numeric(
  quanteda::rowSums(dfm_esfi_recalculated)
)

recalculated_esfi_present <- recalculated_esfi_hits > 0

stored_esfi_hits <- as.numeric(
  quanteda::docvars(
    dfm_vet_analysis_with_esfi,
    "esfi_hits"
  )
)

stored_esfi_present <- as.logical(
  quanteda::docvars(
    dfm_vet_analysis_with_esfi,
    "esfi_present"
  )
)

stopifnot(
  identical(recalculated_esfi_hits, stored_esfi_hits),
  identical(recalculated_esfi_present, stored_esfi_present),
  sum(recalculated_esfi_hits) == 428L,
  sum(recalculated_esfi_present) == 268L,
  length(quanteda::featnames(dfm_esfi_recalculated)) == 11L
)

message("Stored ESFI scores exactly match independent recalculation.")

##-------------------------------------------------------------
## 3. Construct document-level validation data
##-------------------------------------------------------------

document_length <- as.numeric(
  quanteda::ntoken(dfm_vet_analysis_with_esfi)
)

esfi_document_scores <- quanteda::docvars(
  dfm_vet_analysis_with_esfi
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    document_name = quanteda::docnames(
      dfm_vet_analysis_with_esfi
    ),
    document_length = document_length,
    esfi_hits = stored_esfi_hits,
    esfi_present = stored_esfi_present,
    esfi_rate_100 = dplyr::if_else(
      document_length > 0,
      100 * esfi_hits / document_length,
      NA_real_
    ),
    .before = 1
  ) |>
  dplyr::mutate(
    source = factor(
      source,
      levels = c("Dallas", "Finance", "Veteran")
    ),
    year = as.integer(year)
  )

stopifnot(
  nrow(esfi_document_scores) ==
    quanteda::ndoc(dfm_vet_analysis_with_esfi),
  !anyNA(esfi_document_scores$esfi_hits),
  identical(
    esfi_document_scores$document_name,
    quanteda::docnames(dfm_vet_analysis_with_esfi)
  )
)

esfi_model_data <- esfi_document_scores |>
  dplyr::filter(document_length > 0)

message(
  "Documents used in length-adjusted models: ",
  nrow(esfi_model_data)
)

message(
  "Zero-feature documents excluded from length-adjusted models: ",
  sum(esfi_document_scores$document_length == 0)
)

##-------------------------------------------------------------
## 4. Overall ESFI prevalence and distribution
##-------------------------------------------------------------

esfi_overall_summary <- esfi_document_scores |>
  dplyr::summarise(
    documents = dplyr::n(),
    documents_with_esfi = sum(esfi_present),
    percent_with_esfi = 100 * mean(esfi_present),
    total_hits = sum(esfi_hits),
    mean_hits = mean(esfi_hits),
    median_hits = median(esfi_hits),
    mean_rate_100 = mean(esfi_rate_100, na.rm = TRUE),
    median_rate_100 = median(esfi_rate_100, na.rm = TRUE),
    maximum_hits = max(esfi_hits),
    maximum_rate_100 = max(esfi_rate_100, na.rm = TRUE),
    zero_feature_documents = sum(document_length == 0)
  )

esfi_count_distribution <- esfi_document_scores |>
  dplyr::count(esfi_hits, name = "documents") |>
  dplyr::mutate(
    percent_of_documents =
      100 * documents / sum(documents)
  ) |>
  dplyr::arrange(esfi_hits)

print(esfi_overall_summary)
print(esfi_count_distribution)

##-------------------------------------------------------------
## 5. Known-groups summary by source
##-------------------------------------------------------------

esfi_summary_by_source <- esfi_document_scores |>
  dplyr::group_by(source) |>
  dplyr::summarise(
    documents = dplyr::n(),
    documents_with_esfi = sum(esfi_present),
    percent_with_esfi = 100 * mean(esfi_present),
    total_esfi_hits = sum(esfi_hits),
    mean_hits_per_document = mean(esfi_hits),
    median_hits_per_document = median(esfi_hits),
    mean_hits_positive_documents = if (any(esfi_present)) {
      mean(esfi_hits[esfi_present])
    } else {
      NA_real_
    },
    mean_rate_100 = mean(esfi_rate_100, na.rm = TRUE),
    median_rate_100 = median(esfi_rate_100, na.rm = TRUE),
    maximum_hits = max(esfi_hits),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    percent_of_all_hits =
      100 * total_esfi_hits / sum(total_esfi_hits)
  ) |>
  dplyr::arrange(dplyr::desc(total_esfi_hits))

print(esfi_summary_by_source)

##-------------------------------------------------------------
## 6. Probability of any ESFI language by source
##-------------------------------------------------------------

esfi_presence_model <- stats::glm(
  esfi_present ~ source + log1p(document_length),
  data = esfi_document_scores,
  family = stats::binomial()
)

esfi_presence_odds_ratios <- broom::tidy(
  esfi_presence_model,
  exponentiate = TRUE,
  conf.int = TRUE
)

print(summary(esfi_presence_model))
print(esfi_presence_odds_ratios)

##-------------------------------------------------------------
## 7. ESFI hit counts by source
##-------------------------------------------------------------

esfi_count_poisson <- stats::glm(
  esfi_hits ~ source + offset(log(document_length)),
  data = esfi_model_data,
  family = stats::poisson()
)

dispersion_ratio <-
  sum(
    stats::residuals(
      esfi_count_poisson,
      type = "pearson"
    )^2
  ) /
  stats::df.residual(esfi_count_poisson)

message(
  "Poisson dispersion ratio: ",
  round(dispersion_ratio, 3)
)

## Fit the negative-binomial model only when overdispersion is evident.
if (is.finite(dispersion_ratio) && dispersion_ratio > 1.5) {
  esfi_count_nb <- MASS::glm.nb(
    esfi_hits ~ source + offset(log(document_length)),
    data = esfi_model_data
  )

  esfi_count_rate_ratios <- broom::tidy(
    esfi_count_nb,
    exponentiate = TRUE,
    conf.int = TRUE
  )

  count_model_used <- "negative_binomial"
  print(summary(esfi_count_nb))
  print(esfi_count_rate_ratios)
} else {
  esfi_count_nb <- NULL

  esfi_count_rate_ratios <- broom::tidy(
    esfi_count_poisson,
    exponentiate = TRUE,
    conf.int = TRUE
  )

  count_model_used <- "poisson"
  print(summary(esfi_count_poisson))
  print(esfi_count_rate_ratios)
}

##-------------------------------------------------------------
## 8. Review high-scoring documents
##-------------------------------------------------------------

corpus_text <- as.character(corp_vet_analysis)
names(corpus_text) <- quanteda::docnames(corp_vet_analysis)

high_esfi_documents <- esfi_document_scores |>
  dplyr::arrange(
    dplyr::desc(esfi_hits),
    dplyr::desc(esfi_rate_100)
  ) |>
  dplyr::slice_head(n = 30) |>
  dplyr::mutate(
    excerpt = substr(
      corpus_text[document_name],
      1,
      1500
    )
  )

print(
  high_esfi_documents |>
    dplyr::select(
      document_name,
      source,
      year,
      document_length,
      esfi_hits,
      esfi_rate_100,
      excerpt
    ),
  n = 30
)

##-------------------------------------------------------------
## 9. Save validation outputs
##-------------------------------------------------------------

readr::write_csv(
  esfi_document_scores,
  file.path(
    validation_output_dir,
    "esfi_document_scores.csv"
  )
)

readr::write_csv(
  esfi_overall_summary,
  file.path(
    validation_output_dir,
    "esfi_overall_summary.csv"
  )
)

readr::write_csv(
  esfi_count_distribution,
  file.path(
    validation_output_dir,
    "esfi_count_distribution.csv"
  )
)

readr::write_csv(
  esfi_summary_by_source,
  file.path(
    validation_output_dir,
    "esfi_summary_by_source.csv"
  )
)

readr::write_csv(
  high_esfi_documents,
  file.path(
    validation_output_dir,
    "high_esfi_documents_for_review.csv"
  )
)

readr::write_csv(
  esfi_presence_odds_ratios,
  file.path(
    validation_output_dir,
    "esfi_presence_odds_ratios.csv"
  )
)

readr::write_csv(
  esfi_count_rate_ratios,
  file.path(
    validation_output_dir,
    "esfi_count_rate_ratios.csv"
  )
)

validation_check_summary <- tibble::tibble(
  check = c(
    "analysis_documents",
    "analysis_features",
    "matched_esfi_features",
    "total_esfi_hits",
    "esfi_positive_documents",
    "stored_scores_match_recalculation",
    "count_model_used",
    "poisson_dispersion_ratio"
  ),
  value = c(
    as.character(quanteda::ndoc(dfm_vet_analysis_with_esfi)),
    as.character(quanteda::nfeat(dfm_vet_analysis_with_esfi)),
    as.character(length(quanteda::featnames(dfm_esfi_recalculated))),
    as.character(sum(stored_esfi_hits)),
    as.character(sum(stored_esfi_present)),
    "TRUE",
    count_model_used,
    as.character(dispersion_ratio)
  )
)

readr::write_csv(
  validation_check_summary,
  file.path(
    validation_output_dir,
    "esfi_validation_checks.csv"
  )
)

esfi_validation_bundle <- list(
  dictionary = esfi_dictionary,
  matched_features = quanteda::featnames(
    dfm_esfi_recalculated
  ),
  document_scores = esfi_document_scores,
  model_data = esfi_model_data,
  overall_summary = esfi_overall_summary,
  count_distribution = esfi_count_distribution,
  summary_by_source = esfi_summary_by_source,
  presence_model = esfi_presence_model,
  presence_odds_ratios = esfi_presence_odds_ratios,
  poisson_model = esfi_count_poisson,
  dispersion_ratio = dispersion_ratio,
  count_model_used = count_model_used,
  negative_binomial_model = esfi_count_nb,
  count_rate_ratios = esfi_count_rate_ratios,
  high_esfi_documents = high_esfi_documents,
  validation_checks = validation_check_summary
)

saveRDS(
  esfi_validation_bundle,
  file.path(
    validation_output_dir,
    "esfi_validation_results.rds"
  )
)

saved_files <- c(
  "esfi_document_scores.csv",
  "esfi_overall_summary.csv",
  "esfi_count_distribution.csv",
  "esfi_summary_by_source.csv",
  "high_esfi_documents_for_review.csv",
  "esfi_presence_odds_ratios.csv",
  "esfi_count_rate_ratios.csv",
  "esfi_validation_checks.csv",
  "esfi_validation_results.rds"
)

stopifnot(
  all(
    file.exists(
      file.path(
        validation_output_dir,
        saved_files
      )
    )
  )
)

message("")
message("05_validate_esfi_dictionary.R completed successfully.")
message("Analysis documents: ", quanteda::ndoc(dfm_vet_analysis_with_esfi))
message("Matched ESFI features: ", length(quanteda::featnames(dfm_esfi_recalculated)))
message("Total ESFI hits: ", sum(stored_esfi_hits))
message("Documents with ESFI language: ", sum(stored_esfi_present))
message("Count model used: ", count_model_used)
message(
  "Outputs written to: ",
  normalizePath(
    validation_output_dir,
    winslash = "/",
    mustWork = TRUE
  )
)
