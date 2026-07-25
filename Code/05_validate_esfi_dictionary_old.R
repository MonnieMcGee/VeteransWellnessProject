###############################################################
## ESFI validation: dictionary contribution diagnostics
###############################################################
## 03_validate_esfi_dictionary.R
##
## Purpose
## -------
## Evaluate the document-level behavior and known-groups validity
## of the final Experienced System Fragmentation Index (ESFI).
###############################################################

source("00_project_setup.R")

##-------------------------------------------------------------
## 0. Import and verify the frozen analysis DFM
##-------------------------------------------------------------

dfm_file <- file.path(
  "Data",
  "dfm_vet_analysis_final.rds"
)

dictionary_file <- file.path(
  "Output",
  "esfi_dictionary_development",
  "esfi_dict_final.rds"
)

output_dir <- file.path(
  "Output",
  "esfi_validation"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dfm_vet_analysis <- readRDS(dfm_file)
esfi_dict_final <- readRDS(dictionary_file)

stopifnot(quanteda::is.dfm(dfm_vet_analysis))
stopifnot(is.character(esfi_dict_final))
stopifnot("source" %in% names(quanteda::docvars(dfm_vet_analysis)))


##-------------------------------------------------------------
## 1. Calculate document-level ESFI scores
##-------------------------------------------------------------

dfm_esfi <- quanteda::dfm_select(
  dfm_vet_analysis,
  pattern = esfi_dict_final,
  valuetype = "glob",
  selection = "keep"
)

esfi_hits <- as.numeric(
  quanteda::rowSums(dfm_esfi)
)

document_length <- as.numeric(
  quanteda::ntoken(dfm_vet_analysis)
)

esfi_document_scores <- tibble(
  doc_id = quanteda::docnames(dfm_vet_analysis),
  source = quanteda::docvars(dfm_vet_analysis, "source"),
  year = quanteda::docvars(dfm_vet_analysis, "year"),
  document_length = document_length,
  esfi_hits = esfi_hits,
  esfi_present = esfi_hits > 0,
  esfi_rate_100 = if_else(
    document_length > 0,
    100 * esfi_hits / document_length,
    NA_real_
  )
)

esfi_document_scores <- esfi_document_scores |>
  mutate(
    source = factor(
      source,
      levels = c("Dallas", "Finance", "Veteran")
    )
  )

stopifnot(nrow(esfi_document_scores) == quanteda::ndoc(dfm_vet_analysis))
stopifnot(!anyNA(esfi_document_scores$esfi_hits))

## Documents with at least one retained feature are eligible for
## length-standardized analyses and count models.
esfi_model_data <- esfi_document_scores |>
  filter(document_length > 0)

message(
  "Documents used in length-adjusted models: ",
  nrow(esfi_model_data)
)

message(
  "Zero-feature documents excluded from length-adjusted models: ",
  sum(esfi_document_scores$document_length == 0)
)

##-------------------------------------------------------------
## 2. Overall ESFI prevalence and distribution
##-------------------------------------------------------------

esfi_overall_summary <- esfi_document_scores |>
  summarise(
    documents = n(),
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

print(esfi_overall_summary)

esfi_count_distribution <- esfi_document_scores |>
  count(esfi_hits, name = "documents") |>
  mutate(
    percent_of_documents =
      100 * documents / sum(documents)
  ) |>
  arrange(esfi_hits)

print(esfi_count_distribution)

##-------------------------------------------------------------
## 3. Known-groups summary by source
##-------------------------------------------------------------

esfi_summary_by_source <- esfi_document_scores |>
  group_by(source) |>
  summarise(
    documents = n(),
    documents_with_esfi = sum(esfi_present),
    percent_with_esfi = 100 * mean(esfi_present),
    total_esfi_hits = sum(esfi_hits),
    mean_hits_per_document = mean(esfi_hits),
    median_hits_per_document = median(esfi_hits),
    mean_hits_positive_documents =
      if (any(esfi_present)) {
        mean(esfi_hits[esfi_present])
      } else {
        NA_real_
      },
    mean_rate_100 = mean(esfi_rate_100, na.rm = TRUE),
    median_rate_100 = median(esfi_rate_100, na.rm = TRUE),
    maximum_hits = max(esfi_hits),
    .groups = "drop"
  ) |>
  mutate(
    percent_of_all_hits =
      100 * total_esfi_hits / sum(total_esfi_hits)
  ) |>
  arrange(desc(total_esfi_hits))

print(esfi_summary_by_source)

##-------------------------------------------------------------
## 4. Compare probability of any ESFI language by source
##-------------------------------------------------------------

esfi_presence_model <- glm(
  esfi_present ~ source + log1p(document_length),
  data = esfi_document_scores,
  family = binomial()
)

summary(esfi_presence_model)

esfi_presence_odds_ratios <- broom::tidy(
  esfi_presence_model,
  exponentiate = TRUE,
  conf.int = TRUE
)

print(esfi_presence_odds_ratios)

##-------------------------------------------------------------
## 5. Compare ESFI hit counts by source
##-------------------------------------------------------------

esfi_count_poisson <- glm(
  esfi_hits ~ source + offset(log(document_length)),
  data = esfi_model_data,
  family = poisson()
)

dispersion_ratio <-
  sum(residuals(esfi_count_poisson, type = "pearson")^2) /
  df.residual(esfi_count_poisson)

dispersion_ratio

# If overdispersion present

esfi_count_nb <- MASS::glm.nb(
  esfi_hits ~ source + offset(log(document_length)),
  data = esfi_model_data
)

summary(esfi_count_nb)

esfi_count_rate_ratios <- broom::tidy(
  esfi_count_nb,
  exponentiate = TRUE,
  conf.int = TRUE
)

print(esfi_count_rate_ratios)

## Read in corpus
corp_vet_analysis <- readRDS(
  file.path(
    "Data",
    "corp_vet_analysis_final.rds"
  )
)

stopifnot(inherits(corp_vet_analysis, "corpus"))

stopifnot(
  identical(
    quanteda::docnames(corp_vet_analysis),
    quanteda::docnames(dfm_vet_analysis)
  )
)

##-------------------------------------------------------------
## 6. Review high-scoring documents
##-------------------------------------------------------------

corpus_text <- as.character(corp_vet_analysis)
names(corpus_text) <- quanteda::docnames(corp_vet_analysis)

high_esfi_documents <- esfi_document_scores |>
  arrange(
    desc(esfi_hits),
    desc(esfi_rate_100)
  ) |>
  slice_head(n = 30) |>
  mutate(
    excerpt = substr(
      corpus_text[doc_id],
      1,
      1500
    )
  )

print(
  high_esfi_documents |>
    select(
      doc_id,
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
## 7. Save validation outputs
##-------------------------------------------------------------

write.csv(
  esfi_document_scores,
  file.path(output_dir, "esfi_document_scores.csv"),
  row.names = FALSE
)

write.csv(
  esfi_overall_summary,
  file.path(output_dir, "esfi_overall_summary.csv"),
  row.names = FALSE
)

write.csv(
  esfi_count_distribution,
  file.path(output_dir, "esfi_count_distribution.csv"),
  row.names = FALSE
)

write.csv(
  esfi_summary_by_source,
  file.path(output_dir, "esfi_summary_by_source.csv"),
  row.names = FALSE
)

write.csv(
  high_esfi_documents,
  file.path(output_dir, "high_esfi_documents_for_review.csv"),
  row.names = FALSE
)

write.csv(
  esfi_presence_odds_ratios,
  file.path(output_dir, "esfi_presence_odds_ratios.csv"),
  row.names = FALSE
)

if (exists("esfi_count_rate_ratios")) {
  write.csv(
    esfi_count_rate_ratios,
    file.path(output_dir, "esfi_count_rate_ratios.csv"),
    row.names = FALSE
  )
}

esfi_validation_bundle <- list(
  final_dictionary = esfi_dict_final,
  document_scores = esfi_document_scores,
  model_data = esfi_model_data,
  overall_summary = esfi_overall_summary,
  count_distribution = esfi_count_distribution,
  summary_by_source = esfi_summary_by_source,
  presence_model = esfi_presence_model,
  presence_odds_ratios = esfi_presence_odds_ratios,
  poisson_model = esfi_count_poisson,
  dispersion_ratio = dispersion_ratio,
  negative_binomial_model = esfi_count_nb,
  count_rate_ratios = esfi_count_rate_ratios,
  high_esfi_documents = high_esfi_documents
)

saveRDS(
  esfi_validation_bundle,
  file.path(output_dir, "esfi_validation_results.rds")
)

message("ESFI validation completed successfully.")
message(
  "Outputs written to: ",
  normalizePath(output_dir, winslash = "/", mustWork = TRUE)
)


