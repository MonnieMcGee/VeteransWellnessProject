# ==============================================================================
# 08_validate_esfi_with_stm.R
#
# Purpose:
#   Evaluate whether the Experienced System Fragmentation Index (ESFI) is
#   associated with interpretable STM topics, particularly topics involving
#   VA healthcare, service-connected conditions, and disability claims.
#
# Inputs:
#   - reddit_stm_model.rds
#   - reddit_stm_input.rds
#   - reddit_metadata_stm.rds
#   - reddit_dfm_stm.rds
#   - reddit_stm_texts.rds
#
# Expected ESFI variables in metadata:
#   - esfi_hits
#   - esfi_present
#
# Outputs:
#   - reddit_stm_esfi_analysis.rds
#   - topic_esfi_summary.csv
#   - topic_esfi_correlations.csv
#   - topic_esfi_high_low_summary.csv
#   - topic_esfi_regression_summary.csv
#   - estimate_effect_esfi_k12.rds
#   - representative_high_esfi_documents.csv
#
# Figures:
#   - topic_mean_esfi.png
#   - topic_pct_with_fragmentation.png
#   - esfi_effect_claims.png
#   - esfi_effect_healthcare.png
#   - esfi_effect_service_conditions.png
#
# ==============================================================================

source("Code/00_project_setup.R")

# ------------------------------------------------------------------------------
# 1. Load packages
# ------------------------------------------------------------------------------

library(stm)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)

# ------------------------------------------------------------------------------
# 2. File paths
# ------------------------------------------------------------------------------

## Output directories
figure_dir <- file.path(esfi_stm_output_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

## Input files
model_file    <- file.path(reddit_stm_output_dir, "reddit_stm_model.rds")
stm_input_file <- file.path(deriveddata_dir, "reddit_stm_input.rds")
metadata_file  <- file.path(deriveddata_dir, "reddit_metadata_stm.rds")
texts_file     <- file.path(reddit_stm_output_dir, "reddit_stm_texts.rds")
dfm_file <- file.path(deriveddata_dir,"reddit_dfm_stm.rds")

# ------------------------------------------------------------------------------
# 3. Load frozen STM model and aligned objects
# ------------------------------------------------------------------------------

required_files <- c(
  model_file,
  stm_input_file,
  metadata_file,
  dfm_file,
  texts_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "The following required files could not be found:\n",
    paste(missing_files, collapse = "\n")
  )
}

reddit_stm_model <- readRDS(model_file)
reddit_stm_input <- readRDS(stm_input_file)
reddit_metadata_stm <- readRDS(metadata_file)
reddit_dfm_stm <- readRDS(dfm_file)
texts_stm <- readRDS(texts_file)

cat(
  "Model class:",
  paste(class(reddit_stm_model), collapse = ", "),
  "\n"
)

cat(
  "Number of topics:",
  ncol(reddit_stm_model$theta),
  "\n"
)

cat(
  "Number of STM documents:",
  nrow(reddit_stm_model$theta),
  "\n"
)

cat(
  "STM vocabulary:",
  quanteda::nfeat(reddit_dfm_stm),
  "\n"
)

cat(
  "Number of metadata rows:",
  nrow(reddit_metadata_stm),
  "\n\n"
)

cat("Metadata variables:\n")
print(names(reddit_metadata_stm))

# ------------------------------------------------------------------------------
# 4. Validate inputs
# ------------------------------------------------------------------------------

stopifnot(
  ncol(reddit_stm_model$theta) == 12,
  nrow(reddit_stm_model$theta) == 3008,
  quanteda::ndoc(reddit_dfm_stm) == 3008,
  quanteda::nfeat(reddit_dfm_stm) == 1607,
  nrow(reddit_metadata_stm) == 3008,
  length(reddit_stm_input$documents) == 3008,
  length(texts_stm) == 3008
)

required_vars <- c(
  "doc_id",
  "source",
  "year",
  "esfi_hits",
  "esfi_present"
)

missing_vars <- setdiff(
  required_vars,
  names(reddit_metadata_stm)
)

if (length(missing_vars) > 0) {
  stop(
    "The following required metadata variables are missing: ",
    paste(missing_vars, collapse = ", ")
  )
}

stopifnot(
  identical(
    as.character(reddit_metadata_stm$doc_id),
    as.character(
      quanteda::docnames(reddit_dfm_stm)
    )
  ),
  identical(
    as.character(reddit_stm_input$meta$doc_id),
    as.character(reddit_metadata_stm$doc_id)
  ),
  !anyNA(reddit_metadata_stm$esfi_hits),
  !anyNA(reddit_metadata_stm$esfi_present),
  all(
    reddit_metadata_stm$esfi_present ==
      (reddit_metadata_stm$esfi_hits > 0)
  ),
  all(
    abs(
      rowSums(reddit_stm_model$theta) - 1
    ) < 1e-6
  )
)

message(
  "Total ESFI hits in STM corpus: ",
  sum(reddit_metadata_stm$esfi_hits)
)

message(
  "ESFI-positive STM documents: ",
  sum(reddit_metadata_stm$esfi_present)
)

# ------------------------------------------------------------------------------
# 5. Topic labels
# ------------------------------------------------------------------------------

topic_labels <- c(
  "Education Benefits and the GI Bill",
  "Arts, Entertainment, and Community Events",
  "Housing Costs, Property Taxes, and Affordability",
  "VA Home Loans and Home Buying",
  "Living in Texas and Choosing Where to Live",
  "Service Connection for Military-Related Health Conditions",
  "Employment and Career Opportunities",
  "Government, Politics, and Civic Discussion",
  "VA Healthcare and Community Care",
  "General Discussion and Peer Support",
  "VA Disability Claims and the PACT Act",
  "Entertainment Venues and Local Attractions"
)

topic_lookup <- tibble::tibble(
  topic_number = 1:12,
  topic_label = topic_labels
)

topic_service_connection <- 6
topic_healthcare <- 9
topic_claims <- 11

# ------------------------------------------------------------------------------
# 6. Create document-topic analysis data
# ------------------------------------------------------------------------------

theta_df <- as.data.frame(reddit_stm_model$theta)

names(theta_df) <- paste0("topic_", 1:12)

analysis_df <- dplyr::bind_cols(
  reddit_metadata_stm,
  theta_df
)

topic_matrix <- analysis_df |>
  dplyr::select(
    dplyr::starts_with("topic_")
  ) |>
  as.matrix()

stopifnot(
  nrow(topic_matrix) == nrow(analysis_df),
  ncol(topic_matrix) == 12,
  all(abs(rowSums(topic_matrix) - 1) < 1e-6)
)

analysis_df <- analysis_df |>
  dplyr::mutate(
    dominant_topic = max.col(
      topic_matrix,
      ties.method = "first"
    ),
    dominant_topic_label = topic_labels[dominant_topic],
    any_fragmentation = esfi_present,
    log_esfi_hits = log1p(esfi_hits)
  )

stopifnot(
  all(analysis_df$dominant_topic %in% 1:12),
  !anyNA(analysis_df$dominant_topic_label)
)

saveRDS(
  analysis_df,
  file.path(
    esfi_stm_output_dir,
    "reddit_stm_esfi_analysis.rds"
  )
)

# ------------------------------------------------------------------------------
# 7. Basic ESFI descriptive statistics
# ------------------------------------------------------------------------------

esfi_overall_summary <- analysis_df |>
  dplyr::summarize(
    n_documents = dplyr::n(),
    n_with_fragmentation = sum(esfi_present),
    pct_with_fragmentation = mean(esfi_present),
    total_esfi_hits = sum(esfi_hits),
    mean_esfi_hits = mean(esfi_hits),
    median_esfi_hits = median(esfi_hits),
    mean_log_esfi_hits = mean(log_esfi_hits),
    median_log_esfi_hits = median(log_esfi_hits),
    sd_log_esfi_hits = sd(log_esfi_hits),
    max_esfi_hits = max(esfi_hits)
  )

readr::write_csv(
  esfi_overall_summary,
  file.path(esfi_stm_output_dir, "esfi_overall_summary.csv")
)

print(esfi_overall_summary)


# ------------------------------------------------------------------------------
# 8. ESFI by dominant topic
# ------------------------------------------------------------------------------

topic_esfi_summary <- analysis_df |>
  dplyr::group_by(
    dominant_topic,
    dominant_topic_label
  ) |>
  dplyr::summarize(
    n_documents = n(),
    pct_documents = n() / nrow(analysis_df),
    n_with_fragmentation = sum(any_fragmentation, na.rm = TRUE),
    pct_with_fragmentation = mean(any_fragmentation, na.rm = TRUE),
    mean_esfi_hits = mean(esfi_hits, na.rm = TRUE),
    median_esfi_hits = median(esfi_hits, na.rm = TRUE),
    mean_log_esfi_hits = mean(log_esfi_hits),
    median_log_esfi_hits = median(log_esfi_hits),
    sd_log_esfi_hits = sd(log_esfi_hits),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(mean_log_esfi_hits)
  )

readr::write_csv(
  topic_esfi_summary,
  file.path(esfi_stm_output_dir, "topic_esfi_summary.csv")
)

print(topic_esfi_summary)


# ------------------------------------------------------------------------------
# 9. ESFI-topic correlations
#
# These are document-level Pearson and Spearman correlations between each
# topic proportion and the ESFI measures.
# ------------------------------------------------------------------------------

topic_esfi_correlations <- purrr::map_dfr(
  1:12,
  function(k) {
    
    topic_var <- paste0("topic_", k)
    
    tibble::tibble(
      topic_number = k,
      topic_label = topic_labels[k],
      
      pearson_log_esfi_hits = cor(
        analysis_df[[topic_var]],
        analysis_df$log_esfi_hits,
        use = "complete.obs",
        method = "pearson"
      ),
      
      spearman_log_esfi_hits = cor(
        analysis_df[[topic_var]],
        analysis_df$log_esfi_hits,
        use = "complete.obs",
        method = "spearman"
      ),
      
      pearson_esfi_hits = cor(
        analysis_df[[topic_var]],
        analysis_df$esfi_hits,
        use = "complete.obs",
        method = "pearson"
      ),
      
      spearman_esfi_hits = cor(
        analysis_df[[topic_var]],
        analysis_df$esfi_hits,
        use = "complete.obs",
        method = "spearman"
      )
    )
  }
) |>
  dplyr::arrange(
    dplyr::desc(spearman_log_esfi_hits)
  )

readr::write_csv(
  topic_esfi_correlations,
  file.path(esfi_stm_output_dir, "topic_esfi_correlations.csv")
)

print(topic_esfi_correlations)


# ------------------------------------------------------------------------------
# 10. Compare documents with and without fragmentation language
#
# This binary comparison is preferable to a median split because most
# documents are expected to have esfi_hits = 0.
# ------------------------------------------------------------------------------

high_low_topic_summary <- analysis_df |>
  dplyr::mutate(
    fragmentation_group = if_else(
      esfi_hits > 0,
      "ESFI terms present",
      "No ESFI terms"
    )
  ) |>
  dplyr::select(
    fragmentation_group,
    starts_with("topic_")
  ) |>
  tidyr::pivot_longer(
    cols = starts_with("topic_"),
    names_to = "topic",
    values_to = "topic_proportion"
  ) |>
  dplyr::mutate(
    topic_number = as.integer(str_remove(topic, "topic_")),
    topic_label = topic_labels[topic_number]
  ) |>
  dplyr::group_by(
    fragmentation_group,
    topic_number,
    topic_label
  ) |>
  dplyr::summarize(
    n_documents = n(),
    mean_topic_proportion = mean(topic_proportion, na.rm = TRUE),
    median_topic_proportion = median(topic_proportion, na.rm = TRUE),
    sd_topic_proportion = sd(topic_proportion, na.rm = TRUE),
    .groups = "drop"
  )

high_low_topic_wide <- high_low_topic_summary |>
  dplyr::select(
    fragmentation_group,
    topic_number,
    topic_label,
    mean_topic_proportion
  ) |>
  tidyr::pivot_wider(
    names_from = fragmentation_group,
    values_from = mean_topic_proportion
  ) |>
  dplyr::mutate(
    difference = `ESFI terms present` - `No ESFI terms`
  ) |>
  dplyr::arrange(desc(difference))

readr::write_csv(
  high_low_topic_wide,
  file.path(esfi_stm_output_dir, "topic_esfi_high_low_summary.csv")
)

print(high_low_topic_wide)


# ------------------------------------------------------------------------------
# 11. Fit STM prevalence model with ESFI
#
# Adjust variable names here if metadata uses different names.
#
# Recommended model:
#   Topic prevalence ~ ESFI + source + year
#
# year is treated as a factor so that no linear time trend is imposed.
# ------------------------------------------------------------------------------

if (!"source" %in% names(reddit_metadata_stm)) {
  stop("The variable 'source' is missing from the STM metadata.")
}

if (!"year" %in% names(reddit_metadata_stm)) {
  stop("The variable 'year' is missing from the STM metadata.")
}

reddit_metadata_stm <- reddit_metadata_stm |>
  dplyr::mutate(
    source = factor(
      source,
      levels = c(
        "Dallas",
        "Finance",
        "Veteran"
      )
    ),
    year_factor = factor(year),
    log_esfi_hits = log1p(esfi_hits)
  )

set.seed(2026)

effect_esfi <- stm::estimateEffect(
  1:12 ~ log_esfi_hits + source + year_factor,
  reddit_stm_model,
  meta = reddit_metadata_stm,
  uncertainty = "Global"
)

saveRDS(
  effect_esfi,
  file.path(esfi_stm_output_dir, "estimate_effect_esfi_k12.rds")
)

effect_esfi_binary <- stm::estimateEffect(
  1:12 ~ esfi_present + source + year_factor,
  reddit_stm_model,
  meta = reddit_metadata_stm,
  uncertainty = "Global"
)

saveRDS(
  effect_esfi_binary,
  file.path(esfi_stm_output_dir, "estimate_effect_esfi_binary_k12.rds")
)


# ------------------------------------------------------------------------------
# 12. Extract ESFI regression coefficient for each topic
# ------------------------------------------------------------------------------

extract_esfi_coefficient <- function(effect_model, topic_number) {
  
  topic_summary <- summary(effect_model, topics = topic_number)
  
  coef_table <- as.data.frame(topic_summary$tables[[1]])
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL
  
  esfi_row <- coef_table |>
    dplyr::filter(term == "log_esfi_hits")
  
  if (nrow(esfi_row) == 0) {
    return(
      tibble::tibble(
        topic_number = topic_number,
        estimate = NA_real_,
        standard_error = NA_real_,
        t_value = NA_real_,
        p_value = NA_real_
      )
    )
  }
  
  tibble::tibble(
    topic_number = topic_number,
    estimate = esfi_row$Estimate,
    standard_error = esfi_row$`Std. Error`,
    t_value = esfi_row$`t value`,
    p_value = esfi_row$`Pr(>|t|)`
  )
}

topic_esfi_regression_summary <- purrr::map_dfr(
  1:12,
  ~ extract_esfi_coefficient(effect_esfi, .x)
) |>
  dplyr::left_join(topic_lookup, by = "topic_number") |>
  dplyr::mutate(
    lower_95 = estimate - 1.96 * standard_error,
    upper_95 = estimate + 1.96 * standard_error
  ) |>
  dplyr::select(
    topic_number,
    topic_label,
    estimate,
    standard_error,
    lower_95,
    upper_95,
    t_value,
    p_value
  ) |>
  dplyr::arrange(desc(estimate))

readr::write_csv(
  topic_esfi_regression_summary,
  file.path(esfi_stm_output_dir, "topic_esfi_regression_summary.csv")
)

print(topic_esfi_regression_summary)



# ------------------------------------------------------------------------------
# 13. Publication-style figures
# ------------------------------------------------------------------------------

plot_topic_mean_esfi <- topic_esfi_summary |>
  dplyr::mutate(
    dominant_topic_label = fct_reorder(
      dominant_topic_label,
      mean_log_esfi_hits
    )
  ) |>
  ggplot2::ggplot(
    aes(
      x = mean_log_esfi_hits,
      y = dominant_topic_label
    )
  ) +
  geom_col() +
  labs(
    title = "Mean ESFI by Dominant STM Topic",
    x = "Mean log(1 + ESFI Hits)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(figure_dir, "topic_mean_log_esfi_hits.png"),
  plot = plot_topic_mean_esfi,
  width = 9,
  height = 6,
  dpi = 300
)


plot_topic_pct_frag <- topic_esfi_summary |>
  dplyr::mutate(
    dominant_topic_label = fct_reorder(
      dominant_topic_label,
      pct_with_fragmentation
    )
  ) |>
  ggplot2::ggplot(
    aes(
      x = pct_with_fragmentation,
      y = dominant_topic_label
    )
  ) +
  geom_col() +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Documents Containing ESFI Language by Dominant Topic",
    x = "Percentage of documents containing at least one ESFI term",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(
    figure_dir,
    "topic_pct_with_fragmentation.png"
  ),
  plot = plot_topic_pct_frag,
  width = 9,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 15. Continuous ESFI effect plots
#
# Topics:
# 6  = Service Connection for Military-Related Health Conditions
# 9  = VA Healthcare and Community Care
# 11 = VA Disability Claims and the PACT Act
# ------------------------------------------------------------------------------

png(
  filename = file.path(
    figure_dir,
    "esfi_effect_service_conditions.png"
  ),
  width = 1800,
  height = 1400,
  res = 200
)

plot(
  effect_esfi,
  covariate = "log_esfi_hits",
  topics = topic_service_connection,
  model = reddit_stm_model,
  method = "continuous",
  xlab = "log(1 + ESFI Hits)",
  ylab = "Expected Topic Proportion",
  main = "Service-Connected Conditions"
)

dev.off()


png(
  filename = file.path(
    figure_dir,
    "esfi_effect_healthcare.png"
  ),
  width = 1800,
  height = 1400,
  res = 200
)

plot(
  effect_esfi,
  covariate = "log_esfi_hits",
  topics = topic_healthcare,
  model = reddit_stm_model,
  method = "continuous",
  xlab = "log(1 + ESFI Hits)",
  ylab = "Expected Topic Proportion",
  main = "VA Healthcare and Community Care"
)

dev.off()


png(
  filename = file.path(
    figure_dir,
    "esfi_effect_claims.png"
  ),
  width = 1800,
  height = 1400,
  res = 200
)

plot(
  effect_esfi,
  covariate = "log_esfi_hits",
  topics = topic_claims,
  model = reddit_stm_model,
  method = "continuous",
  xlab = "log(1 + ESFI Hits)",
  ylab = "Expected Topic Proportion",
  main = "Disability Claims and PACT Act"
)

dev.off()


# ------------------------------------------------------------------------------
# 16. Coefficient plot for all topics
# ------------------------------------------------------------------------------

plot_esfi_coefficients <- topic_esfi_regression_summary |>
  dplyr::mutate(
    topic_label = fct_reorder(topic_label, estimate)
  ) |>
  ggplot2::ggplot(
    aes(
      x = estimate,
      y = topic_label
    )
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(
      ymin = topic_label,
      ymax = topic_label,
      xmin = lower_95,
      xmax = upper_95
    ),
    orientation = "y",
    width = 0
  ) +
  geom_point() +
  labs(
    title = "Association Between ESFI and Topic Prevalence",
    subtitle = "Coefficients adjusted for source and year",
    x = "Estimated change in topic prevalence per unit increase in log(1 + ESFI hits)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(
    figure_dir,
    "esfi_topic_coefficients.png"
  ),
  plot = plot_esfi_coefficients,
  width = 9,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 17. Extract representative high-ESFI documents
# ------------------------------------------------------------------------------

analysis_df$text <- as.character(texts_stm)

stopifnot(
  length(analysis_df$text) == nrow(analysis_df)
)

representative_high_esfi <- analysis_df |>
  dplyr::arrange(
    dplyr::desc(esfi_hits)
  ) |>
  dplyr::transmute(
    doc_id = doc_id,
    source = source,
    year = year,
    esfi_hits = esfi_hits,
    esfi_present = esfi_present,
    log_esfi_hits = log_esfi_hits,
    dominant_topic = dominant_topic,
    dominant_topic_label = dominant_topic_label,
    excerpt = stringr::str_trunc(
      stringr::str_squish(text),
      width = 1000,
      side = "right",
      ellipsis = "..."
    )
  ) |>
  dplyr::slice_head(n = 25)

readr::write_csv(
  representative_high_esfi,
  file.path(
    esfi_stm_output_dir,
    "representative_high_esfi_documents.csv"
  )
)

# ------------------------------------------------------------------------------
# 18. Focused comparison of veteran-service topics
# ------------------------------------------------------------------------------

veteran_service_topics <- topic_esfi_summary |>
  dplyr::filter(
    dominant_topic %in% c(topic_service_connection, topic_healthcare, topic_claims)
  ) |>
  dplyr::select(
    dominant_topic,
    dominant_topic_label,
    n_documents,
    pct_with_fragmentation,
    mean_esfi_hits,
    mean_log_esfi_hits,
    median_log_esfi_hits
  ) |>
  dplyr::arrange(desc(mean_log_esfi_hits))

readr::write_csv(
  veteran_service_topics,
  file.path(
    esfi_stm_output_dir,
    "veteran_service_topic_esfi_comparison.csv"
  )
)

print(veteran_service_topics)


# ------------------------------------------------------------------------------
# 19. Session information
# ------------------------------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(esfi_stm_output_dir, "session_info.txt")
)

cat("\nScript 08 completed successfully.\n")
cat("Outputs saved to:", esfi_stm_output_dir, "\n")