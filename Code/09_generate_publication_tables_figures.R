# ==============================================================================
# 09_generate_publication_tables_figures.R
#
# Purpose:
#   Generate publication-ready tables and figures from frozen outputs created
#   by Scripts 07 and 08. This script does not rebuild the corpus, refit the
#   STM, or rerun estimateEffect().
#
# Color accessibility:
#   Figures use the Okabe-Ito color-blind-friendly palette. Where groups are
#   distinguished by plotting symbols, both color and shape are used.
# ==============================================================================

source("Code/00_project_setup.R")

library(dplyr)
library(forcats)
library(ggplot2)
library(knitr)
library(readr)
library(scales)
library(stringr)
library(tidyr)

publication_dir <- file.path(output_dir, "publication")
table_dir <- file.path(publication_dir, "tables")
figure_dir <- file.path(publication_dir, "figures")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

overall_file <- file.path(esfi_stm_output_dir, "esfi_overall_summary.csv")
topic_summary_file <- file.path(esfi_stm_output_dir, "topic_esfi_summary.csv")
high_low_file <- file.path(esfi_stm_output_dir, "topic_esfi_high_low_summary.csv")
regression_file <- file.path(esfi_stm_output_dir, "topic_esfi_regression_summary.csv")

required_files <- c(
  overall_file,
  topic_summary_file,
  high_low_file,
  regression_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "The following required frozen output files are missing:\n",
    paste(missing_files, collapse = "\n")
  )
}

esfi_overall_summary <- readr::read_csv(overall_file, show_col_types = FALSE)
topic_esfi_summary <- readr::read_csv(topic_summary_file, show_col_types = FALSE)
topic_esfi_high_low <- readr::read_csv(high_low_file, show_col_types = FALSE)
topic_esfi_regression <- readr::read_csv(regression_file, show_col_types = FALSE)

stopifnot(
  nrow(esfi_overall_summary) == 1,
  esfi_overall_summary$n_documents == 3008,
  esfi_overall_summary$n_with_fragmentation == 268,
  esfi_overall_summary$total_esfi_hits == 428,
  nrow(topic_esfi_summary) == 12,
  nrow(topic_esfi_high_low) == 12,
  nrow(topic_esfi_regression) == 12
)

expected_topic_labels <- c(
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

stopifnot(
  setequal(topic_esfi_summary$dominant_topic_label, expected_topic_labels),
  setequal(topic_esfi_regression$topic_label, expected_topic_labels)
)

oi_blue <- "#0072B2"
oi_orange <- "#E69F00"
oi_vermillion <- "#D55E00"
oi_black <- "#000000"
neutral_gray <- "#666666"

publication_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(8, 12, 8, 8)
  )

save_publication_figure <- function(plot, filename, width, height) {
  ggsave(
    filename = file.path(figure_dir, paste0(filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 600,
    bg = "white"
  )

  ggsave(
    filename = file.path(figure_dir, paste0(filename, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    device = cairo_pdf,
    bg = "white"
  )
}

write_latex_table <- function(data, filename, caption, align = NULL) {
  latex_output <- knitr::kable(
    data,
    format = "latex",
    booktabs = TRUE,
    caption = caption,
    align = align,
    escape = TRUE,
    linesep = ""
  )

  writeLines(latex_output, con = file.path(table_dir, filename))
}

table_1 <- esfi_overall_summary |>
  transmute(
    `STM documents` = n_documents,
    `Documents with ESFI language` = n_with_fragmentation,
    `Percent with ESFI language` = percent(
      pct_with_fragmentation,
      accuracy = 0.1
    ),
    `Total ESFI hits` = total_esfi_hits,
    `Mean ESFI hits per document` = number(
      mean_esfi_hits,
      accuracy = 0.01
    ),
    `Maximum ESFI hits in one document` = max_esfi_hits
  )

write_csv(table_1, file.path(table_dir, "table_1_corpus_esfi_summary.csv"))

write_latex_table(
  table_1,
  "table_1_corpus_esfi_summary.tex",
  "Summary of the Reddit STM corpus and Experienced System Fragmentation Index."
)

table_2 <- topic_esfi_summary |>
  transmute(
    Topic = dominant_topic,
    `Topic label` = dominant_topic_label,
    Documents = n_documents,
    `Corpus share` = percent(pct_documents, accuracy = 0.1),
    `Documents with ESFI language` = n_with_fragmentation,
    `Percent with ESFI language` = percent(
      pct_with_fragmentation,
      accuracy = 0.1
    ),
    `Mean ESFI hits` = number(mean_esfi_hits, accuracy = 0.01)
  ) |>
  arrange(Topic)

write_csv(table_2, file.path(table_dir, "table_2_topic_esfi_summary.csv"))

write_latex_table(
  table_2,
  "table_2_topic_esfi_summary.tex",
  paste(
    "Topic prevalence and ESFI language by dominant STM topic.",
    "Dominant topic is the topic with the largest estimated document-level",
    "topic proportion."
  ),
  align = c("r", "l", "r", "r", "r", "r", "r")
)

table_3 <- topic_esfi_regression |>
  mutate(
    p_display = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ number(p_value, accuracy = 0.001)
    )
  ) |>
  arrange(desc(estimate)) |>
  transmute(
    Topic = topic_number,
    `Topic label` = topic_label,
    Estimate = number(estimate, accuracy = 0.001),
    `Standard error` = number(standard_error, accuracy = 0.001),
    `95% CI` = paste0(
      "(",
      number(lower_95, accuracy = 0.001),
      ", ",
      number(upper_95, accuracy = 0.001),
      ")"
    ),
    `p-value` = p_display
  )

write_csv(
  table_3,
  file.path(table_dir, "table_3_adjusted_esfi_topic_associations.csv")
)

write_latex_table(
  table_3,
  "table_3_adjusted_esfi_topic_associations.tex",
  paste(
    "Adjusted associations between log(1 + ESFI hits) and STM topic",
    "prevalence. Estimates are adjusted for source and year."
  ),
  align = c("r", "l", "r", "r", "l", "r")
)

figure_1_data <- topic_esfi_summary |>
  mutate(
    dominant_topic_label = fct_reorder(
      dominant_topic_label,
      pct_with_fragmentation
    ),
    veteran_service_topic = if_else(
      dominant_topic %in% c(6, 9, 11),
      "Veteran-service topic",
      "Other topic"
    )
  )

figure_1 <- ggplot(
  figure_1_data,
  aes(
    x = pct_with_fragmentation,
    y = dominant_topic_label,
    fill = veteran_service_topic
  )
) +
  geom_col(width = 0.72) +
  scale_fill_manual(
    values = c(
      "Veteran-service topic" = oi_blue,
      "Other topic" = neutral_gray
    )
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    title = "Documents containing ESFI language by dominant topic",
    subtitle = "Veteran-service topics are highlighted",
    x = "Documents containing at least one ESFI term",
    y = NULL
  ) +
  publication_theme

save_publication_figure(
  figure_1,
  "figure_1_esfi_by_dominant_topic",
  8.5,
  6.5
)

figure_2_data <- topic_esfi_regression |>
  mutate(
    result_group = case_when(
      lower_95 > 0 ~ "Positive; 95% CI excludes zero",
      upper_95 < 0 ~ "Negative; 95% CI excludes zero",
      TRUE ~ "95% CI includes zero"
    ),
    topic_label = fct_reorder(topic_label, estimate)
  )

figure_2 <- ggplot(
  figure_2_data,
  aes(
    x = estimate,
    y = topic_label,
    color = result_group,
    shape = result_group
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    color = oi_black
  ) +
  geom_errorbar(
    aes(xmin = lower_95, xmax = upper_95),
    orientation = "y",
    width = 0,
    linewidth = 0.55
  ) +
  geom_point(size = 2.8, stroke = 0.8) +
  scale_color_manual(
    values = c(
      "Positive; 95% CI excludes zero" = oi_blue,
      "Negative; 95% CI excludes zero" = oi_vermillion,
      "95% CI includes zero" = neutral_gray
    )
  ) +
  scale_shape_manual(
    values = c(
      "Positive; 95% CI excludes zero" = 16,
      "Negative; 95% CI excludes zero" = 17,
      "95% CI includes zero" = 15
    )
  ) +
  labs(
    title = "Association between ESFI language and topic prevalence",
    subtitle = paste(
      "Estimated coefficients adjusted for source and year;",
      "bars show 95% confidence intervals"
    ),
    x = paste(
      "Estimated change in topic prevalence per unit increase",
      "in log(1 + ESFI hits)"
    ),
    y = NULL
  ) +
  publication_theme

save_publication_figure(
  figure_2,
  "figure_2_adjusted_esfi_topic_coefficients",
  9,
  6.5
)

figure_3_data <- topic_esfi_high_low |>
  select(
    topic_number,
    topic_label,
    `ESFI terms present`,
    `No ESFI terms`,
    difference
  ) |>
  pivot_longer(
    cols = c(`ESFI terms present`, `No ESFI terms`),
    names_to = "ESFI group",
    values_to = "Mean topic proportion"
  ) |>
  mutate(
    topic_label = factor(
      topic_label,
      levels = topic_esfi_high_low |>
        arrange(difference) |>
        pull(topic_label)
    )
  )

figure_3 <- ggplot(
  figure_3_data,
  aes(
    x = `Mean topic proportion`,
    y = topic_label,
    color = `ESFI group`,
    shape = `ESFI group`
  )
) +
  geom_point(size = 2.8, stroke = 0.8) +
  scale_color_manual(
    values = c(
      "ESFI terms present" = oi_blue,
      "No ESFI terms" = oi_orange
    )
  ) +
  scale_shape_manual(
    values = c(
      "ESFI terms present" = 16,
      "No ESFI terms" = 17
    )
  ) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.01, 0.04))
  ) +
  labs(
    title = "Mean topic prevalence with and without ESFI language",
    subtitle = "Unadjusted document-level comparison",
    x = "Mean topic proportion",
    y = NULL
  ) +
  publication_theme

save_publication_figure(
  figure_3,
  "figure_3_topic_prevalence_by_esfi_presence",
  9,
  6.5
)

figure_4_data <- topic_esfi_high_low |>
  filter(topic_number %in% c(6, 9, 11)) |>
  select(
    topic_number,
    topic_label,
    `ESFI terms present`,
    `No ESFI terms`
  ) |>
  pivot_longer(
    cols = c(`ESFI terms present`, `No ESFI terms`),
    names_to = "ESFI group",
    values_to = "Mean topic proportion"
  ) |>
  mutate(
    topic_label = factor(
      topic_label,
      levels = c(
        "Service Connection for Military-Related Health Conditions",
        "VA Healthcare and Community Care",
        "VA Disability Claims and the PACT Act"
      )
    )
  )

figure_4 <- ggplot(
  figure_4_data,
  aes(
    x = `ESFI group`,
    y = `Mean topic proportion`,
    color = `ESFI group`,
    shape = `ESFI group`,
    group = topic_label
  )
) +
  geom_line(color = neutral_gray, linewidth = 0.5) +
  geom_point(size = 3.2, stroke = 0.9) +
  facet_wrap(~ topic_label, ncol = 1, scales = "free_y") +
  scale_color_manual(
    values = c(
      "ESFI terms present" = oi_blue,
      "No ESFI terms" = oi_orange
    )
  ) +
  scale_shape_manual(
    values = c(
      "ESFI terms present" = 16,
      "No ESFI terms" = 17
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Veteran-service topic prevalence by ESFI-language presence",
    subtitle = "Mean document-level topic proportions",
    x = NULL,
    y = "Mean topic proportion"
  ) +
  publication_theme +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

save_publication_figure(
  figure_4,
  "figure_4_veteran_service_topics_by_esfi_presence",
  7.5,
  7.5
)

publication_files <- list.files(
  publication_dir,
  recursive = TRUE,
  full.names = TRUE
)

publication_manifest <- tibble::tibble(
  relative_path = file.path(
    basename(publication_dir),
    sub(
      paste0("^", normalizePath(publication_dir), "/?"),
      "",
      normalizePath(publication_files)
    )
  ),
  size_bytes = file.info(publication_files)$size,
  modified = file.info(publication_files)$mtime
)

write_csv(
  publication_manifest,
  file.path(publication_dir, "publication_output_manifest.csv")
)

capture.output(
  sessionInfo(),
  file = file.path(publication_dir, "session_info.txt")
)

cat("\nScript 09 completed successfully.\n")
cat("Tables saved to:", table_dir, "\n")
cat("Figures saved to:", figure_dir, "\n")
