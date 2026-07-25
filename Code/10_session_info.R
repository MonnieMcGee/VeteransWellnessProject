# ============================================================================
# 10_session_info.R
#
# Purpose:
#   Create a reproducibility record for the completed Dallas Veterans analysis.
#   This script records R/package versions, Git information, checksums, the
#   expected project structure, and key frozen analysis counts.
#
# This script does not modify data, refit models, or regenerate results.
# ============================================================================

source("Code/00_project_setup.R")

library(digest)
library(dplyr)
library(readr)
library(tibble)

# 1. Output directory ---------------------------------------------------------

reproducibility_output_dir <- file.path(
  output_dir,
  "reproducibility"
)

dir.create(
  reproducibility_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. Project structure checks -------------------------------------------------

required_directories <- c(
  "Code",
  "RawData",
  "DerivedData",
  "Output",
  "Papers",
  "Presentations",
  "Reproducibility"
)

required_directory_paths <- file.path(
  project_dir,
  required_directories
)

directory_check <- tibble(
  directory = required_directories,
  path = required_directory_paths,
  exists = dir.exists(required_directory_paths)
)

write_csv(
  directory_check,
  file.path(
    reproducibility_output_dir,
    "directory_structure_check.csv"
  )
)

if (any(!directory_check$exists)) {
  stop(
    "The following required project directories are missing:\n",
    paste(
      directory_check$directory[!directory_check$exists],
      collapse = "\n"
    )
  )
}

# 3. Pipeline definition and script checks -----------------------------------

pipeline_scripts <- c(
  "00_project_setup.R",
  "01_project_inventory.R",
  "02_raw_input_manifest.R",
  "03_build_analysis_corpus.R",
  "04_develop_esfi_dictionary.R",
  "05_validate_esfi_dictionary.R",
  "06_prepare_stm_corpus.R",
  "07_fit_reddit_stm.R",
  "08_validate_esfi_with_stm.R",
  "09_generate_publication_tables_figures.R",
  "10_session_info.R"
)

pipeline_paths <- file.path(
  project_dir,
  "Code",
  pipeline_scripts
)

pipeline_check <- tibble(
  step = seq_along(pipeline_scripts) - 1L,
  script = pipeline_scripts,
  path = pipeline_paths,
  exists = file.exists(pipeline_paths)
)

write_csv(
  pipeline_check,
  file.path(
    reproducibility_output_dir,
    "pipeline_script_check.csv"
  )
)

if (any(!pipeline_check$exists)) {
  stop(
    "The following pipeline scripts are missing:\n",
    paste(
      pipeline_check$script[!pipeline_check$exists],
      collapse = "\n"
    )
  )
}

# 4. Session information ------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(
    reproducibility_output_dir,
    "session_info.txt"
  )
)

r_information <- tibble(
  item = c(
    "R version",
    "R platform",
    "Operating system",
    "Working directory",
    "Project directory",
    "Report generated"
  ),
  value = c(
    R.version.string,
    R.version$platform,
    Sys.info()[["sysname"]],
    getwd(),
    project_dir,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
)

write_csv(
  r_information,
  file.path(
    reproducibility_output_dir,
    "r_information.csv"
  )
)

# 5. Package versions ---------------------------------------------------------

installed_package_table <- as.data.frame(
  installed.packages(),
  stringsAsFactors = FALSE
) |>
  as_tibble() |>
  transmute(
    package = Package,
    version = Version,
    built = Built,
    library_path = LibPath
  ) |>
  arrange(package)

write_csv(
  installed_package_table,
  file.path(
    reproducibility_output_dir,
    "package_versions.csv"
  )
)

# 6. Git information ----------------------------------------------------------

run_git <- function(args) {
  result <- tryCatch(
    system2(
      "git",
      args = c("-C", shQuote(project_dir), args),
      stdout = TRUE,
      stderr = TRUE
    ),
    warning = function(w) character(0),
    error = function(e) character(0)
  )

  if (length(result) == 0) {
    NA_character_
  } else {
    paste(result, collapse = "\n")
  }
}

git_repository_expected <-
  "https://github.com/MonnieMcGee/VeteransWellnessProject"

git_commit <- run_git(c("rev-parse", "HEAD"))
git_branch <- run_git(c("branch", "--show-current"))
git_status <- run_git(c("status", "--short"))
git_remote <- run_git(c("remote", "-v"))

working_tree_clean <- if (is.na(git_status)) {
  NA
} else {
  identical(trimws(git_status), "")
}

git_information <- tibble(
  item = c(
    "Expected repository",
    "Current branch",
    "Current commit",
    "Working tree clean",
    "Git remotes",
    "Git status"
  ),
  value = c(
    git_repository_expected,
    git_branch,
    git_commit,
    as.character(working_tree_clean),
    git_remote,
    ifelse(
      is.na(git_status) || git_status == "",
      "Clean",
      git_status
    )
  )
)

write_csv(
  git_information,
  file.path(
    reproducibility_output_dir,
    "git_information.csv"
  )
)

writeLines(
  c(
    paste("Expected repository:", git_repository_expected),
    paste("Branch:", git_branch),
    paste("Commit:", git_commit),
    paste("Working tree clean:", working_tree_clean),
    "",
    "Remotes:",
    git_remote,
    "",
    "Status:",
    ifelse(
      is.na(git_status) || git_status == "",
      "Clean",
      git_status
    )
  ),
  con = file.path(
    reproducibility_output_dir,
    "git_information.txt"
  )
)

# 7. SHA-256 checksum helper --------------------------------------------------

create_checksum_table <- function(directory, label, recursive = TRUE) {
  if (!dir.exists(directory)) {
    return(
      tibble(
        category = character(),
        relative_path = character(),
        size_bytes = numeric(),
        modified = as.POSIXct(character()),
        sha256 = character()
      )
    )
  }

  files <- list.files(
    directory,
    recursive = recursive,
    full.names = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )

  files <- files[file.exists(files) & !dir.exists(files)]

  if (length(files) == 0) {
    return(
      tibble(
        category = character(),
        relative_path = character(),
        size_bytes = numeric(),
        modified = as.POSIXct(character()),
        sha256 = character()
      )
    )
  }

  tibble(
    category = label,
    relative_path = sub(
      paste0("^", normalizePath(project_dir), "/?"),
      "",
      normalizePath(files)
    ),
    size_bytes = file.info(files)$size,
    modified = file.info(files)$mtime,
    sha256 = vapply(
      files,
      digest::digest,
      character(1),
      algo = "sha256",
      file = TRUE
    )
  )
}

# 8. Checksums ----------------------------------------------------------------

raw_checksums <- create_checksum_table(rawdata_dir, "RawData")
derived_checksums <- create_checksum_table(deriveddata_dir, "DerivedData")
publication_checksums <- create_checksum_table(
  file.path(output_dir, "publication"),
  "Publication outputs"
)

write_csv(
  raw_checksums,
  file.path(reproducibility_output_dir, "rawdata_checksums.csv")
)

write_csv(
  derived_checksums,
  file.path(reproducibility_output_dir, "deriveddata_checksums.csv")
)

write_csv(
  publication_checksums,
  file.path(reproducibility_output_dir, "publication_checksums.csv")
)

all_checksums <- bind_rows(
  raw_checksums,
  derived_checksums,
  publication_checksums
)

write_csv(
  all_checksums,
  file.path(reproducibility_output_dir, "all_checksums.csv")
)

# 9. Frozen pipeline counts ---------------------------------------------------

overall_summary_file <- file.path(
  esfi_stm_output_dir,
  "esfi_overall_summary.csv"
)

stm_model_file <- file.path(
  reddit_stm_output_dir,
  "reddit_stm_model.rds"
)

analysis_dfm_file <- file.path(
  deriveddata_dir,
  "reddit_dfm_analysis_corpus.rds"
)

pipeline_counts <- tibble(
  measure = c(
    "Raw Reddit rows imported",
    "Documents in 2020-2025 corpus",
    "Analysis corpus documents",
    "Analysis corpus features",
    "STM corpus documents",
    "STM vocabulary",
    "STM topics",
    "Total ESFI hits",
    "ESFI-positive STM documents"
  ),
  expected = c(
    4245,
    3361,
    3066,
    1681,
    3008,
    1607,
    12,
    428,
    268
  ),
  observed = NA_real_
)

if (file.exists(analysis_dfm_file)) {
  analysis_dfm <- readRDS(analysis_dfm_file)

  pipeline_counts$observed[
    pipeline_counts$measure == "Analysis corpus documents"
  ] <- quanteda::ndoc(analysis_dfm)

  pipeline_counts$observed[
    pipeline_counts$measure == "Analysis corpus features"
  ] <- quanteda::nfeat(analysis_dfm)
}

if (file.exists(stm_model_file)) {
  stm_model <- readRDS(stm_model_file)

  pipeline_counts$observed[
    pipeline_counts$measure == "STM corpus documents"
  ] <- nrow(stm_model$theta)

  pipeline_counts$observed[
    pipeline_counts$measure == "STM vocabulary"
  ] <- length(stm_model$vocab)

  pipeline_counts$observed[
    pipeline_counts$measure == "STM topics"
  ] <- stm_model$settings$dim$K
}

if (file.exists(overall_summary_file)) {
  overall_summary <- read_csv(
    overall_summary_file,
    show_col_types = FALSE
  )

  pipeline_counts$observed[
    pipeline_counts$measure == "Total ESFI hits"
  ] <- overall_summary$total_esfi_hits

  pipeline_counts$observed[
    pipeline_counts$measure == "ESFI-positive STM documents"
  ] <- overall_summary$n_with_fragmentation
}

pipeline_counts <- pipeline_counts |>
  mutate(
    matches_expected = case_when(
      is.na(observed) ~ NA,
      TRUE ~ observed == expected
    )
  )

write_csv(
  pipeline_counts,
  file.path(reproducibility_output_dir, "pipeline_counts.csv")
)

# 10. Combined reproducibility report ----------------------------------------

report_lines <- c(
  "DALLAS VETERANS ANALYSIS: REPRODUCIBILITY REPORT",
  paste(rep("=", 58), collapse = ""),
  "",
  paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("Project directory:", project_dir),
  paste("GitHub repository:", git_repository_expected),
  paste("Git branch:", git_branch),
  paste("Git commit:", git_commit),
  paste("Working tree clean:", working_tree_clean),
  "",
  "PIPELINE",
  paste(rep("-", 58), collapse = ""),
  paste(
    sprintf("%02d", pipeline_check$step),
    pipeline_check$script,
    ifelse(pipeline_check$exists, "[FOUND]", "[MISSING]")
  ),
  "",
  "FROZEN COUNTS",
  paste(rep("-", 58), collapse = ""),
  paste(
    pipeline_counts$measure,
    ": expected =",
    pipeline_counts$expected,
    "; observed =",
    ifelse(
      is.na(pipeline_counts$observed),
      "not read",
      pipeline_counts$observed
    ),
    "; match =",
    ifelse(
      is.na(pipeline_counts$matches_expected),
      "not checked",
      pipeline_counts$matches_expected
    )
  ),
  "",
  "CHECKSUM FILES",
  paste(rep("-", 58), collapse = ""),
  "rawdata_checksums.csv",
  "deriveddata_checksums.csv",
  "publication_checksums.csv",
  "all_checksums.csv",
  "",
  "SOFTWARE RECORDS",
  paste(rep("-", 58), collapse = ""),
  "session_info.txt",
  "r_information.csv",
  "package_versions.csv",
  "git_information.csv",
  "git_information.txt"
)

writeLines(
  report_lines,
  con = file.path(
    reproducibility_output_dir,
    "reproducibility_report.txt"
  )
)

cat("\nScript 10 completed successfully.\n")
cat(
  "Reproducibility outputs saved to:",
  reproducibility_output_dir,
  "\n"
)
