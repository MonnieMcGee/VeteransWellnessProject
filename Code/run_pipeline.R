# ============================================================================
# run_pipeline.R
#
# Purpose:
#   Run the complete Dallas Veterans Reddit STM and ESFI reproducibility
#   pipeline from project setup through publication outputs and final
#   reproducibility reporting.
#
# Run from the project root with:
#   source("Code/run_pipeline.R")
# ============================================================================

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

pipeline_start_time <- Sys.time()

cat("\n")
cat("============================================================\n")
cat("Dallas Veterans reproducibility pipeline\n")
cat("Started:", format(pipeline_start_time), "\n")
cat("============================================================\n")

for (script_name in pipeline_scripts) {
  script_path <- file.path("Code", script_name)

  if (!file.exists(script_path)) {
    stop(
      "Pipeline stopped because this script is missing: ",
      script_path
    )
  }

  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("Running:", script_path, "\n")
  cat("Started:", format(Sys.time()), "\n")
  cat("------------------------------------------------------------\n")

  step_start_time <- Sys.time()

  tryCatch(
    {
      source(
        script_path,
        local = .GlobalEnv,
        echo = FALSE,
        chdir = FALSE
      )
    },
    error = function(e) {
      stop(
        "\nPipeline failed in ",
        script_path,
        ":\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  step_elapsed <- difftime(
    Sys.time(),
    step_start_time,
    units = "mins"
  )

  cat(
    "Completed:",
    script_path,
    "in",
    round(as.numeric(step_elapsed), 2),
    "minutes\n"
  )
}

pipeline_end_time <- Sys.time()
pipeline_elapsed <- difftime(
  pipeline_end_time,
  pipeline_start_time,
  units = "mins"
)

cat("\n")
cat("============================================================\n")
cat("Pipeline completed successfully.\n")
cat("Finished:", format(pipeline_end_time), "\n")
cat(
  "Total elapsed time:",
  round(as.numeric(pipeline_elapsed), 2),
  "minutes\n"
)
cat("============================================================\n")
