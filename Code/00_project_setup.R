###############################################################
## 00_project_setup.R
## Dallas Veterans Well-Being Assessment
##
## Purpose:
##   Load required packages, define project-relative paths,
##   and set global reproducibility options.
###############################################################

## ------------------------------------------------------------
## 1. Load packages
## ------------------------------------------------------------

# Core tidyverse
library(tidyverse)
library(here)

# Text processing
library(quanteda)
library(readtext)
library(stm)

# Date handling
library(lubridate)

# Plot formatting
library(ggplot2)
library(scales)


## ------------------------------------------------------------
## 2. Reproducibility settings
## ------------------------------------------------------------

set.seed(1234)

options(
  stringsAsFactors = FALSE,
  timezone = "America/Chicago"
)

theme_set(theme_bw())


## ------------------------------------------------------------
## 3. Define project directories
## ------------------------------------------------------------

project_dir <- here::here()

code_dir        <- file.path(project_dir, "Code")
rawdata_dir     <- file.path(project_dir, "RawData")
deriveddata_dir <- file.path(project_dir, "DerivedData")
output_dir      <- file.path(project_dir, "Output")
repro_dir       <- file.path(project_dir, "Reproducibility")

manifest_dir    <- file.path(repro_dir, "Manifests")
log_dir         <- file.path(repro_dir, "Logs")
report_dir      <- file.path(repro_dir, "Reports")

papers_dir      <- file.path(project_dir, "Papers")
presentations_dir <- file.path(project_dir, "Presentations")

## ------------------------------------------------------------
## 4. Standard output directories
## ------------------------------------------------------------

reddit_stm_output_dir <- file.path(output_dir, "reddit_stm")
esfi_stm_output_dir   <- file.path(output_dir, "esfi_stm")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(reddit_stm_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(esfi_stm_output_dir, recursive = TRUE, showWarnings = FALSE)
