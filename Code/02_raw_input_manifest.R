###############################################################
## 02_raw_input_manifest.R
## Dallas Veterans Well-Being Assessment
##
## Purpose:
##   Create a manifest of immutable files in RawData,
##   including SHA-256 checksums, file sizes, modification
##   times, and CSV dimensions.
###############################################################

## ------------------------------------------------------------
## 1. Load project setup
## ------------------------------------------------------------

source(
  here::here(
    "Code",
    "00_project_setup.R"
  )
)


## ------------------------------------------------------------
## 2. Verify required package
## ------------------------------------------------------------

if (!requireNamespace(
  "digest",
  quietly = TRUE
)) {
  stop(
    paste0(
      "Package 'digest' is required to compute SHA-256 ",
      "checksums.\nInstall it using:\n",
      "install.packages(\"digest\")"
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 3. Verify RawData directory
## ------------------------------------------------------------

if (!dir.exists(rawdata_dir)) {
  stop(
    paste0(
      "RawData directory was not found:\n",
      rawdata_dir
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 4. Create manifest directory if needed
## ------------------------------------------------------------

dir.create(
  manifest_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


## ------------------------------------------------------------
## 5. Define output files
## ------------------------------------------------------------

manifest_file <- file.path(
  manifest_dir,
  "raw_input_manifest.csv"
)

checksum_file <- file.path(
  manifest_dir,
  "raw_input_manifest.sha256"
)


## ------------------------------------------------------------
## 6. Identify raw-input files
## ------------------------------------------------------------

raw_files <- list.files(
  path = rawdata_dir,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  include.dirs = FALSE,
  no.. = TRUE
)

raw_files <- raw_files[
  basename(raw_files) != ".DS_Store"
]

if (length(raw_files) == 0) {
  stop(
    paste0(
      "No files were found in:\n",
      rawdata_dir
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 7. Construct project-relative paths
## ------------------------------------------------------------

project_prefix <- paste0(
  normalizePath(
    project_dir,
    winslash = "/",
    mustWork = TRUE
  ),
  "/"
)

normalized_raw_files <- normalizePath(
  raw_files,
  winslash = "/",
  mustWork = TRUE
)

relative_paths <- sub(
  pattern = paste0(
    "^",
    project_prefix
  ),
  replacement = "",
  x = normalized_raw_files
)


## ------------------------------------------------------------
## 8. Function to inspect CSV dimensions
## ------------------------------------------------------------

inspect_csv <- function(file_path) {
  
  file_extension <- tolower(
    tools::file_ext(file_path)
  )
  
  if (file_extension != "csv") {
    return(
      tibble::tibble(
        row_count = NA_integer_,
        column_count = NA_integer_,
        column_names = NA_character_,
        audit_read_encoding = NA_character_,
        csv_read_status = "not_applicable"
      )
    )
  }
  
  tryCatch(
    {
      
      # Latin-1 is used for a byte-safe structural read.
      # This does not assert that the source text is Latin-1.
      csv_data <- readr::read_csv(
        file = file_path,
        locale = readr::locale(
          encoding = "Latin1"
        ),
        show_col_types = FALSE,
        progress = FALSE,
        name_repair = "minimal"
      )
      
      tibble::tibble(
        row_count = nrow(csv_data),
        column_count = ncol(csv_data),
        column_names = paste(
          names(csv_data),
          collapse = ";"
        ),
        audit_read_encoding = "Latin1",
        csv_read_status = "success"
      )
    },
    error = function(e) {
      
      tibble::tibble(
        row_count = NA_integer_,
        column_count = NA_integer_,
        column_names = NA_character_,
        audit_read_encoding = "Latin1",
        csv_read_status = paste0(
          "error: ",
          conditionMessage(e)
        )
      )
    }
  )
}


## ------------------------------------------------------------
## 9. Compute file metadata and checksums
## ------------------------------------------------------------

manifest_list <- lapply(
  seq_along(raw_files),
  function(i) {
    
    current_file <- raw_files[[i]]
    
    file_information <- file.info(
      current_file
    )
    
    csv_information <- inspect_csv(
      current_file
    )
    
    tibble::tibble(
      relative_path = relative_paths[[i]],
      file_name = basename(current_file),
      file_extension = tolower(
        tools::file_ext(current_file)
      ),
      file_size_bytes = as.numeric(
        file_information$size
      ),
      modified_time = format(
        file_information$mtime,
        format = "%Y-%m-%d %H:%M:%S",
        tz = "America/Chicago"
      ),
      sha256 = digest::digest(
        file = current_file,
        algo = "sha256",
        serialize = FALSE
      ),
      row_count = csv_information$row_count,
      column_count = csv_information$column_count,
      column_names = csv_information$column_names,
      audit_read_encoding =
        csv_information$audit_read_encoding,
      csv_read_status =
        csv_information$csv_read_status
    )
  }
)

raw_input_manifest <- dplyr::bind_rows(
  manifest_list
) |>
  dplyr::arrange(
    relative_path
  )


## ------------------------------------------------------------
## 10. Write CSV manifest
## ------------------------------------------------------------

readr::write_csv(
  raw_input_manifest,
  manifest_file,
  na = ""
)


## ------------------------------------------------------------
## 11. Write standard SHA-256 checksum file
## ------------------------------------------------------------

checksum_lines <- paste0(
  raw_input_manifest$sha256,
  "  ",
  raw_input_manifest$relative_path
)

writeLines(
  text = checksum_lines,
  con = checksum_file,
  useBytes = TRUE
)


## ------------------------------------------------------------
## 12. Validate manifest
## ------------------------------------------------------------

stopifnot(
  file.exists(manifest_file),
  file.exists(checksum_file),
  nrow(raw_input_manifest) == length(raw_files),
  !anyDuplicated(raw_input_manifest$relative_path),
  !anyDuplicated(raw_input_manifest$sha256),
  all(
    nchar(raw_input_manifest$sha256) == 64
  ),
  all(
    !is.na(raw_input_manifest$file_size_bytes)
  )
)

csv_manifest <- raw_input_manifest |>
  dplyr::filter(
    file_extension == "csv"
  )

if (
  nrow(csv_manifest) > 0 &&
  any(csv_manifest$csv_read_status != "success")
) {
  warning(
    paste0(
      "At least one CSV could not be read successfully. ",
      "Review csv_read_status in:\n",
      manifest_file
    ),
    call. = FALSE
  )
}


## ------------------------------------------------------------
## 13. Report completion
## ------------------------------------------------------------

message(
  paste0(
    "\nRaw-input manifest created successfully.\n",
    "Raw files documented: ",
    length(raw_files),
    "\nManifest: ",
    manifest_file,
    "\nChecksums: ",
    checksum_file,
    "\n"
  )
)
