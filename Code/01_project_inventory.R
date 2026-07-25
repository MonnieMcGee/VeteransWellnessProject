###############################################################
## 01_project_inventory.R
## Dallas Veterans Well-Being Assessment
##
## Purpose:
##   Create a portable inventory of files in the project,
##   including relative paths, file sizes, modification times,
##   and file extensions.
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
## 2. Create manifest directory if needed
## ------------------------------------------------------------

dir.create(
  manifest_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


## ------------------------------------------------------------
## 3. Define output file
## ------------------------------------------------------------

inventory_file <- file.path(
  manifest_dir,
  "project_inventory.txt"
)


## ------------------------------------------------------------
## 4. Identify all project files
## ------------------------------------------------------------

project_files <- list.files(
  path = project_dir,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  include.dirs = FALSE,
  no.. = TRUE
)


## ------------------------------------------------------------
## 5. Convert absolute paths to project-relative paths
## ------------------------------------------------------------

project_prefix <- paste0(
  normalizePath(
    project_dir,
    winslash = "/",
    mustWork = TRUE
  ),
  "/"
)

normalized_files <- normalizePath(
  project_files,
  winslash = "/",
  mustWork = FALSE
)

relative_paths <- sub(
  pattern = paste0(
    "^",
    project_prefix
  ),
  replacement = "",
  x = normalized_files
)


## ------------------------------------------------------------
## 6. Exclude system and version-control files
## ------------------------------------------------------------

exclude_file_names <- c(
  ".DS_Store"
)

exclude_path_patterns <- c(
  "^\\.git/",
  "^\\.Rproj\\.user/",
  "^renv/library/",
  "^renv/staging/"
)

keep_file <- !basename(relative_paths) %in% exclude_file_names

for (pattern in exclude_path_patterns) {
  keep_file <- keep_file & !grepl(
    pattern,
    relative_paths
  )
}

# Exclude the inventory file itself so rerunning the script does
# not cause the manifest to inventory its previous version.
inventory_relative_path <- file.path(
  "Reproducibility",
  "Manifests",
  basename(inventory_file)
)

keep_file <- keep_file &
  relative_paths != inventory_relative_path

project_files <- project_files[keep_file]
relative_paths <- relative_paths[keep_file]


## ------------------------------------------------------------
## 7. Collect file metadata
## ------------------------------------------------------------

file_information <- file.info(
  project_files
)

project_inventory <- tibble::tibble(
  relative_path = relative_paths,
  file_size_bytes = as.numeric(
    file_information$size
  ),
  modified_time = format(
    file_information$mtime,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "America/Chicago"
  ),
  file_extension = tools::file_ext(
    relative_paths
  )
) |>
  dplyr::arrange(
    relative_path
  )


## ------------------------------------------------------------
## 8. Write inventory
## ------------------------------------------------------------

inventory_lines <- paste(
  project_inventory$relative_path,
  project_inventory$file_size_bytes,
  project_inventory$modified_time,
  sep = "|"
)

writeLines(
  text = inventory_lines,
  con = inventory_file,
  useBytes = TRUE
)


## ------------------------------------------------------------
## 9. Validate output
## ------------------------------------------------------------

stopifnot(
  file.exists(inventory_file),
  nrow(project_inventory) > 0,
  !anyDuplicated(project_inventory$relative_path),
  all(!is.na(project_inventory$file_size_bytes))
)


## ------------------------------------------------------------
## 10. Report completion
## ------------------------------------------------------------

message(
  paste0(
    "\nProject inventory created successfully.\n",
    "Files inventoried: ",
    format(
      nrow(project_inventory),
      big.mark = ","
    ),
    "\nOutput file: ",
    inventory_file,
    "\n"
  )
)
