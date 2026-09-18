#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
  library(yaml)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "select_run.R"
analysis_dir <- normalizePath(dirname(script_path), mustWork = TRUE)
setwd(analysis_dir)
source(file.path("R", "run_registry.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("Usage: Rscript select_run.R RUN_ID", call. = FALSE)
selected_id <- args[[1]]
selected_dir <- file.path(analysis_dir, "runs", selected_id)
manifest_path <- file.path(selected_dir, "manifest.json")
if (!file.exists(manifest_path)) stop("Unknown run: ", selected_id, call. = FALSE)
manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
if (!identical(manifest$status, "complete")) stop("Only a complete run can be selected", call. = FALSE)
validate_run_directory(selected_dir)

records_root <- file.path(analysis_dir, "run_records")
dir.create(records_root, recursive = TRUE, showWarnings = FALSE)
record_dir <- file.path(records_root, selected_id)
if (dir.exists(record_dir)) stop("A record for this run is already tracked", call. = FALSE)
staging_record <- tempfile(pattern = paste0(".", selected_id, "-"), tmpdir = records_root)
dir.create(file.path(staging_record, "models"), recursive = TRUE, showWarnings = FALSE)
fail_selection <- function(message) {
  unlink(staging_record, recursive = TRUE)
  stop(message, call. = FALSE)
}
record_manifest_path <- file.path(staging_record, "manifest.json")
portable_manifest <- portable_run_manifest(manifest)
tryCatch(
  jsonlite::write_json(portable_manifest, record_manifest_path, pretty = TRUE,
                       auto_unbox = TRUE, null = "null"),
  error = function(error) fail_selection(paste("Could not write portable run manifest:", error$message))
)
model_metadata <- list.files(file.path(selected_dir, "models"), pattern = "\\.json$", full.names = TRUE)
if (length(model_metadata) &&
    !all(file.copy(model_metadata, file.path(staging_record, "models"), overwrite = FALSE))) {
  fail_selection("Could not copy all model records")
}
if (!file.rename(staging_record, record_dir)) fail_selection("Could not finalize run record")

pointer <- list(run_id = selected_id,
                manifest_sha256 = sha256_file(file.path(record_dir, "manifest.json")))
yaml::write_yaml(pointer, file.path(analysis_dir, "manuscript_run.yml"))
cat("Selected manuscript run ", selected_id, "\n", sep = "")
