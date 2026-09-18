#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(digest)
  library(jsonlite)
  library(rmarkdown)
})

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "run_analysis.R"
analysis_dir <- normalizePath(dirname(script_path), mustWork = TRUE)
setwd(analysis_dir)
source(file.path("R", "run_registry.R"))
source(file.path("R", "run_runtime.R"))

args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(flag) flag %in% args
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^", flag, "="), "", hit[[1]]) else default
}

if (has_flag("--help")) {
  cat(paste0(
    "Usage: Rscript run_analysis.R [--run-id=ID] [--skip-preparation] ",
    "[--skip-sensitivity] [--skip-exact-loo] [--include-agreement] ",
    "[--agreement-rater-2=lil_rater_2.csv]\n"
  ))
  quit(status = 0)
}

agreement_rater_2_argument <- arg_value("--agreement-rater-2", NULL)
include_agreement <- has_flag("--include-agreement") || !is.null(agreement_rater_2_argument)
agreement_rater_1 <- "annotated_exercise_data.csv"
agreement_rater_2 <- if (is.null(agreement_rater_2_argument))
  "lil_rater_2.csv" else agreement_rater_2_argument
if (include_agreement && (grepl("^(/|[A-Za-z]:)", agreement_rater_2) ||
                          any(strsplit(agreement_rater_2, "/", fixed = TRUE)[[1]] == ".."))) {
  stop("--agreement-rater-2 must be a path inside statistical_analyses", call. = FALSE)
}

timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
suffix <- substr(digest::digest(paste(Sys.time(), Sys.getpid(), runif(1))), 1L, 8L)
requested_id <- arg_value("--run-id", paste0(timestamp, "_", suffix))
if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", requested_id)) stop("Invalid run ID", call. = FALSE)

runs_dir <- file.path(analysis_dir, "runs")
staging_dir <- file.path(runs_dir, ".incomplete", requested_id)
final_dir <- file.path(runs_dir, requested_id)
if (dir.exists(staging_dir) || dir.exists(final_dir)) stop("Run ID already exists: ", requested_id)
for (part in c("inputs", "source", "fits", "models", "diagnostics", "tables",
               "figures", "reports", "logs", "artifacts")) {
  dir.create(file.path(staging_dir, part), recursive = TRUE, showWarnings = FALSE)
}

Sys.setenv(
  VOCABEX_RUN_ID = requested_id,
  VOCABEX_RUN_DIR = staging_dir,
  VOCABEX_SKIP_EXACT_LOO = if (has_flag("--skip-exact-loo")) "true" else "false"
)
initialize_run_runtime(run_path("logs", "analysis.log"))
run_log(paste0("Analysis run ", requested_id, " started"))

git_value <- function(args) {
  value <- tryCatch(system2("git", args, stdout = TRUE, stderr = FALSE), error = function(e) character())
  paste(value, collapse = "\n")
}

manifest <- portable_run_manifest(list(
  schema_version = 1L, registry_version = run_registry_version,
  run_id = requested_id, status = "initializing",
  started_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  git = list(commit = git_value(c("rev-parse", "HEAD")),
             branch = git_value(c("rev-parse", "--abbrev-ref", "HEAD")),
             dirty = nzchar(git_value(c("status", "--porcelain")))),
  command = commandArgs(), inputs = list(), source = list(), outputs = list()
))
write_run_manifest(manifest)

fail_run <- function(message) {
  try(update_run_manifest(status = "failed", finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
                          error = as.character(message)), silent = TRUE)
  stop(message, call. = FALSE)
}

tryCatch({
  run_timed_step("Snapshot source and raw inputs", {
    source_files <- c("analysis.Rmd", "data_exploration.Rmd", "data_preparation.R",
                      "rq2_prior_sensitivity.R", "il_agreement.R",
                      "run_analysis.R", "select_run.R",
                      "R/run_registry.R", "R/run_runtime.R", "R/diagnostics.R",
                      "R/influence_checks.R", "R/agreement.R")
    source_files <- source_files[file.exists(source_files)]
    source_records <- lapply(source_files, snapshot_file, category = "source", root = analysis_dir)
    names(source_records) <- source_files
    raw_input_files <- c("annotated_exercise_data.csv", "cleaned_data.csv", "exercises_analysis.csv")
    if (include_agreement) {
      if (!file.exists(agreement_rater_2)) {
        fail_run(paste0("Agreement input file does not exist: ", agreement_rater_2))
      }
      raw_input_files <- c(raw_input_files, agreement_rater_2)
    }
    raw_input_files <- unique(raw_input_files[file.exists(raw_input_files)])
    raw_input_records <- lapply(raw_input_files, snapshot_file, category = "inputs", root = analysis_dir)
    names(raw_input_records) <- raw_input_files
    update_run_manifest(status = "preparing", source = source_records, inputs = raw_input_records)
  })

  if (!has_flag("--skip-preparation")) {
    run_timed_step("Data preparation", {
      source("data_preparation.R", local = new.env(parent = globalenv()))
    })
  } else {
    record_skipped_step("Data preparation", "--skip-preparation was supplied")
  }
  if (has_flag("--skip-preparation") && !file.exists(run_input("data_for_analysis.csv"))) {
    if (!file.exists("data_for_analysis.csv")) fail_run("No prepared data are available")
    file.copy("data_for_analysis.csv", run_input("data_for_analysis.csv"), overwrite = FALSE)
  }
  if (!file.exists(run_input("data_for_analysis.csv"))) fail_run("data_for_analysis.csv was not produced")

  input_paths <- list.files(run_path("inputs"), full.names = TRUE, recursive = TRUE)
  input_records <- lapply(input_paths, file_record, root = run_dir(TRUE))
  update_run_manifest(status = "fitting", inputs = input_records)

  analysis_env <- new.env(parent = globalenv())
  run_timed_logged_step("Render main analysis", run_path("logs", "analysis-render.log"), {
    rmarkdown::render("analysis.Rmd", envir = analysis_env,
                      output_file = "analysis.html", output_dir = run_path("reports"), quiet = FALSE)
  })
  if (!file.exists(run_input("data_for_analysis_pca.csv"))) {
    fail_run("analysis.Rmd did not create the run-specific PCA data")
  }

  run_timed_logged_step("Render data exploration", run_path("logs", "data-exploration-render.log"), {
    rmarkdown::render("data_exploration.Rmd", envir = new.env(parent = globalenv()),
                      output_file = "data_exploration.html", output_dir = run_path("reports"), quiet = FALSE)
  })

  if (!has_flag("--skip-sensitivity")) {
    run_timed_step("RQ2 prior sensitivity analysis", {
      source("rq2_prior_sensitivity.R", local = new.env(parent = globalenv()))
    })
  } else {
    record_skipped_step("RQ2 prior sensitivity analysis", "--skip-sensitivity was supplied")
  }

  if (include_agreement) {
    run_timed_step("LIL inter-rater agreement", {
      source(file.path("R", "agreement.R"), local = TRUE)
      run_il_agreement(
        run_path("inputs", agreement_rater_1), run_path("inputs", agreement_rater_2),
        run_path("tables"), id_columns = c("participant_ID", "text_file", "error")
      )
    })
  } else {
    record_skipped_step("LIL inter-rater agreement", "--include-agreement was not supplied")
  }

  run_timed_step("Register outputs and environment", {
    reports <- list.files(run_path("reports"), full.names = TRUE)
    figures <- list.files(run_path("figures"), full.names = TRUE, recursive = TRUE)
    tables <- list.files(run_path("tables"), full.names = TRUE)
    analysis_model_ids <- c(
      "rq1_recognition", "rq1_correction", "rq1_recognition_covariate_null",
      "rq1_correction_covariate_null", "rq1_recognition_random_intercept",
      "rq1_correction_random_intercept", "rq2_primary", "rq3_correction",
      "rq3_time_only_null", "rq3_no_interactions", "rq3_random_intercept"
    )
    for (path in reports) {
      record_run_artifact(path, paste0("report_", tools::file_path_sans_ext(basename(path))),
                          source_models = analysis_model_ids)
    }
    for (path in figures) {
      artifact_id <- paste0("figure_", substr(sha256_file(path), 1L, 12L))
      record_run_artifact(path, artifact_id, source_models = analysis_model_ids,
                          settings = list(generator = if (grepl("exploration-", basename(path)))
                            "data_exploration.Rmd" else "analysis.Rmd"))
    }
    for (path in tables) {
      source_models <- if (startsWith(basename(path), "il_agreement_")) character() else
        c("rq2_primary", "rq2_generic_sensitivity")
      record_run_artifact(path, paste0("table_", tools::file_path_sans_ext(basename(path))),
                          source_models = source_models)
    }
    outputs <- lapply(c(reports, figures, tables), file_record, root = run_dir(TRUE))
    diagnostic_paths <- list.files(run_path("diagnostics"), full.names = TRUE, recursive = TRUE)
    model_metadata_paths <- list.files(run_path("models"), pattern = "\\.json$", full.names = TRUE)
    supporting_paths <- c(run_path("fits", "pca_objects_raw.rds"))
    supporting_paths <- supporting_paths[file.exists(supporting_paths)]
    final_input_paths <- list.files(run_path("inputs"), full.names = TRUE, recursive = TRUE)
    final_input_records <- lapply(final_input_paths, file_record, root = run_dir(TRUE))

    packages <- as.data.frame(installed.packages()[, c("Package", "Version"), drop = FALSE])
    package_path <- run_path("environment", "packages.csv")
    write.csv(packages, package_path, row.names = FALSE)
    session_path <- run_path("environment", "session-info.txt")
    writeLines(capture.output(sessionInfo()), session_path)
    update_run_manifest(status = "finalizing", finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
                        inputs = final_input_records, outputs = outputs,
                        models = lapply(model_metadata_paths, file_record, root = run_dir(TRUE)),
                        diagnostics = lapply(diagnostic_paths, file_record, root = run_dir(TRUE)),
                        supporting_files = lapply(supporting_paths, file_record, root = run_dir(TRUE)),
                        environment = list(packages = file_record(package_path, run_dir(TRUE)),
                                           session_info = file_record(session_path, run_dir(TRUE))))
  })
  run_timed_step("Validate run artifacts", validate_run_directory(staging_dir, require_complete = FALSE))

  timing_path <- run_path("tables", "analysis_timings.csv")
  write_run_timing_report(timing_path)
  record_run_artifact(timing_path, "table_analysis_timings",
                      settings = list(unit = "minutes", includes_cache_status = TRUE))
  run_log(paste0("Analysis run ", requested_id, " completed successfully"))

  reports <- list.files(run_path("reports"), full.names = TRUE)
  figures <- list.files(run_path("figures"), full.names = TRUE, recursive = TRUE)
  tables <- list.files(run_path("tables"), full.names = TRUE)
  logs <- list.files(run_path("logs"), full.names = TRUE)
  update_run_manifest(
    status = "complete", finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    elapsed_seconds = run_elapsed_seconds(), timings = run_timing_table(),
    outputs = lapply(c(reports, figures, tables), file_record, root = run_dir(TRUE)),
    logs = lapply(logs, file_record, root = run_dir(TRUE))
  )
  validate_run_directory(staging_dir)

  if (!file.rename(staging_dir, final_dir)) stop("Run completed but could not be finalized: ", staging_dir)
  Sys.unsetenv(c("VOCABEX_RUN_DIR", "VOCABEX_RUN_ID", "VOCABEX_SKIP_EXACT_LOO"))
  cat("Completed run ", requested_id, "\n", file.path("runs", requested_id), "\n", sep = "")
  print_run_timing_report()
  notify_run_finished()
}, error = function(e) {
  message_text <- conditionMessage(e)
  try(run_log(paste0("Analysis run failed: ", message_text), level = "ERROR"), silent = TRUE)
  try(write_run_timing_report(run_path("tables", "analysis_timings.csv")), silent = TRUE)
  try({
    logs <- list.files(run_path("logs"), full.names = TRUE)
    timing_path <- run_path("tables", "analysis_timings.csv")
    update_run_manifest(
      status = "failed", finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      elapsed_seconds = run_elapsed_seconds(), timings = run_timing_table(),
      timing_report = if (file.exists(timing_path)) file_record(timing_path, run_dir(TRUE)) else NULL,
      logs = lapply(logs, file_record, root = run_dir(TRUE)), error = message_text
    )
  }, silent = TRUE)
  print_run_timing_report()
  notify_run_finished()
  stop(message_text, call. = FALSE)
})
