#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "il_agreement.R"
analysis_dir <- normalizePath(dirname(script_path), mustWork = TRUE)
setwd(analysis_dir)
source(file.path("R", "agreement.R"))

args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(flag) flag %in% args
arg_value <- function(flag, default = NULL) {
  hit <- grep(paste0("^", flag, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^", flag, "="), "", hit[[1]]) else default
}

if (has_flag("--help")) {
  cat(paste0(
    "Usage: Rscript il_agreement.R [--rater-1=annotated_exercise_data.csv] ",
    "[--rater-2=lil_rater_2.csv] ",
    "[--output-dir=agreement_outputs] [--id-columns=participant_ID,text_file,error] ",
    "[--bootstrap=2000] [--seed=42]\n"
  ))
  quit(status = 0)
}

rater_1_path <- arg_value("--rater-1", "annotated_exercise_data.csv")
rater_2_path <- arg_value("--rater-2", "lil_rater_2.csv")
output_directory <- arg_value("--output-dir", "agreement_outputs")
id_columns <- strsplit(arg_value("--id-columns", "participant_ID,text_file,error"), ",", fixed = TRUE)[[1]]
bootstrap_iterations <- as.integer(arg_value("--bootstrap", "2000"))
seed <- as.integer(arg_value("--seed", "42"))
if (is.na(bootstrap_iterations) || bootstrap_iterations < 0L) stop("--bootstrap must be non-negative")
if (is.na(seed)) stop("--seed must be an integer")

run <- run_il_agreement(rater_1_path, rater_2_path, output_directory, id_columns,
                        bootstrap_iterations, seed)
cat("Raters: ", paste(run$result$raters, collapse = " and "), "\n", sep = "")
cat("Components: ", length(run$result$component_columns), "\n", sep = "")
