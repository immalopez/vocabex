#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else
  "categorical_agreement.R"
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
    "Usage: Rscript categorical_agreement.R [--input-dir=../code/resources] ",
    "[--output-dir=agreement_outputs] [--bootstrap=2000] [--seed=42]\n"
  ))
  quit(status = 0)
}

input_directory <- arg_value("--input-dir", file.path("..", "code", "resources"))
output_directory <- arg_value("--output-dir", "agreement_outputs")
bootstrap_iterations <- as.integer(arg_value("--bootstrap", "2000"))
seed <- as.integer(arg_value("--seed", "42"))
if (is.na(bootstrap_iterations) || bootstrap_iterations < 0L) {
  stop("--bootstrap must be non-negative", call. = FALSE)
}
if (is.na(seed)) stop("--seed must be an integer", call. = FALSE)

cohen_analyses <- list(
  exercise_type = list(
    path = file.path(input_directory, "cohen_kappa_exercise_type.csv"),
    rater_1 = "rater_1", rater_2 = "rater_2"
  ),
  exercise_format = list(
    path = file.path(input_directory, "cohen_kappa_exercise_format.csv"),
    rater_1 = "rater_1", rater_2 = "rater_2"
  )
)
fleiss_analyses <- list(
  error_type = list(
    path = file.path(input_directory, "fleiss_kappa_error_type.csv"),
    rater_columns = c("A01", "A02", "A03")
  )
)

run <- run_categorical_agreement(
  cohen_analyses, fleiss_analyses, output_directory, bootstrap_iterations, seed
)
cat("Cohen analyses: ", length(run$cohen), "\n", sep = "")
cat("Fleiss analyses: ", length(run$fleiss), "\n", sep = "")
