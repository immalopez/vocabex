suppressPackageStartupMessages({
  library(digest)
  library(jsonlite)
})

source("R/run_registry.R")
source("R/run_runtime.R")

test_root <- tempfile("vocabex-cache-")
runs_root <- file.path(test_root, "runs")
prior_dir <- file.path(runs_root, "prior-run")
active_dir <- file.path(runs_root, ".incomplete", "active-run")
old_env <- Sys.getenv(c("VOCABEX_RUN_ID", "VOCABEX_RUN_DIR"), unset = NA_character_)
on.exit({
  unlink(test_root, recursive = TRUE)
  for (name in names(old_env)) {
    if (is.na(old_env[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
  }
}, add = TRUE)

make_run <- function(id, path, status) {
  Sys.setenv(VOCABEX_RUN_ID = id, VOCABEX_RUN_DIR = path)
  for (part in c("fits", "models", "diagnostics", "artifacts", "logs", "tables")) {
    dir.create(run_path(part), recursive = TRUE, showWarnings = FALSE)
  }
  write_run_manifest(list(run_id = id, status = status))
}

data <- data.frame(y = c(0, 1, 0, 1, 1, 0), x = c(-2, -1, 0, 1, 2, 3))
formula <- y ~ x
family <- list(family = "binomial", link = "logit")
settings <- list(purpose = "cache test")

make_run("prior-run", prior_dir, "complete")
fit_key <- run_cache_key(
  "model_fit", "cached_glm",
  inputs = list(data_sha256 = sha256_object(data), formula = canonical_formula_text(formula),
                family = family, priors = NULL),
  settings = c(list(seed = NULL), settings)
)
prior_fit <- glm(formula, data = data, family = binomial())
save_run_fit(prior_fit, "cached_glm", data, formula, settings = settings,
             cache_key = fit_key, filename = "cached_glm.rds")

artifact_key <- run_cache_key(
  "rds_artifact", "cached_artifact", inputs = list(value = 42L),
  settings = list(method = "test")
)
artifact_path <- run_path("diagnostics", "cached_artifact.rds")
saveRDS(list(answer = 42L), artifact_path)
record_run_artifact(
  artifact_path, "cached_artifact",
  settings = list(method = "test", cache_key = artifact_key, cache_source = NULL)
)
prior_manifest <- read_run_manifest()
prior_manifest$models <- list(file_record(run_path("models", "cached_glm.json"), prior_dir))
prior_manifest$diagnostics <- list(file_record(artifact_path, prior_dir))
write_run_manifest(prior_manifest)

make_run("active-run", active_dir, "testing")
initialize_run_runtime(run_path("logs", "analysis.log"))

reused_fit <- cached_run_fit(
  "Cached GLM", "cached_glm", data = data, formula = formula, family = family,
  settings = settings,
  fit_function = function() stop("fit should have been reused"),
  filename = "cached_glm.rds"
)
stopifnot(
  inherits(reused_fit, "glm"),
  identical(attr(reused_fit, "vocabex_run")$run_id, "active-run")
)
fit_metadata <- read_json(run_path("models", "cached_glm.json"), simplifyVector = FALSE)
stopifnot(identical(fit_metadata$cache_source, "prior-run"))

calculated <- FALSE
changed_fit <- cached_run_fit(
  "Changed GLM", "changed_glm", data = transform(data, x = x + 1),
  formula = formula, family = family, settings = settings,
  fit_function = function() {
    calculated <<- TRUE
    glm(formula, data = transform(data, x = x + 1), family = binomial())
  }, filename = "changed_glm.rds"
)
stopifnot(calculated, inherits(changed_fit, "glm"))

reused_artifact <- cached_run_rds(
  "Cached artifact", "cached_artifact", file.path("diagnostics", "cached_artifact.rds"),
  inputs = list(value = 42L), settings = list(method = "test"),
  compute_function = function() stop("artifact should have been reused")
)
stopifnot(identical(reused_artifact$answer, 42L))

timings <- run_timing_table()
stopifnot(
  nrow(timings) == 3L,
  identical(timings$cache_status, c("cached", "calculated", "cached")),
  identical(timings$cache_source[c(1, 3)], c("prior-run", "prior-run")),
  any(grepl("cached from prior-run", readLines(run_path("logs", "analysis.log")), fixed = TRUE))
)

timing_path <- run_path("tables", "analysis_timings.csv")
write_run_timing_report(timing_path)
stopifnot(
  file.exists(timing_path), nrow(read.csv(timing_path)) == 4L,
  tail(read.csv(timing_path)$step, 1L) == "TOTAL"
)

# A corrupted completed-run fit must be ignored and recalculated.
prior_fit_path <- file.path(prior_dir, "fits", "cached_glm.rds")
writeBin(c(readBin(prior_fit_path, "raw", n = file.info(prior_fit_path)$size), as.raw(0)), prior_fit_path)
corrupt_active <- file.path(runs_root, ".incomplete", "corrupt-active")
make_run("corrupt-active", corrupt_active, "testing")
initialize_run_runtime(run_path("logs", "analysis.log"))
recalculated_corrupt <- FALSE
cached_run_fit(
  "Corrupt cache fallback", "cached_glm", data = data, formula = formula, family = family,
  settings = settings,
  fit_function = function() {
    recalculated_corrupt <<- TRUE
    glm(formula, data = data, family = binomial())
  }, filename = "cached_glm.rds"
)
stopifnot(recalculated_corrupt)

# A matching fit in a non-complete run must never be reused.
incomplete_dir <- file.path(runs_root, "incomplete-source")
make_run("incomplete-source", incomplete_dir, "failed")
incomplete_fit <- glm(formula, data = data, family = binomial())
incomplete_key <- run_cache_key(
  "model_fit", "incomplete_glm",
  inputs = list(data_sha256 = sha256_object(data), formula = canonical_formula_text(formula),
                family = family, priors = NULL),
  settings = c(list(seed = NULL), settings)
)
save_run_fit(incomplete_fit, "incomplete_glm", data, formula, settings = settings,
             cache_key = incomplete_key, filename = "incomplete_glm.rds")
incomplete_manifest <- read_run_manifest()
incomplete_manifest$models <- list(
  file_record(run_path("models", "incomplete_glm.json"), incomplete_dir)
)
write_run_manifest(incomplete_manifest)

incomplete_active <- file.path(runs_root, ".incomplete", "incomplete-active")
make_run("incomplete-active", incomplete_active, "testing")
initialize_run_runtime(run_path("logs", "analysis.log"))
recalculated_incomplete <- FALSE
cached_run_fit(
  "Incomplete cache fallback", "incomplete_glm", data = data, formula = formula,
  family = family, settings = settings,
  fit_function = function() {
    recalculated_incomplete <<- TRUE
    glm(formula, data = data, family = binomial())
  }, filename = "incomplete_glm.rds"
)
stopifnot(recalculated_incomplete)

# Failed steps remain available in a partial timing report.
failure_active <- file.path(runs_root, ".incomplete", "failure-active")
make_run("failure-active", failure_active, "testing")
initialize_run_runtime(run_path("logs", "analysis.log"))
expected_failure <- try(run_timed_step("Expected failure", stop("intentional test error")), silent = TRUE)
stopifnot(inherits(expected_failure, "try-error"), run_timing_table()$status == "failed")
failure_timing_path <- run_path("tables", "analysis_timings.csv")
write_run_timing_report(failure_timing_path)
failure_timings <- read.csv(failure_timing_path)
stopifnot(tail(failure_timings$status, 1L) == "failed")

cat("run cache and timing tests passed\n")
