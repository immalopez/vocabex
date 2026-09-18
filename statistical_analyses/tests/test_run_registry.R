suppressPackageStartupMessages({
  library(digest)
  library(jsonlite)
})

source(file.path("R", "run_registry.R"))

portable <- portable_run_manifest(list(
  project_directory = "/Users/reviewer/private-project/statistical_analyses",
  command = c("/Library/Frameworks/R.framework/Resources/bin/exec/R",
              "--no-restore", "--file=/Users/reviewer/private-project/run_analysis.R")
))
stopifnot(
  identical(portable$project_directory, "."),
  identical(portable$command[[1]], "R"),
  identical(portable$command[[3]], "--file=run_analysis.R"),
  !any(grepl("^(/|[A-Za-z]:[/\\\\])", portable$command))
)
windows_portable <- portable_run_manifest(list(
  project_directory = "C:\\Users\\reviewer\\private-project",
  command = c("C:\\Program Files\\R\\R.exe", "--file=C:\\private-project\\run_analysis.R")
))
stopifnot(
  identical(windows_portable$project_directory, "."),
  identical(windows_portable$command, c("R.exe", "--file=run_analysis.R"))
)

test_parent <- tempfile("vocabex-run-registry-")
test_dir <- file.path(test_parent, "test-run")
dir.create(test_dir, recursive = TRUE)
old_env <- Sys.getenv(c("VOCABEX_RUN_ID", "VOCABEX_RUN_DIR"), unset = NA_character_)
on.exit({
  unlink(test_parent, recursive = TRUE)
  for (name in names(old_env)) {
    if (is.na(old_env[[name]])) {
      Sys.unsetenv(name)
    } else {
      do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
    }
  }
}, add = TRUE)

Sys.setenv(VOCABEX_RUN_ID = "test-run", VOCABEX_RUN_DIR = test_dir)
for (part in c("fits", "models", "inputs", "artifacts")) {
  dir.create(run_path(part), recursive = TRUE)
}
write_run_manifest(list(run_id = "test-run", status = "testing"))

outside_file <- tempfile("vocabex-outside-root-")
writeLines("outside", outside_file)
on.exit(unlink(outside_file), add = TRUE)
outside_record <- try(file_record(outside_file, root = test_parent), silent = TRUE)
stopifnot(inherits(outside_record, "try-error"))

data <- data.frame(y = c(0, 1, 0, 1), x = c(-1, 0, 1, 2))
save_run_data(data, "test_model")
saved_data <- load_run_data("test_model", expected_data = data)
stopifnot(identical(saved_data, data))

wrong_data <- transform(data, x = x + 1)
data_mismatch <- try(load_run_data("test_model", expected_data = wrong_data), silent = TRUE)
stopifnot(inherits(data_mismatch, "try-error"))

fit <- glm(y ~ x, data = data, family = binomial())
save_run_fit(fit, "test_model", data = data, seed = 7L,
             settings = list(purpose = "registry test"))
loaded <- load_run_fit("test_model", expected_data = data)
stopifnot(inherits(loaded, "glm"), identical(attr(loaded, "vocabex_run")$run_id, "test-run"))
wrong_formula <- try(load_run_fit("test_model", expected_formula = y ~ I(x^2)), silent = TRUE)
stopifnot(inherits(wrong_formula, "try-error"))

manifest <- read_run_manifest()
manifest$status <- "complete"
write_run_manifest(manifest)
stopifnot(isTRUE(validate_run_directory(test_dir)))

mismatch <- try(load_run_fit("test_model", expected_data = wrong_data), silent = TRUE)
stopifnot(inherits(mismatch, "try-error"))

data_path <- run_path("inputs", "model_data", "test_model.rds")
original_data_raw <- readBin(data_path, "raw", n = file.info(data_path)$size)
writeBin(c(original_data_raw, as.raw(0)), data_path)
tampered_data <- try(load_run_data("test_model"), silent = TRUE)
stopifnot(inherits(tampered_data, "try-error"))
tampered_run <- try(validate_run_directory(test_dir), silent = TRUE)
stopifnot(inherits(tampered_run, "try-error"))
writeBin(original_data_raw, data_path)
stopifnot(isTRUE(validate_run_directory(test_dir)))

fit_path <- run_path("fits", "test_model.rds")
writeBin(c(readBin(fit_path, "raw", n = file.info(fit_path)$size), as.raw(0)), fit_path)
tampered <- try(load_run_fit("test_model", expected_data = data), silent = TRUE)
stopifnot(inherits(tampered, "try-error"))

cat("run registry tests passed\n")
