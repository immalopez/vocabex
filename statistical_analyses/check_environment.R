#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else "check_environment.R"
analysis_dir <- normalizePath(dirname(script_path), mustWork = TRUE)
setwd(analysis_dir)

failures <- character()
check <- function(condition, message) {
  if (!isTRUE(condition)) failures <<- c(failures, message)
  invisible(condition)
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required; run renv::restore() first", call. = FALSE)
}
if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Package 'yaml' is required; run renv::restore() first", call. = FALSE)
}

lock <- jsonlite::read_json("renv.lock", simplifyVector = FALSE)
requirements <- yaml::read_yaml("system-requirements.yml")

expected_r <- lock$R$Version
actual_r <- as.character(getRversion())
check(identical(actual_r, expected_r), paste0("R version mismatch: expected ", expected_r,
                                              ", found ", actual_r))

locked_packages <- lock$Packages
missing_packages <- names(locked_packages)[!vapply(
  names(locked_packages), requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
)]
check(!length(missing_packages), paste("Missing locked packages:", paste(missing_packages, collapse = ", ")))

installed_locked <- setdiff(names(locked_packages), missing_packages)
version_mismatches <- vapply(installed_locked, function(package) {
  actual <- as.character(utils::packageVersion(package))
  expected <- locked_packages[[package]]$Version
  if (isTRUE(numeric_version(actual) == numeric_version(expected))) "" else
    paste0(package, " (expected ", expected, ", found ", actual, ")")
}, FUN.VALUE = character(1))
version_mismatches <- unname(version_mismatches[nzchar(version_mismatches)])
check(!length(version_mismatches), paste("Package version mismatches:",
                                         paste(version_mismatches, collapse = ", ")))

source_mismatches <- vapply(installed_locked, function(package) {
  expected <- locked_packages[[package]]$RemoteSha
  if (is.null(expected) || !nzchar(expected)) return("")
  actual <- utils::packageDescription(package)$RemoteSha
  if (!is.null(actual) && identical(actual, expected)) "" else
    paste0(package, " (expected source revision ", expected, ")")
}, FUN.VALUE = character(1))
source_mismatches <- unname(source_mismatches[nzchar(source_mismatches)])
check(!length(source_mismatches), paste("Package source mismatches:",
                                        paste(source_mismatches, collapse = ", ")))

if (requireNamespace("cmdstanr", quietly = TRUE)) {
  actual_cmdstanr <- as.character(utils::packageVersion("cmdstanr"))
  check(identical(actual_cmdstanr, requirements$cmdstanr$version),
        paste0("cmdstanr version mismatch: expected ", requirements$cmdstanr$version,
               ", found ", actual_cmdstanr))
  actual_cmdstan <- tryCatch(as.character(cmdstanr::cmdstan_version()), error = conditionMessage)
  check(identical(actual_cmdstan, requirements$cmdstan$version),
        paste0("CmdStan version mismatch: expected ", requirements$cmdstan$version,
               ", found ", actual_cmdstan))
  toolchain_error <- tryCatch({
    cmdstanr::check_cmdstan_toolchain(fix = FALSE, quiet = TRUE)
    NULL
  }, error = conditionMessage)
  check(is.null(toolchain_error), paste("CmdStan toolchain check failed:", toolchain_error))
} else {
  failures <- c(failures, "Package 'cmdstanr' is unavailable")
}

if (requireNamespace("rmarkdown", quietly = TRUE)) {
  pandoc_available <- rmarkdown::pandoc_available()
  check(pandoc_available, "Pandoc is unavailable")
  if (pandoc_available) {
    actual_pandoc <- as.character(rmarkdown::pandoc_version())
    check(identical(actual_pandoc, requirements$pandoc$version),
          paste0("Pandoc version mismatch: expected ", requirements$pandoc$version,
                 ", found ", actual_pandoc))
  }
} else {
  failures <- c(failures, "Package 'rmarkdown' is unavailable")
}

check(nzchar(Sys.which("make")), "Build tool 'make' is unavailable")
check(nzchar(Sys.which("c++")), "C++ compiler is unavailable")

cat("R:", actual_r, "\n")
cat("Locked packages:", length(locked_packages), "\n")
cat("Tested platform:", requirements$r$platform, "\n")

if (length(failures)) {
  cat("\nEnvironment check failed:\n", paste0("- ", failures, collapse = "\n"), "\n", sep = "")
  quit(status = 1L)
}

cat("Environment check passed\n")
