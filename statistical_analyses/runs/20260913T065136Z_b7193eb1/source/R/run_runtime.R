.vocabex_runtime <- new.env(parent = emptyenv())

initialize_run_runtime <- function(log_path = NULL) {
  .vocabex_runtime$started_at <- Sys.time()
  .vocabex_runtime$steps <- list()
  .vocabex_runtime$log_path <- log_path
  if (!is.null(log_path)) {
    dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(character(), log_path)
  }
  invisible(NULL)
}

run_log <- function(message, level = "INFO") {
  line <- sprintf(
    "%s [%s] %s",
    format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"), level, paste(message, collapse = "")
  )
  cat(line, "\n")
  path <- .vocabex_runtime$log_path
  if (!is.null(path) && nzchar(path)) cat(line, "\n", file = path, append = TRUE)
  invisible(line)
}

record_step_timing <- function(name, started_at, status = "success",
                               cache_status = "calculated", cache_source = NA_character_,
                               error = NA_character_) {
  finished_at <- Sys.time()
  entry <- data.frame(
    step = name,
    started_at = format(started_at, tz = "UTC", usetz = TRUE),
    finished_at = format(finished_at, tz = "UTC", usetz = TRUE),
    elapsed_seconds = as.numeric(difftime(finished_at, started_at, units = "secs")),
    elapsed_minutes = as.numeric(difftime(finished_at, started_at, units = "mins")),
    status = status,
    cache_status = cache_status,
    cache_source = if (is.null(cache_source)) NA_character_ else as.character(cache_source),
    error = if (is.null(error)) NA_character_ else as.character(error),
    stringsAsFactors = FALSE
  )
  .vocabex_runtime$steps[[length(.vocabex_runtime$steps) + 1L]] <- entry
  run_log(sprintf(
    "Finished step '%s' in %.2f minutes (%s%s)", name, entry$elapsed_minutes,
    cache_status, if (!is.na(entry$cache_source)) paste0(" from ", entry$cache_source) else ""
  ), if (identical(status, "failed")) "ERROR" else "INFO")
  invisible(entry)
}

record_skipped_step <- function(name, reason) {
  started_at <- Sys.time()
  run_log(sprintf("Skipped step '%s': %s", name, reason))
  record_step_timing(name, started_at, status = "skipped",
                     cache_status = "not_applicable", error = reason)
}

run_timed_step <- function(name, code, cache_status = "calculated", cache_source = NA_character_) {
  started_at <- Sys.time()
  run_log(paste0("Starting step '", name, "'"))
  tryCatch(
    {
      value <- eval.parent(substitute(code))
      record_step_timing(name, started_at, cache_status = cache_status,
                         cache_source = cache_source)
      value
    },
    error = function(e) {
      record_step_timing(name, started_at, status = "failed", cache_status = cache_status,
                         cache_source = cache_source, error = conditionMessage(e))
      stop(e)
    }
  )
}

run_timed_logged_step <- function(name, log_path, code) {
  expression <- substitute(code)
  evaluation_environment <- parent.frame()
  started_at <- Sys.time()
  run_log(paste0("Starting step '", name, "'"))
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  connection <- file(log_path, open = "wt")
  sink(connection, split = TRUE)
  on.exit({
    sink()
    close(connection)
  }, add = TRUE)
  tryCatch(
    {
      value <- withCallingHandlers(
        eval(expression, envir = evaluation_environment),
        warning = function(w) run_log(conditionMessage(w), level = "WARN"),
        message = function(m) run_log(conditionMessage(m), level = "MESSAGE")
      )
      record_step_timing(name, started_at)
      value
    },
    error = function(e) {
      record_step_timing(name, started_at, status = "failed", error = conditionMessage(e))
      stop(e)
    }
  )
}

run_timing_table <- function() {
  if (!length(.vocabex_runtime$steps)) {
    return(data.frame(
      step = character(), started_at = character(), finished_at = character(),
      elapsed_seconds = numeric(), elapsed_minutes = numeric(), status = character(),
      cache_status = character(), cache_source = character(), error = character()
    ))
  }
  timings <- do.call(rbind, .vocabex_runtime$steps)
  timings[order(timings$started_at, timings$finished_at), , drop = FALSE]
}

run_elapsed_seconds <- function() {
  as.numeric(difftime(Sys.time(), .vocabex_runtime$started_at, units = "secs"))
}

write_run_timing_report <- function(path) {
  timings <- run_timing_table()
  total <- data.frame(
    step = "TOTAL",
    started_at = format(.vocabex_runtime$started_at, tz = "UTC", usetz = TRUE),
    finished_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    elapsed_seconds = run_elapsed_seconds(),
    elapsed_minutes = run_elapsed_seconds() / 60,
    status = if (any(timings$status == "failed")) "failed" else "success",
    cache_status = "not_applicable", cache_source = NA_character_, error = NA_character_,
    stringsAsFactors = FALSE
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(rbind(timings, total), path, row.names = FALSE, na = "")
  invisible(timings)
}

print_run_timing_report <- function() {
  timings <- run_timing_table()
  cat("\nAnalysis timing report\n")
  if (nrow(timings)) {
    cat(sprintf("%-44s %8s  %s\n", "Step", "Minutes", "Result"))
    cat(strrep("-", 68L), "\n", sep = "")
    for (index in seq_len(nrow(timings))) {
      entry <- timings[index, ]
      result <- if (identical(entry$status, "failed")) {
        "FAILED"
      } else if (identical(entry$status, "skipped")) {
        "skipped"
      } else {
        entry$cache_status
      }
      cat(sprintf("%-44s %8.2f  %s\n", entry$step, entry$elapsed_minutes, result))
      if (!is.na(entry$cache_source) && nzchar(entry$cache_source)) {
        cat("  Cache source run: ", entry$cache_source, "\n", sep = "")
      }
    }
  } else {
    cat("No completed steps were recorded.\n")
  }
  cat(sprintf("Total elapsed time: %.2f minutes\n", run_elapsed_seconds() / 60))
  invisible(timings)
}

notify_run_finished <- function() {
  # RStudio commonly ignores the ASCII terminal bell. Prefer a native sound
  # player where one is available, then fall back to package/console methods.
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    player <- Sys.which("afplay")
    sound <- "/System/Library/Sounds/Glass.aiff"
    if (nzchar(player) && file.exists(sound)) {
      result <- try(system2(player, sound, stdout = FALSE, stderr = FALSE, wait = FALSE),
                    silent = TRUE)
      if (!inherits(result, "try-error")) return(invisible(TRUE))
    }
  }
  if (requireNamespace("beepr", quietly = TRUE)) {
    result <- try(beepr::beep(), silent = TRUE)
    if (!inherits(result, "try-error")) return(invisible(TRUE))
  }
  if (.Platform$OS.type == "windows") {
    result <- try(utils::alarm(), silent = TRUE)
    if (!inherits(result, "try-error")) return(invisible(TRUE))
  }
  cat("\a")
  flush.console()
  invisible(FALSE)
}
