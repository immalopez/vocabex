rq1_influence_cache_version <- 1L

rq1_model_cache_signature <- function(model) {
  identity <- attr(model, "vocabex_run")
  if (!is.null(identity)) {
    return(list(
      model_id = identity$model_id,
      data_sha256 = identity$data_sha256,
      specification_sha256 = identity$specification_sha256
    ))
  }

  formula <- stats::formula(model)
  environment(formula) <- emptyenv()
  list(
    formula = paste(deparse(formula, width.cutoff = 500L), collapse = " "),
    family = stats::family(model)[c("family", "link")],
    model_frame_sha256 = digest::digest(stats::model.frame(model), algo = "sha256"),
    contrasts = attr(stats::model.matrix(model), "contrasts"),
    optimizer = model@optinfo$optimizer,
    control = model@optinfo$control
  )
}

rq1_influence_cache_key <- function(recognition_model, correction_model,
                                    coef_name = "dose_WP_sc",
                                    item_sample_size = 100L, seed = 1L) {
  digest::digest(
    list(
      cache_version = rq1_influence_cache_version,
      lme4_version = as.character(utils::packageVersion("lme4")),
      recognition_model = rq1_model_cache_signature(recognition_model),
      correction_model = rq1_model_cache_signature(correction_model),
      coefficient = coef_name,
      item_sample_size = as.integer(item_sample_size),
      seed = as.integer(seed)
    ),
    algo = "sha256"
  )
}

read_matching_influence_cache <- function(path, cache_key) {
  if (!file.exists(path)) return(NULL)
  result <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(result) || !identical(result$cache_key, cache_key)) return(NULL)
  result
}

manifest_records_diagnostic <- function(run_directory, relative_path) {
  manifest_path <- file.path(run_directory, "manifest.json")
  if (!file.exists(manifest_path)) return(FALSE)
  manifest <- tryCatch(
    jsonlite::read_json(manifest_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(manifest) || !identical(manifest$status, "complete")) return(FALSE)

  records <- manifest$diagnostics
  if (is.null(records)) return(FALSE)
  any(vapply(records, function(record) {
    identical(record$path, relative_path) &&
      identical(
        record$sha256,
        digest::digest(
          file.path(run_directory, relative_path),
          algo = "sha256", file = TRUE, serialize = FALSE
        )
      )
  }, logical(1)))
}

find_rq1_influence_cache <- function(cache_key, active_run_dir = NULL,
                                     standalone_path = "rq1_influence_checks.rds") {
  if (!is.null(active_run_dir) && nzchar(active_run_dir)) {
    active_run_dir <- normalizePath(active_run_dir, mustWork = FALSE)
    parent <- dirname(active_run_dir)
    runs_root <- if (identical(basename(parent), ".incomplete")) dirname(parent) else parent
    completed_runs <- list.dirs(runs_root, recursive = FALSE, full.names = TRUE)
    completed_runs <- completed_runs[basename(completed_runs) != ".incomplete"]
    completed_runs <- completed_runs[order(basename(completed_runs), decreasing = TRUE)]

    relative_path <- file.path("diagnostics", "rq1_influence_checks.rds")
    for (candidate_run in completed_runs) {
      candidate_path <- file.path(candidate_run, relative_path)
      result <- read_matching_influence_cache(candidate_path, cache_key)
      if (!is.null(result) && manifest_records_diagnostic(candidate_run, relative_path)) {
        return(list(result = result, source = basename(candidate_run)))
      }
    }
    return(NULL)
  }

  result <- read_matching_influence_cache(standalone_path, cache_key)
  if (is.null(result)) NULL else list(result = result, source = normalizePath(standalone_path))
}

loo_by_group <- function(model, data, group_var, coef_name) {
  groups <- levels(droplevels(data[[group_var]]))

  results <- future.apply::future_lapply(
    groups,
    function(group) {
      options(contrasts = c("contr.sum", "contr.poly"))
      reduced_data <- data[data[[group_var]] != group, , drop = FALSE]
      fit <- update(model, data = reduced_data)
      c(
        group = group,
        beta = lme4::fixef(fit)[[coef_name]],
        logLik = as.numeric(stats::logLik(fit))
      )
    },
    future.seed = TRUE,
    future.packages = "lme4"
  )

  output <- as.data.frame(do.call(rbind, results), stringsAsFactors = FALSE)
  output$beta <- as.numeric(output$beta)
  output$logLik <- as.numeric(output$logLik)
  output$delta_beta <- output$beta - lme4::fixef(model)[[coef_name]]
  output[order(-abs(output$delta_beta)), ]
}

loo_by_item_sample <- function(model, data, item_ids, coef_name) {
  results <- future.apply::future_lapply(
    item_ids,
    function(item_id) {
      options(contrasts = c("contr.sum", "contr.poly"))
      fit <- update(model, data = data[data$id != item_id, , drop = FALSE])
      c(id = item_id, beta = lme4::fixef(fit)[[coef_name]])
    },
    future.seed = TRUE,
    future.packages = "lme4"
  )

  output <- as.data.frame(do.call(rbind, results), stringsAsFactors = FALSE)
  output$beta <- as.numeric(output$beta)
  output$delta_beta <- output$beta - lme4::fixef(model)[[coef_name]]
  output[order(-abs(output$delta_beta)), ]
}

summarize_influence_deltas <- function(results) {
  entries <- list(
    recognition_participant = results$loo_rec,
    correction_participant = results$loo_cor,
    recognition_item_sample = results$loo_items_rec,
    correction_item_sample = results$loo_items_cor
  )

  dplyr::bind_rows(lapply(names(entries), function(check) {
    delta <- entries[[check]]$delta_beta
    tibble::tibble(
      check = check,
      refits = length(delta),
      min_delta_beta = min(delta),
      max_delta_beta = max(delta),
      max_abs_delta_beta = max(abs(delta)),
      sd_delta_beta = stats::sd(delta)
    )
  }))
}

run_rq1_influence_checks <- function(recognition_model, correction_model,
                                     recognition_data, correction_data,
                                     coef_name = "dose_WP_sc",
                                     item_sample_size = 100L,
                                     seed = 1L,
                                     workers = NULL) {
  if (is.null(workers)) {
    available <- parallel::detectCores(logical = TRUE)
    if (is.na(available)) available <- 2L
    workers <- min(8L, max(2L, available - 1L))
  }

  previous_plan <- future::plan()
  on.exit(future::plan(previous_plan), add = TRUE)
  future::plan(future::multisession, workers = workers)

  loo_rec <- loo_by_group(
    recognition_model, recognition_data, "participant_ID", coef_name
  )
  loo_cor <- loo_by_group(
    correction_model, correction_data, "participant_ID", coef_name
  )

  set.seed(seed)
  recognition_ids <- levels(droplevels(recognition_data$id))
  correction_ids <- levels(droplevels(correction_data$id))
  sample_ids_rec <- sample(
    recognition_ids, min(item_sample_size, length(recognition_ids))
  )
  sample_ids_cor <- sample(
    correction_ids, min(item_sample_size, length(correction_ids))
  )

  loo_items_rec <- loo_by_item_sample(
    recognition_model, recognition_data, sample_ids_rec, coef_name
  )
  loo_items_cor <- loo_by_item_sample(
    correction_model, correction_data, sample_ids_cor, coef_name
  )

  results <- list(
    loo_rec = loo_rec,
    loo_cor = loo_cor,
    loo_items_rec = loo_items_rec,
    loo_items_cor = loo_items_cor,
    sample_ids_rec = sample_ids_rec,
    sample_ids_cor = sample_ids_cor,
    settings = list(
      coefficient = coef_name,
      item_sample_size = item_sample_size,
      seed = seed,
      workers = workers
    )
  )
  results$summary <- summarize_influence_deltas(results)
  results
}
