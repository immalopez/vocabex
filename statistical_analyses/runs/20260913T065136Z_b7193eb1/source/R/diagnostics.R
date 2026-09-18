# Diagnostics only: these helpers never fit a model or change posterior draws.
resolve_max_treedepth <- function(fit, max_treedepth = NULL) {
  recorded <- attr(fit, "diagnostic_control")$max_treedepth
  if (is.null(recorded) && inherits(fit$fit, "CmdStanMCMC")) {
    recorded <- fit$fit$metadata()$max_treedepth
  }
  if (is.null(recorded) && inherits(fit$fit, "stanfit")) {
    recorded <- unlist(lapply(fit$fit@stan_args, function(x) {
      if (!is.null(x$control$max_treedepth)) x$control$max_treedepth else x$max_depth
    }))
  }
  recorded <- unique(recorded)
  if (length(recorded) > 1L) stop("Inconsistent recorded treedepth limits across chains.")
  if (length(recorded) && !is.null(max_treedepth) &&
      !identical(as.numeric(recorded), as.numeric(max_treedepth))) {
    stop("Explicit treedepth limit disagrees with the saved fit.")
  }
  limit <- if (length(recorded)) recorded else max_treedepth
  if (is.null(limit)) {
    stop("No recorded max_treedepth: supply the actual fitting limit explicitly.")
  }
  if (length(limit) != 1L || !is.finite(limit) || limit < 1 || limit != floor(limit)) {
    stop("max_treedepth must be one positive integer.")
  }
  as.integer(limit)
}

summarize_sampler_diagnostics <- function(sampler, draws_summary, max_treedepth) {
  stopifnot(length(max_treedepth) == 1L, is.finite(max_treedepth),
            max_treedepth >= 1, max_treedepth == floor(max_treedepth))
  required <- c("variable", "rhat", "ess_bulk", "ess_tail")
  if (!all(required %in% names(draws_summary))) stop("Incomplete posterior summary columns.")
  if (!all(c("Parameter", "Value") %in% names(sampler))) stop("Invalid sampler table.")
  diagnostic <- function(name, predicate) {
    values <- sampler$Value[sampler$Parameter == name]
    if (!length(values) || any(!is.finite(values))) return(NA_integer_)
    sum(predicate(values))
  }
  extreme <- function(x, fun) {
    if (!length(x) || any(!is.finite(x))) return(NA_real_)
    fun(x)
  }
  ds <- as.data.frame(draws_summary)
  ds$diagnostic_status <- vapply(seq_len(nrow(ds)), function(i) {
    x <- as.numeric(ds[i, c("rhat", "ess_bulk", "ess_tail")])
    if (any(!is.finite(x))) return("unavailable")
    if (x[1] > 1.01 || x[2] < 400 || x[3] < 400) return("review")
    "ok"
  }, character(1))
  divergences <- diagnostic("divergent__", function(x) x > 0)
  hits <- diagnostic("treedepth__", function(x) x >= max_treedepth)
  unavailable <- any(ds$diagnostic_status == "unavailable") ||
    is.na(divergences) || is.na(hits) || !nrow(ds)
  review <- any(ds$diagnostic_status == "review") ||
    isTRUE(divergences > 0) || isTRUE(hits > 0)
  # Thresholds flag investigation, not proof of convergence or model validity.
  list(
    sampler_summary = data.frame(
      max_treedepth = max_treedepth,
      divergences = divergences,
      treedepth_hits = hits,
      max_rhat = extreme(ds$rhat, max),
      min_bulk_ess = extreme(ds$ess_bulk, min),
      min_tail_ess = extreme(ds$ess_tail, min),
      unavailable_parameters = sum(ds$diagnostic_status == "unavailable"),
      status = if (unavailable) "incomplete" else if (review) "review" else "ok"
    ),
    problematic_parameters = ds[ds$diagnostic_status != "ok", , drop = FALSE]
  )
}

calibration_bins <- function(predictions, observed, bins = 10L) {
  if (!is.matrix(predictions) || !is.numeric(predictions) ||
      nrow(predictions) < 2L || !ncol(predictions)) {
    stop("predictions must be a numeric draws-by-observations matrix with >= 2 draws.")
  }
  if (length(observed) != ncol(predictions) || anyNA(observed) ||
      !all(observed %in% c(0, 1))) stop("observed must contain aligned binary outcomes.")
  if (any(!is.finite(predictions)) || any(predictions < 0 | predictions > 1)) {
    stop("Predictions must be finite probabilities.")
  }
  if (length(bins) != 1L || !is.finite(bins) || bins < 1 || bins != floor(bins)) {
    stop("bins must be one positive integer.")
  }
  fitted <- colMeans(predictions)
  breaks <- unique(as.numeric(stats::quantile(fitted, seq(0, 1, length.out = bins + 1L))))
  # Ties may produce fewer bins; keep tied predictions together and include both endpoints.
  membership <- if (length(breaks) == 1L) rep(1L, length(fitted)) else
    as.integer(cut(fitted, breaks, include.lowest = TRUE, right = TRUE))
  if (anyNA(membership)) stop("Calibration binning lost observations.")
  result <- do.call(rbind, lapply(sort(unique(membership)), function(bin) {
    idx <- which(membership == bin)
    draw_means <- rowMeans(predictions[, idx, drop = FALSE])
    ci <- stats::quantile(draw_means, c(0.025, 0.975), names = FALSE)
    data.frame(bin = bin, n = length(idx), p_hat = mean(draw_means),
               p_obs = mean(observed[idx]), lo = ci[1], hi = ci[2])
  }))
  stopifnot(sum(result$n) == length(observed))
  result
}

plot_calibration <- function(bins) {
  ggplot2::ggplot(bins, ggplot2::aes(x = p_hat, y = p_obs)) +
    ggplot2::geom_abline(linetype = 2, linewidth = 0.5) +
    ggplot2::geom_segment(ggplot2::aes(x = lo, xend = hi, yend = p_obs)) +
    ggplot2::geom_point(ggplot2::aes(size = n)) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::scale_size_continuous(range = c(2, 6)) +
    ggplot2::labs(
      x = "Mean predicted probability", y = "Observed frequency", size = "Observations",
      title = "In-sample calibration (binned)",
      subtitle = "Horizontal bars: 95% credible intervals for mean predicted probability"
    )
}

bayesian_diagnostics <- function(fit, observed, max_treedepth = NULL, bins = 10L) {
  limit <- resolve_max_treedepth(fit, max_treedepth)
  # brms uses the same long sampler format for both supported backends.
  sampler <- brms::nuts_params(fit, inc_warmup = FALSE)
  draws_summary <- posterior::summarise_draws(posterior::as_draws_array(fit))
  result <- summarize_sampler_diagnostics(sampler, draws_summary, limit)
  result$calibration_bins <- calibration_bins(brms::posterior_epred(fit), observed, bins)
  result$calibration_plot <- plot_calibration(result$calibration_bins)
  result
}

inspect_high_k_observations <- function(fit, loo_result, threshold = 0.7) {
  if (!inherits(loo_result, "loo")) stop("loo_result must inherit from 'loo'.")
  if (length(threshold) != 1L || !is.finite(threshold)) {
    stop("threshold must be one finite number.")
  }
  data <- model.frame(fit)
  pareto_k <- loo::pareto_k_values(loo_result)
  if (length(pareto_k) != nrow(data)) stop("LOO diagnostics are not aligned with the model frame.")
  index <- which(pareto_k > threshold)
  if (!length(index)) return(data.frame())

  response_formula <- if (inherits(fit, "brmsfit")) fit$formula$formula else stats::formula(fit)
  response_name <- all.vars(response_formula)[1]
  if (!response_name %in% names(data)) stop("Could not identify the model response.")
  observed <- data[[response_name]]
  predictions <- brms::posterior_epred(fit)
  if (ncol(predictions) != nrow(data)) stop("Posterior predictions are not aligned with the model frame.")
  fitted_probability <- colMeans(predictions)
  observed_probability <- ifelse(observed == 1, fitted_probability, 1 - fitted_probability)

  value_or_na <- function(name) {
    if (name %in% names(data)) as.character(data[[name]][index]) else rep(NA_character_, length(index))
  }
  group_count <- function(name) {
    if (!name %in% names(data)) return(rep(NA_integer_, length(index)))
    as.integer(table(data[[name]])[as.character(data[[name]][index])])
  }
  numeric_data <- data[vapply(data, is.numeric, logical(1))]
  numeric_data <- numeric_data[setdiff(names(numeric_data), response_name)]
  max_abs_scaled_predictor <- if (!length(numeric_data)) rep(NA_real_, nrow(data)) else {
    scaled <- vapply(numeric_data, function(x) {
      sx <- stats::sd(x)
      if (!is.finite(sx) || sx == 0) rep(NA_real_, length(x)) else abs((x - mean(x)) / sx)
    }, numeric(nrow(data)))
    apply(scaled, 1, max, na.rm = TRUE)
  }

  data.frame(
    observation = index,
    pareto_k = unname(pareto_k[index]),
    response = observed[index],
    fitted_probability = fitted_probability[index],
    observed_probability = observed_probability[index],
    surprising_outcome = observed_probability[index] < 0.20,
    time = value_or_na("time"),
    participant_ID = value_or_na("participant_ID"),
    participant_n = group_count("participant_ID"),
    id = value_or_na("id"),
    item_n = group_count("id"),
    error_type = value_or_na("error_type"),
    error_type_n = group_count("error_type"),
    max_abs_scaled_predictor = max_abs_scaled_predictor[index],
    stringsAsFactors = FALSE
  )
}

summarize_exact_loo <- function(approximate, exact, threshold = 0.7) {
  if (!inherits(approximate, "loo") || !inherits(exact, "loo")) {
    stop("approximate and exact must inherit from 'loo'.")
  }
  approximate_k <- loo::pareto_k_values(approximate)
  exact_k <- loo::pareto_k_values(exact)
  data.frame(
    method = c("Moment-matched PSIS-LOO", "PSIS-LOO with exact refits"),
    looic = c(approximate$estimates["looic", "Estimate"], exact$estimates["looic", "Estimate"]),
    looic_se = c(approximate$estimates["looic", "SE"], exact$estimates["looic", "SE"]),
    p_loo = c(approximate$estimates["p_loo", "Estimate"], exact$estimates["p_loo", "Estimate"]),
    psis_over_threshold = c(sum(approximate_k > threshold, na.rm = TRUE),
                            sum(exact_k > threshold, na.rm = TRUE)),
    exact_refits = c(0L, sum(approximate_k > threshold, na.rm = TRUE)),
    unresolved = c(sum(approximate_k > threshold, na.rm = TRUE),
                   sum(exact_k > threshold, na.rm = TRUE))
  )
}

exact_reloo <- function(fit, approximate, threshold = 0.7, cores = 4L,
                        backend = "cmdstanr", future_globals_gib = 2) {
  if (!inherits(approximate, "loo")) stop("approximate must inherit from 'loo'.")
  if (length(future_globals_gib) != 1L || !is.finite(future_globals_gib) ||
      future_globals_gib <= 0) stop("future_globals_gib must be positive.")
  old <- options(future.globals.maxSize = future_globals_gib * 1024^3)
  on.exit(options(old), add = TRUE)
  brms::reloo(
    fit, loo = approximate, k_threshold = threshold,
    backend = backend, cores = cores
  )
}
