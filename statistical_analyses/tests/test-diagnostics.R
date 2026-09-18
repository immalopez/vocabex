# Run from repository root: Rscript statistical_analyses/tests/test-diagnostics.R
source("statistical_analyses/R/diagnostics.R")
expect_error <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))
s <- data.frame(Parameter = rep(c("treedepth__", "divergent__"), each = 3),
                Value = c(10, 11, 12, 0, 1, 0))
d <- data.frame(variable = "beta", rhat = 1, ess_bulk = 500, ess_tail = 600)
a <- summarize_sampler_diagnostics(s, d, 12)$sampler_summary
b <- summarize_sampler_diagnostics(s, d, 10)$sampler_summary
stopifnot(a$treedepth_hits == 1, b$treedepth_hits == 3, a$divergences == 1)
missing <- summarize_sampler_diagnostics(s[FALSE, ], d, 12)$sampler_summary
stopifnot(is.na(missing$divergences), missing$status == "incomplete")
d$rhat <- NA_real_
stopifnot(summarize_sampler_diagnostics(s, d, 12)$problematic_parameters$diagnostic_status == "unavailable")
expect_error(resolve_max_treedepth(list(fit = NULL)))
stopifnot(resolve_max_treedepth(list(fit = NULL), 12) == 12)
fake <- structure(list(fit = NULL), diagnostic_control = list(max_treedepth = 12L))
expect_error(resolve_max_treedepth(fake, 10))
expect_error(inspect_high_k_observations(NULL, list()))
expect_error(summarize_exact_loo(list(), list()))

# Known draw-wise bin means: c(.2, .4, .6), not a collapsed scalar.
p <- rbind(c(.1, .3), c(.3, .5), c(.5, .7))
x <- calibration_bins(p, c(0, 1), bins = 1)
stopifnot(isTRUE(all.equal(x$lo, .21)), isTRUE(all.equal(x$hi, .59)), x$n == 2)
# Tied quantile boundaries, singleton bins, and endpoints 0 and 1.
p <- rbind(c(0, .2, .2, .7, 1), c(0, .4, .4, .9, 1))
x <- calibration_bins(p, c(0, 0, 1, 1, 1), bins = 10)
stopifnot(sum(x$n) == 5, !anyNA(x), any(x$p_hat == 0), any(x$p_hat == 1))
x <- calibration_bins(matrix(.5, 3, 4), c(0, 1, 0, 1))
stopifnot(nrow(x) == 1, x$n == 4, x$lo == .5, x$hi == .5)
expect_error(calibration_bins(p, c(0, 1)))
expect_error(calibration_bins(p, c(0, 0, NA, 1, 1)))
cat("Diagnostic unit checks passed.\n")

if ("--saved-fit" %in% commandArgs(trailingOnly = TRUE)) {
  fit_path <- "statistical_analyses/fit2_latest.rds"
  hash_before <- tools::md5sum(fit_path)
  fit <- readRDS(fit_path)
  coefficients_before <- brms::fixef(fit)
  result <- bayesian_diagnostics(fit, model.frame(fit)$correction)
  native <- rstan::get_sampler_params(fit$fit, inc_warmup = FALSE)
  stopifnot(result$sampler_summary$max_treedepth == 12L,
    result$sampler_summary$divergences == sum(vapply(native, function(x) sum(x[, "divergent__"]), numeric(1))),
    result$sampler_summary$treedepth_hits == sum(vapply(native, function(x) sum(x[, "treedepth__"] >= 12), numeric(1))),
    sum(result$calibration_bins$n) == nrow(model.frame(fit)),
    identical(coefficients_before, brms::fixef(fit)),
    identical(hash_before, tools::md5sum(fit_path)))
  # Independent check for the first quantile bin using the saved draws.
  pred <- brms::posterior_epred(fit)
  means <- colMeans(pred)
  boundary <- quantile(means, .1)
  per_draw <- apply(pred[, means <= boundary, drop = FALSE], 1, mean)
  stopifnot(isTRUE(all.equal(result$calibration_bins$lo[1], unname(quantile(per_draw, .025)))),
            isTRUE(all.equal(result$calibration_bins$hi[1], unname(quantile(per_draw, .975)))))
  print(result$sampler_summary)
  # Exercise plotting without modifying publication artifacts.
  ggplot2::ggplot_build(result$calibration_plot)
  cat("Saved-posterior integration checks passed; coefficients and saved fit unchanged.\n")
}
