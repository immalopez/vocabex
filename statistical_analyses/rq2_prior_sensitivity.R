library(tidyverse)
library(lme4)
library(brms)
library(cmdstanr)
library(posterior)
library(readr)

options(contrasts = c("contr.sum", "contr.poly"))

source("R/run_registry.R")
source("R/diagnostics.R")

# Hold the computational specification fixed across the two prior models so
# that priors are the only modeling choice varied by this sensitivity analysis.
rq2_sensitivity_fit_settings <- list(
  backend = "cmdstanr",
  chains = 4L,
  cores = 4L,
  threads_per_chain = 6L,
  iter = 2000L,
  warmup = 1000L,
  control = list(metric = "diag_e", adapt_delta = 0.99, max_treedepth = 12L),
  refresh = 50L,
  save_pars = "all"
)
rq2_sensitivity_seeds <- c(glmm_informed = 202502L, generic_weak = 202502L)
rq2_sensitivity_diagnostic_seed <- 202503L
rq2_sensitivity_loo_seed <- 202504L
rq2_exact_loo_enabled <- !identical(Sys.getenv("VOCABEX_SKIP_EXACT_LOO"), "true")
rq2_sensitivity_loo_method <- if (rq2_exact_loo_enabled) {
  "Moment-matched PSIS-LOO with exact refits for k > 0.7"
} else {
  "Moment-matched PSIS-LOO (exact refits skipped)"
}

df <- read_csv(run_input("data_for_analysis_pca.csv"), show_col_types = FALSE)

df <- df %>%
  mutate(
    gram_primary = case_when(
      n_gramcats == 1 ~ gram_primary,
      n_gramcats > 1 ~ "Mixed"
    )
  )

df$error_type[as.character(df$id) %in% c("440", "583")] <- "Synthesis"
df$error_type[as.character(df$id) == "904"] <- "Order"

df <- df %>%
  mutate(
    id = factor(id),
    participant_ID = factor(participant_ID),
    error_type = factor(error_type)
  )

df_clean <- df %>%
  dplyr::select(
    id, participant_ID, text_file, prof_level = level, error_type, exercise_type,
    exercise_format, complete, exercise_score, recognition_imm, correction_imm,
    overall_score_imm, recognition_del, correction_del, overall_score_del,
    time_invested, goal_productive_use_of_targets, goal_meaning_and_sense_elaboration,
    goal_contextual_retrieval, goal_paraphrastic_flexibility,
    goal_conceptual_organization, goal_pragmatic_appropriateness,
    goal_collocational_control, fmt_discrete_choice, fmt_sentence_reconstruction,
    fmt_matching, fmt_prompted_writing, fmt_metalinguistic_reflection, fmt_cloze,
    fmt_accent_marking_and_read_aloud, fmt_text_editing, fmt_paradigmatic_systematization,
    fmt_vocabulary_extraction_and_list_creation, fmt_translation, fmt_syntagmatic_systematization,
    n_goals, n_formats, gram_cat_clean, gram_primary, gram_Preposition, gram_Adjective, gram_Adverb,
    gram_Noun, `gram_Conjunctive phrase`, gram_Verb, gram_Pronoun, gram_Determiner,
    n_gramcats, il_22, il_need_motivation, il_noticing, il_search, il_retrieval,
    il_evaluation_generation, il_retention, Soph_PC1_ex_z, Soph_PC1_imm_z,
    Soph_PC1_del_z, LexDiv_PC1_ex_z, LexDiv_PC1_imm_z, LexDiv_PC1_del_z,
    POS_PC1_ex_z, POS_PC1_imm_z, POS_PC1_del_z, POS_PC2_ex_z, POS_PC2_imm_z,
    POS_PC2_del_z, Tree_PC1_ex_z, Tree_PC1_imm_z, Tree_PC1_del_z, Total_z,
    Total_imm_z, Total_del_z
  ) %>%
  mutate(
    prof_level = relevel(factor(prof_level, ordered = FALSE), ref = "Heritage"),
    gram_primary = as.factor(gram_primary)
  )

ti_il_raw <- c("il_retrieval", "il_evaluation_generation", "il_noticing", "il_retention", "il_search")
le_il_raw <- "il_need_motivation"

df_clean <- df_clean %>%
  mutate(
    ti_il = rowSums(pick(all_of(ti_il_raw)), na.rm = FALSE),
    le_il = rowSums(pick(all_of(le_il_raw)), na.rm = FALSE),
    ti_il_z = as.numeric(scale(ti_il)),
    le_il_z = as.numeric(scale(le_il))
  )

make_long <- function(data, target = c("recognition", "correction")) {
  target <- match.arg(target)
  wide <- paste0(target, c("_imm", "_del"))

  data %>%
    transmute(
      id, participant_ID, prof_level, error_type, complete, exercise_score, ti_il_z,
      Soph_ex = Soph_PC1_ex_z, LexDiv_ex = LexDiv_PC1_ex_z, POS1_ex = POS_PC1_ex_z,
      POS2_ex = POS_PC2_ex_z, Tree_ex = Tree_PC1_ex_z, Total_ex = Total_z,
      Soph_imm = Soph_PC1_imm_z, LexDiv_imm = LexDiv_PC1_imm_z,
      POS1_imm = POS_PC1_imm_z, POS2_imm = POS_PC2_imm_z,
      Tree_imm = Tree_PC1_imm_z, Total_imm = Total_imm_z,
      Soph_del = Soph_PC1_del_z, LexDiv_del = LexDiv_PC1_del_z,
      POS1_del = POS_PC1_del_z, POS2_del = POS_PC2_del_z,
      Tree_del = Tree_PC1_del_z, Total_del = Total_del_z, le_il_z,
      !!sym(wide[1]), !!sym(wide[2])
    ) %>%
    pivot_longer(all_of(wide), names_to = "time", values_to = target) %>%
    mutate(
      time = factor(if_else(grepl("imm", time), "imm", "del"), levels = c("imm", "del")),
      Soph_post = if_else(time == "imm", Soph_imm, Soph_del),
      LexDiv_post = if_else(time == "imm", LexDiv_imm, LexDiv_del),
      POS1_post = if_else(time == "imm", POS1_imm, POS1_del),
      POS2_post = if_else(time == "imm", POS2_imm, POS2_del),
      Tree_post = if_else(time == "imm", Tree_imm, Tree_del),
      Total_post = if_else(time == "imm", Total_imm, Total_del)
    ) %>%
    mutate(
      across(
        c(Soph_ex, LexDiv_ex, POS1_ex, POS2_ex, Tree_ex, Total_ex,
          Soph_post, LexDiv_post, POS1_post, POS2_post, Tree_post, Total_post,
          exercise_score),
        ~ as.numeric(scale(.x)),
        .names = "{.col}_sc"
      )
    )
}

df_cor <- make_long(df_clean, "correction") %>%
  group_by(participant_ID) %>%
  mutate(
    dose_BP_sc = mean(exercise_score_sc, na.rm = TRUE),
    dose_WP_sc = exercise_score_sc - dose_BP_sc
  ) %>%
  ungroup()

m_cor <- glmer(
  correction ~
    time * ti_il_z +
    time * prof_level +
    time * dose_WP_sc + dose_BP_sc +
    error_type + (1 + time | participant_ID) + (1 | id),
  data = df_cor, family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

df_brm_cor <- df_clean

to_pm05 <- function(x) if (is.numeric(x)) x - 0.5 else x
goal_vars <- grep("^goal_", names(df_clean), value = TRUE)
fmt_vars <- grep("^fmt_", names(df_clean), value = TRUE)
vars <- union(goal_vars, fmt_vars)

df_brm_cor <- df_brm_cor |>
  mutate(across(all_of(vars), ~ as.numeric(.), .names = "{.col}")) |>
  mutate(across(all_of(vars), to_pm05))

df_brm_cor <- df_brm_cor %>%
  left_join(
    df_cor %>% dplyr::select(
      id, time, dose_BP_sc, dose_WP_sc, correction, Soph_post_sc,
      LexDiv_post_sc, POS1_post_sc, POS2_post_sc, Tree_post_sc, Total_post_sc,
      Soph_ex_sc, LexDiv_ex_sc, POS1_ex_sc, POS2_ex_sc, Tree_ex_sc, Total_ex_sc
    ),
    by = "id"
  ) %>%
  distinct()

if (run_is_active()) {
  df_brm_cor <- load_run_data("rq2_primary")
}

f2 <- bf(
  correction ~
    time +
    ti_il_z +
    prof_level +
    Soph_post_sc +
    LexDiv_post_sc +
    POS1_post_sc +
    POS2_post_sc +
    Tree_post_sc +
    Total_post_sc +
    goal_productive_use_of_targets +
    goal_meaning_and_sense_elaboration +
    goal_contextual_retrieval +
    goal_paraphrastic_flexibility +
    goal_conceptual_organization +
    goal_pragmatic_appropriateness +
    goal_collocational_control +
    fmt_discrete_choice +
    fmt_sentence_reconstruction +
    fmt_matching +
    fmt_prompted_writing +
    fmt_metalinguistic_reflection +
    fmt_cloze +
    fmt_accent_marking_and_read_aloud +
    fmt_text_editing +
    fmt_paradigmatic_systematization +
    fmt_vocabulary_extraction_and_list_creation +
    fmt_translation +
    fmt_syntagmatic_systematization +
    Soph_ex_sc + LexDiv_ex_sc + POS1_ex_sc + POS2_ex_sc + Tree_ex_sc + Total_ex_sc +
    (1 | participant_ID) +
    (1 | id) +
    (1 + time +
       fmt_sentence_reconstruction + fmt_matching +
       fmt_text_editing + fmt_translation +
       fmt_prompted_writing + fmt_metalinguistic_reflection +
       goal_paraphrastic_flexibility +
       goal_meaning_and_sense_elaboration +
       goal_conceptual_organization || error_type),
  family = bernoulli(link = "logit"),
  decomp = "QR"
)

add_if_present <- function(gp, class, prior_str, coef = NULL, group = NULL) {
  m <- gp$class == class
  if (!is.null(coef)) m <- m & (gp$coef %in% coef)
  if (!is.null(group)) m <- m & (gp$group %in% group)
  hits <- gp[m, , drop = FALSE]
  if (!nrow(hits)) return(NULL)
  Reduce(c, lapply(seq_len(nrow(hits)), function(i) {
    set_prior(
      prior_str,
      class = hits$class[i],
      coef = if (nzchar(hits$coef[i])) hits$coef[i],
      group = if (nzchar(hits$group[i])) hits$group[i]
    )
  }))
}

gp2 <- get_prior(f2, data = df_brm_cor)

fe <- fixef(m_cor)
se <- sqrt(diag(vcov(m_cor)))
vc <- VarCorr(m_cor)

inflate <- 2
b_names <- subset(gp2, class == "b" & nzchar(coef))$coef
shared <- intersect(setdiff(names(fe), "(Intercept)"), b_names)

pri_shared_list <- lapply(shared, function(nm) {
  mu <- unname(fe[[nm]])
  s <- unname(se[[nm]]) * inflate
  set_prior(sprintf("normal(%0.6f,%0.6f)", mu, s), class = "b", coef = nm)
})
pri_shared <- if (length(pri_shared_list)) do.call(c, pri_shared_list) else NULL

pri_int <- set_prior(sprintf("student_t(3,%0.6f,2)", unname(fe[["(Intercept)"]])), class = "Intercept")

sd_id_int <- tryCatch(attr(vc$id, "stddev")[["(Intercept)"]], error = function(e) NA_real_)
sd_p_int <- tryCatch(attr(vc$participant_ID, "stddev")[["(Intercept)"]], error = function(e) NA_real_)

if (is.finite(sd_id_int)) {
  pri_sd_id <- set_prior(sprintf("exponential(%0.6f)", 1 / sd_id_int), class = "sd", group = "id", coef = "Intercept")
} else {
  pri_sd_id <- set_prior("exponential(1.5)", class = "sd", group = "id", coef = "Intercept")
}

if (is.finite(sd_p_int)) {
  pri_sd_pid <- set_prior(sprintf("exponential(%0.6f)", 1 / sd_p_int), class = "sd", group = "participant_ID", coef = "Intercept")
} else {
  pri_sd_pid <- set_prior("exponential(1.5)", class = "sd", group = "participant_ID", coef = "Intercept")
}

pri_sd_err <- set_prior("exponential(2.0)", class = "sd", group = "error_type", coef = "Intercept")
pri_global_b <- prior(normal(0, 1), class = "b")

error_type_slope_coefs <- c(
  "time1", "fmt_sentence_reconstruction", "fmt_matching", "fmt_text_editing",
  "fmt_translation", "fmt_metalinguistic_reflection", "fmt_prompted_writing",
  "goal_paraphrastic_flexibility", "goal_meaning_and_sense_elaboration",
  "goal_conceptual_organization"
)

pri_err_slopes_informed <- add_if_present(
  gp2, class = "sd", prior_str = "exponential(3)",
  coef = error_type_slope_coefs, group = "error_type"
)

pri2_informed <- do.call(c, Filter(Negate(is.null), list(
  pri_int, pri_shared, pri_sd_id, pri_sd_pid, pri_sd_err, pri_global_b, pri_err_slopes_informed
)))

make_generic_priors <- function(gp, err_slope_coefs) {
  pri_int_generic <- set_prior("student_t(3, 0, 2.5)", class = "Intercept")
  pri_b_generic <- prior(normal(0, 1), class = "b")
  pri_sd_generic <- prior(exponential(1), class = "sd")
  pri_err_slopes_generic <- add_if_present(
    gp, class = "sd", prior_str = "exponential(1)",
    coef = err_slope_coefs, group = "error_type"
  )
  do.call(c, Filter(Negate(is.null), list(
    pri_int_generic, pri_b_generic, pri_sd_generic, pri_err_slopes_generic
  )))
}

pri2_generic <- make_generic_priors(gp2, error_type_slope_coefs)

fit_brm_rq2 <- function(formula, data, priors, seed) {
  fit <- brm(
    formula, data = data, prior = priors,
    backend = rq2_sensitivity_fit_settings$backend, seed = seed,
    chains = rq2_sensitivity_fit_settings$chains,
    cores = rq2_sensitivity_fit_settings$cores,
    threads = threading(rq2_sensitivity_fit_settings$threads_per_chain),
    iter = rq2_sensitivity_fit_settings$iter,
    warmup = rq2_sensitivity_fit_settings$warmup,
    control = rq2_sensitivity_fit_settings$control,
    refresh = rq2_sensitivity_fit_settings$refresh,
    save_pars = save_pars(all = TRUE)
  )
  attr(fit, "diagnostic_control") <- rq2_sensitivity_fit_settings$control
  fit
}

if (run_is_active()) {
  fit2_informed <- load_run_fit(
    "rq2_primary", "rq2_primary.rds", expected_data = df_brm_cor, expected_formula = f2
  )
} else {
  warning("No active run: refitting both sensitivity models; use run_analysis.R for managed provenance")
  fit2_informed <- fit_brm_rq2(
    f2, df_brm_cor, pri2_informed, seed = rq2_sensitivity_seeds[["glmm_informed"]]
  )
  saveRDS(fit2_informed, "rq2_fit_informed.rds")
}

fit2_generic <- cached_run_fit(
  "RQ2 generic-prior Bayesian fit", "rq2_generic_sensitivity",
  data = df_brm_cor, formula = f2,
  family = list(family = "bernoulli", link = "logit"),
  priors = pri2_generic, seed = rq2_sensitivity_seeds[["generic_weak"]],
  settings = rq2_sensitivity_fit_settings,
  packages = c("brms", "cmdstanr", "posterior"),
  fit_function = function() fit_brm_rq2(
    f2, df_brm_cor, pri2_generic, seed = rq2_sensitivity_seeds[["generic_weak"]]
  ),
  filename = "rq2_generic_sensitivity.rds"
)

run_sensitivity_diagnostics <- function(fit, label, model_id) {
  set.seed(rq2_sensitivity_diagnostic_seed)
  cached_run_rds(
    paste("RQ2 sensitivity diagnostics:", label),
    paste0(model_id, "_diagnostics"),
    file.path("diagnostics", paste0(model_id, "_diagnostics.rds")),
    inputs = list(model = run_fit_signature(fit)),
    settings = list(seed = rq2_sensitivity_diagnostic_seed),
    packages = c("rstan", "posterior"), source_models = model_id,
    compute_function = function() bayesian_diagnostics(
      fit, observed = model.frame(fit)$correction, bins = 10L
    )
  )
}

rq2_sensitivity_diagnostics <- list(
  glmm_informed = run_sensitivity_diagnostics(
    fit2_informed, "glmm_informed", "rq2_primary"
  ),
  generic_weak = run_sensitivity_diagnostics(
    fit2_generic, "generic_weak", "rq2_generic_sensitivity"
  )
)

compare_brm_fixed_effects <- function(fit_a, fit_b, label_a = "informed", label_b = "generic") {
  draws_a <- as_draws_df(fit_a) |> summarise_draws()
  draws_b <- as_draws_df(fit_b) |> summarise_draws()

  sum_a <- draws_a |>
    filter(grepl("^b_", variable)) |>
    transmute(term = sub("^b_", "", variable), mean_a = mean, q5_a = q5, q95_a = q95)

  sum_b <- draws_b |>
    filter(grepl("^b_", variable)) |>
    transmute(term = sub("^b_", "", variable), mean_b = mean, q5_b = q5, q95_b = q95)

  full_join(sum_a, sum_b, by = "term") |>
    mutate(
      delta_mean = mean_b - mean_a,
      or_a = exp(mean_a),
      or_b = exp(mean_b),
      delta_or = or_b - or_a,
      same_sign = sign(mean_a) == sign(mean_b),
      a_excludes_zero = !(q5_a <= 0 & q95_a >= 0),
      b_excludes_zero = !(q5_b <= 0 & q95_b >= 0),
      changed_support = a_excludes_zero != b_excludes_zero,
      model_a = label_a,
      model_b = label_b
    ) |>
    arrange(desc(abs(delta_mean)))
}

compare_brm_fit_stats <- function(fit_a, fit_b, label_a = "informed", label_b = "generic") {
  set.seed(rq2_sensitivity_loo_seed)
  loo_a <- cached_run_rds(
    paste("RQ2 moment-matched sensitivity LOO:", label_a),
    "rq2_primary_loo_moment_matched",
    file.path("diagnostics", "rq2_primary_loo_moment_matched.rds"),
    inputs = list(model = run_fit_signature(fit_a)),
    settings = list(moment_match = TRUE, seed = rq2_sensitivity_loo_seed),
    packages = c("brms", "loo"), source_models = "rq2_primary",
    compute_function = function() loo(fit_a, moment_match = TRUE)
  )
  set.seed(rq2_sensitivity_loo_seed)
  loo_b <- cached_run_rds(
    paste("RQ2 moment-matched sensitivity LOO:", label_b),
    paste0("rq2_sensitivity_loo_", label_b, "_moment_matched"),
    file.path("diagnostics", paste0("rq2_sensitivity_loo_", label_b, "_moment_matched.rds")),
    inputs = list(model = run_fit_signature(fit_b)),
    settings = list(moment_match = TRUE, seed = rq2_sensitivity_loo_seed),
    packages = c("brms", "loo"), source_models = "rq2_generic_sensitivity",
    compute_function = function() loo(fit_b, moment_match = TRUE)
  )
  inspection_a <- inspect_high_k_observations(fit_a, loo_a, threshold = 0.7)
  inspection_b <- inspect_high_k_observations(fit_b, loo_b, threshold = 0.7)
  if (rq2_exact_loo_enabled) {
    set.seed(rq2_sensitivity_loo_seed + 1L)
    loo_a_reported <- cached_run_rds(
      paste("RQ2 exact sensitivity LOO refits:", label_a),
      "rq2_primary_loo_exact",
      file.path("diagnostics", "rq2_primary_loo_exact.rds"),
      inputs = list(model = run_fit_signature(fit_a), approximate_loo = sha256_object(loo_a)),
      settings = list(k_threshold = 0.7, seed = rq2_sensitivity_loo_seed + 1L,
                      backend = "cmdstanr", cores = rq2_sensitivity_fit_settings$cores,
                      future_globals_gib = 2),
      packages = c("brms", "loo"), source_models = "rq2_primary",
      compute_function = function() exact_reloo(
        fit_a, loo_a, cores = rq2_sensitivity_fit_settings$cores
      )
    )
    set.seed(rq2_sensitivity_loo_seed + 1L)
    loo_b_reported <- cached_run_rds(
      paste("RQ2 exact sensitivity LOO refits:", label_b),
      paste0("rq2_sensitivity_loo_", label_b, "_exact"),
      file.path("diagnostics", paste0("rq2_sensitivity_loo_", label_b, "_exact.rds")),
      inputs = list(model = run_fit_signature(fit_b), approximate_loo = sha256_object(loo_b)),
      settings = list(k_threshold = 0.7, seed = rq2_sensitivity_loo_seed + 1L,
                      backend = "cmdstanr", cores = rq2_sensitivity_fit_settings$cores,
                      future_globals_gib = 2),
      packages = c("brms", "loo"), source_models = "rq2_generic_sensitivity",
      compute_function = function() exact_reloo(
        fit_b, loo_b, cores = rq2_sensitivity_fit_settings$cores
      )
    )
  } else {
    warning("Exact RQ2 sensitivity LOO refits were skipped; reported LOO retains high-k observations.")
    loo_a_reported <- loo_a
    loo_b_reported <- loo_b
  }
  vc_a <- VarCorr(fit_a)
  vc_b <- VarCorr(fit_b)

  result <- tibble(
    model = c(label_a, label_b),
    loo_method = rq2_sensitivity_loo_method,
    looic = c(loo_a_reported$estimates["looic", "Estimate"], loo_b_reported$estimates["looic", "Estimate"]),
    looic_se = c(loo_a_reported$estimates["looic", "SE"], loo_b_reported$estimates["looic", "SE"]),
    p_loo = c(loo_a_reported$estimates["p_loo", "Estimate"], loo_b_reported$estimates["p_loo", "Estimate"]),
    pareto_k_initially_over_0_7 = c(nrow(inspection_a), nrow(inspection_b)),
    exact_refits = if (rq2_exact_loo_enabled) c(nrow(inspection_a), nrow(inspection_b)) else c(0L, 0L),
    unresolved_pareto_k_over_0_7 = c(
      sum(loo::pareto_k_values(loo_a_reported) > 0.7, na.rm = TRUE),
      sum(loo::pareto_k_values(loo_b_reported) > 0.7, na.rm = TRUE)
    ),
    max_initial_pareto_k = c(max(loo::pareto_k_values(loo_a)), max(loo::pareto_k_values(loo_b))),
    participant_intercept_sd = c(vc_a$participant_ID$sd["Intercept", "Estimate"], vc_b$participant_ID$sd["Intercept", "Estimate"]),
    item_intercept_sd = c(vc_a$id$sd["Intercept", "Estimate"], vc_b$id$sd["Intercept", "Estimate"]),
    error_type_intercept_sd = c(vc_a$error_type$sd["Intercept", "Estimate"], vc_b$error_type$sd["Intercept", "Estimate"])
  )
  attr(result, "high_k_inspection") <- bind_rows(
    mutate(inspection_a, model = label_a, .before = 1),
    mutate(inspection_b, model = label_b, .before = 1)
  )
  result
}

rq2_prior_sensitivity_fixed <- compare_brm_fixed_effects(
  fit2_informed, fit2_generic,
  label_a = "glmm_informed", label_b = "generic_weak"
)

rq2_prior_sensitivity_fit <- compare_brm_fit_stats(
  fit2_informed, fit2_generic,
  label_a = "glmm_informed", label_b = "generic_weak"
)
rq2_prior_sensitivity_high_k <- attr(rq2_prior_sensitivity_fit, "high_k_inspection")

rq2_prior_sensitivity_diagnostics <- bind_rows(lapply(
  names(rq2_sensitivity_diagnostics),
  function(model) mutate(
    rq2_sensitivity_diagnostics[[model]]$sampler_summary, model = model, .before = 1
  )
))

rq2_prior_sensitivity_calibration <- bind_rows(lapply(
  names(rq2_sensitivity_diagnostics),
  function(model) mutate(
    rq2_sensitivity_diagnostics[[model]]$calibration_bins, model = model, .before = 1
  )
))

rq2_prior_sensitivity_problematic_parameters <- bind_rows(lapply(
  names(rq2_sensitivity_diagnostics),
  function(model) mutate(
    rq2_sensitivity_diagnostics[[model]]$problematic_parameters,
    model = model, .before = 1
  )
))

rq2_prior_sensitivity_settings <- tibble(
  model = names(rq2_sensitivity_seeds),
  prior_specification = c("GLMM-informed hybrid", "generic weakly informative"),
  seed = unname(rq2_sensitivity_seeds),
  backend = rq2_sensitivity_fit_settings$backend,
  chains = rq2_sensitivity_fit_settings$chains,
  cores = rq2_sensitivity_fit_settings$cores,
  threads_per_chain = rq2_sensitivity_fit_settings$threads_per_chain,
  iterations_per_chain = rq2_sensitivity_fit_settings$iter,
  warmup_per_chain = rq2_sensitivity_fit_settings$warmup,
  retained_draws = rq2_sensitivity_fit_settings$chains *
    (rq2_sensitivity_fit_settings$iter - rq2_sensitivity_fit_settings$warmup),
  metric = rq2_sensitivity_fit_settings$control$metric,
  adapt_delta = rq2_sensitivity_fit_settings$control$adapt_delta,
  max_treedepth = rq2_sensitivity_fit_settings$control$max_treedepth,
  loo_method = rq2_sensitivity_loo_method,
  diagnostic_seed = rq2_sensitivity_diagnostic_seed,
  loo_seed = rq2_sensitivity_loo_seed
)

write_csv(rq2_prior_sensitivity_fixed,
          run_output("tables", "rq2_prior_sensitivity_fixed.csv"))
write_csv(rq2_prior_sensitivity_fit,
          run_output("tables", "rq2_prior_sensitivity_fit.csv"))
write_csv(rq2_prior_sensitivity_high_k,
          run_output("tables", "rq2_prior_sensitivity_high_k.csv"))
write_csv(rq2_prior_sensitivity_diagnostics,
          run_output("tables", "rq2_prior_sensitivity_diagnostics.csv"))
write_csv(rq2_prior_sensitivity_calibration,
          run_output("tables", "rq2_prior_sensitivity_calibration.csv"))
write_csv(rq2_prior_sensitivity_problematic_parameters,
          run_output("tables", "rq2_prior_sensitivity_problematic_parameters.csv"))
write_csv(rq2_prior_sensitivity_settings,
          run_output("tables", "rq2_prior_sensitivity_settings.csv"))

print(rq2_prior_sensitivity_fit)
print(rq2_prior_sensitivity_diagnostics)
print(rq2_prior_sensitivity_settings)
print(head(rq2_prior_sensitivity_fixed, 20))
