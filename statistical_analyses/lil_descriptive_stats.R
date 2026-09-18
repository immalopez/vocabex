library(tidyverse)

dat <- read_csv("annotated_exercise_data.csv")

# =============================================================================
# 1. PARTICIPANT-LEVEL DATA
# =============================================================================

participant_dat <- dat %>%
  group_by(participant_ID) %>%
  summarise(
    overall_usefulness_score = first(overall_usefulness_score),
    overall_experience_score = first(overall_experience_score),
    time_invested = first(time_invested),
    external_source = first(external_source),
    
    # Completion is exercise-level, so we aggregate it here
    n_exercises = n(),
    n_completed = sum(complete == 1, na.rm = TRUE),
    prop_completed = mean(complete == 1, na.rm = TRUE),
    completed_all = as.integer(n_completed == 15),
    .groups = "drop"
  )


# =============================================================================
# 2. EXERCISE-SPECIFIC USEFULNESS
# =============================================================================

exercise_usefulness <- dat %>%
  filter(between(evaluation_score, 1, 4)) %>%
  summarise(
    n = n(),
    mean = mean(evaluation_score, na.rm = TRUE),
    sd = sd(evaluation_score, na.rm = TRUE),
    median = median(evaluation_score, na.rm = TRUE),
    q1 = quantile(evaluation_score, .25, na.rm = TRUE),
    q3 = quantile(evaluation_score, .75, na.rm = TRUE),
    min = min(evaluation_score, na.rm = TRUE),
    max = max(evaluation_score, na.rm = TRUE),
    n_positive = sum(evaluation_score >= 3, na.rm = TRUE),
    pct_positive = mean(evaluation_score >= 3, na.rm = TRUE) * 100
  )

exercise_usefulness

dat %>%
  filter(between(evaluation_score, 1, 4)) %>%
  count(evaluation_score) %>%
  mutate(percent = n / sum(n) * 100)


# =============================================================================
# 3. TIME INVESTED
# =============================================================================

time_stats <- participant_dat %>%
  summarise(
    n = n(),
    mean = mean(time_invested),
    sd = sd(time_invested),
    median = median(time_invested),
    q1 = quantile(time_invested, .25),
    q3 = quantile(time_invested, .75),
    iqr = IQR(time_invested),
    min = min(time_invested),
    max = max(time_invested)
  )

time_stats


# =============================================================================
# 4. EXTERNAL RESOURCE USE
# =============================================================================

external_source_stats <- participant_dat %>%
  summarise(
    n_total = n(),
    n_used = sum(external_source == 1, na.rm = TRUE),
    pct_used = mean(external_source == 1, na.rm = TRUE) * 100
  )

external_source_stats


# =============================================================================
# 5. OVERALL PERCEIVED USEFULNESS
# =============================================================================

overall_usefulness_stats <- participant_dat %>%
  summarise(
    n = sum(!is.na(overall_usefulness_score)),
    mean = mean(overall_usefulness_score, na.rm = TRUE),
    sd = sd(overall_usefulness_score, na.rm = TRUE),
    median = median(overall_usefulness_score, na.rm = TRUE),
    q1 = quantile(overall_usefulness_score, .25, na.rm = TRUE),
    q3 = quantile(overall_usefulness_score, .75, na.rm = TRUE),
    min = min(overall_usefulness_score, na.rm = TRUE),
    max = max(overall_usefulness_score, na.rm = TRUE),
    n_positive = sum(overall_usefulness_score >= 3, na.rm = TRUE),
    pct_positive = mean(overall_usefulness_score >= 3, na.rm = TRUE) * 100
  )

overall_usefulness_stats


# =============================================================================
# 6. OVERALL EXPERIENCE
# =============================================================================

overall_experience_stats <- participant_dat %>%
  summarise(
    n = sum(!is.na(overall_experience_score)),
    mean = mean(overall_experience_score, na.rm = TRUE),
    sd = sd(overall_experience_score, na.rm = TRUE),
    median = median(overall_experience_score, na.rm = TRUE),
    q1 = quantile(overall_experience_score, .25, na.rm = TRUE),
    q3 = quantile(overall_experience_score, .75, na.rm = TRUE),
    min = min(overall_experience_score, na.rm = TRUE),
    max = max(overall_experience_score, na.rm = TRUE),
    n_positive = sum(overall_experience_score >= 3, na.rm = TRUE),
    pct_positive = mean(overall_experience_score >= 3, na.rm = TRUE) * 100
  )

overall_experience_stats


# =============================================================================
# 7. COMPLETION
# =============================================================================

exercise_completion <- dat %>%
  summarise(
    n_exercises = n(),
    n_completed = sum(complete == 1, na.rm = TRUE),
    pct_completed = mean(complete == 1, na.rm = TRUE) * 100
  )

exercise_completion

participant_completion <- participant_dat %>%
  summarise(
    n_participants = n(),
    n_completed_all = sum(completed_all == 1),
    pct_completed_all = mean(completed_all == 1) * 100,
    mean_n_completed = mean(n_completed),
    sd_n_completed = sd(n_completed),
    median_n_completed = median(n_completed),
    min_n_completed = min(n_completed),
    max_n_completed = max(n_completed)
  )

participant_completion


# =============================================================================
# 8. UNDERSTANDING
# =============================================================================

understanding_stats <- dat %>%
  summarise(
    n = n(),
    n_understood = sum(understanding == 1, na.rm = TRUE),
    pct_understood = mean(understanding == 1, na.rm = TRUE) * 100
  )

understanding_stats


# =============================================================================
# 9. LEARNER-EXPERIENCED INVOLVEMENT LOAD (LIL)
# =============================================================================

analysis_dat <- read_csv("data_for_analysis.csv")

lil_stats <- analysis_dat %>%
  summarise(
    n = sum(!is.na(il_need_motivation)),
    mean = mean(il_need_motivation, na.rm = TRUE),
    sd = sd(il_need_motivation, na.rm = TRUE),
    median = median(il_need_motivation, na.rm = TRUE),
    q1 = quantile(il_need_motivation, .25, na.rm = TRUE),
    q3 = quantile(il_need_motivation, .75, na.rm = TRUE),
    min = min(il_need_motivation, na.rm = TRUE),
    max = max(il_need_motivation, na.rm = TRUE)
  )

lil_stats
