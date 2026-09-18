# Run from statistical_analyses/: Rscript tests/test-agreement.R
source(file.path("R", "agreement.R"))

spec <- il_component_spec()
stopifnot(nrow(spec) == 6L, identical(unique(spec$construct), "LIL"))

original_source <- data.frame(
  item = 1:6,
  why_solve = c("helpful", "it was required", "useful", NA, "it was easy", "unknown"),
  belief = c("useful", "unclear", "relevant", NA, "focused", "boring"),
  overall_usefulness_score = c(4, 2, 3, NA, 1, 4),
  evaluation_score = c(4, 2, 3, 1, NA, 4),
  overall_experience_score = c(4, 2, 3, 1, 4, NA),
  understanding = c(1, 1, 0, 1, 0, NA)
)

original_prepared <- prepare_original_il_ratings(original_source, "item")
stopifnot(
  identical(original_prepared$ownership, c(1L, 0L, 1L, NA_integer_, 0L, NA_integer_)),
  identical(original_prepared$goal_aligned, c(1L, 0L, 1L, NA_integer_, 1L, 0L)),
  is.na(original_prepared$relevant_goals[4]),
  is.na(original_prepared$positive_emotion[6])
)

second <- original_prepared
second$goal_aligned <- c(1, 1, 1, 0, 1, 0)
ratings <- combine_il_raters(original_source, second, "item", c("A", "B"))
result <- calculate_il_agreement(ratings, "item", bootstrap_iterations = 100L)
goal <- result$by_column[result$by_column$column == "goal_aligned", ]
stopifnot(
  nrow(result$by_column) == 6L,
  nrow(result$composites) == 1L,
  result$composites$construct == "LIL",
  goal$n_complete_pairs == 5L,
  result$by_column$percent_agreement[result$by_column$column == "understanding"] == 100,
  result$by_column$kappa[result$by_column$column == "understanding"] == 1,
  goal$unit == "learner_response",
  result$composites$n_components == 6L,
  nrow(result$disagreements) == 1L
)

perfect <- matrix(c(0, 1, 2, 3, 0, 1, 2, 3), ncol = 2)
stopifnot(isTRUE(all.equal(icc_absolute_single(perfect), 1)))

duplicate <- rbind(ratings, ratings[1, ])
stopifnot(inherits(try(calculate_il_agreement(duplicate, "item"), silent = TRUE), "try-error"))

non_binary <- ratings
non_binary$ownership[1] <- 2
non_binary_result <- calculate_il_agreement(non_binary, "item", bootstrap_iterations = 0L)
stopifnot(
  non_binary_result$by_column$status[non_binary_result$by_column$column == "ownership"] == "non_binary",
  startsWith(non_binary_result$composites$status, "invalid_components:")
)

output_directory <- tempfile("lil-agreement-")
paths <- write_il_agreement(result, output_directory)
stopifnot(all(file.exists(paths)))
unlink(output_directory, recursive = TRUE)

rater_1_path <- tempfile(fileext = ".csv")
rater_2_path <- tempfile(fileext = ".csv")
output_directory <- tempfile("lil-agreement-run-")
utils::write.csv(original_source, rater_1_path, row.names = FALSE, na = "")
utils::write.csv(second, rater_2_path, row.names = FALSE, na = "")
standalone <- run_il_agreement(
  rater_1_path, rater_2_path, output_directory, "item", bootstrap_iterations = 25L
)
stopifnot(all(file.exists(standalone$paths)))
unlink(c(rater_1_path, rater_2_path, output_directory), recursive = TRUE)

cohen_toy <- data.frame(
  first = c("a", "a", "b", "b"),
  second = c("a", "a", "b", "b")
)
cohen_perfect <- calculate_cohen_kappa(cohen_toy, "first", "second", "toy")
stopifnot(
  cohen_perfect$summary$n_items == 4L,
  cohen_perfect$summary$cohen_kappa == 1,
  sum(diag(cohen_perfect$confusion)) == 4L
)

fleiss_toy <- data.frame(
  A = c("x", "y", "x"), B = c("x", "y", "x"), C = c("x", "y", "x")
)
fleiss_perfect <- calculate_fleiss_kappa(fleiss_toy, c("A", "B", "C"),
                                         "toy", bootstrap_iterations = 20L)
stopifnot(
  fleiss_perfect$summary$n_items == 3L,
  fleiss_perfect$summary$fleiss_kappa == 1,
  all(rowSums(fleiss_perfect$counts) == 3L)
)

til_first <- data.frame(
  participant_ID = rep(c("P1", "P2"), each = 3L),
  item = 1:6,
  recall = c(1, 0, 1, 0, 1, 0)
)
til_second <- til_first
til_second$recall <- c(1, 0, 0, 0, 1, 1)
til_second <- til_second[c("item", "recall")]
til_ratings <- combine_til_raters(
  til_first, til_second, c("participant_ID", "item"), c("A", "B")
)
til_result <- calculate_til_agreement(
  til_ratings, c("participant_ID", "item"), cluster_column = "participant_ID",
  bootstrap_iterations = 50L, seed = 42L
)
stopifnot(
  til_result$by_column$construct == "TIL",
  til_result$by_column$dimension == "retrieval",
  til_result$by_column$column == "recall",
  til_result$by_column$n_complete_pairs == 6L,
  til_result$by_column$n_clusters == 2L,
  isTRUE(all.equal(til_result$by_column$percent_agreement, 100 * 4 / 6)),
  nrow(til_result$disagreements) == 2L
)

til_first_path <- tempfile(fileext = ".csv")
til_second_path <- tempfile(fileext = ".csv")
til_output_directory <- tempfile("til-agreement-")
utils::write.csv(til_first, til_first_path, row.names = FALSE)
utils::write.csv(til_second, til_second_path, row.names = FALSE)
til_run <- run_til_agreement(
  til_first_path, til_second_path, til_output_directory,
  c("participant_ID", "item"), cluster_column = "participant_ID",
  bootstrap_iterations = 25L, seed = 42L
)
stopifnot(all(file.exists(til_run$paths)))
unlink(c(til_first_path, til_second_path, til_output_directory), recursive = TRUE)

categorical_input_directory <- file.path("..", "code", "resources")
if (dir.exists(categorical_input_directory)) {
  cohen_type <- calculate_cohen_kappa(
    utils::read.csv(file.path(categorical_input_directory, "cohen_kappa_exercise_type.csv"),
                    check.names = FALSE, stringsAsFactors = FALSE),
    "rater_1", "rater_2", "exercise_type"
  )
  cohen_format <- calculate_cohen_kappa(
    utils::read.csv(file.path(categorical_input_directory, "cohen_kappa_exercise_format.csv"),
                    check.names = FALSE, stringsAsFactors = FALSE),
    "rater_1", "rater_2", "exercise_format"
  )
  fleiss_error <- calculate_fleiss_kappa(
    utils::read.csv(file.path(categorical_input_directory, "fleiss_kappa_error_type.csv"),
                    check.names = FALSE, stringsAsFactors = FALSE),
    c("A01", "A02", "A03"), "error_type", bootstrap_iterations = 100L, seed = 42L
  )
  stopifnot(
    isTRUE(all.equal(cohen_type$summary$cohen_kappa, 0.8857849656493546, tolerance = 1e-12)),
    isTRUE(all.equal(cohen_format$summary$cohen_kappa, 0.9001718543683522, tolerance = 1e-12)),
    isTRUE(all.equal(fleiss_error$summary$fleiss_kappa, 0.95165748295141, tolerance = 1e-12))
  )

  categorical_output_directory <- tempfile("categorical-agreement-")
  categorical_run <- run_categorical_agreement(
    list(type = list(
      path = file.path(categorical_input_directory, "cohen_kappa_exercise_type.csv"),
      rater_1 = "rater_1", rater_2 = "rater_2"
    )),
    list(error = list(
      path = file.path(categorical_input_directory, "fleiss_kappa_error_type.csv"),
      rater_columns = c("A01", "A02", "A03")
    )),
    categorical_output_directory, bootstrap_iterations = 20L, seed = 42L
  )
  stopifnot(all(file.exists(categorical_run$paths)))
  unlink(categorical_output_directory, recursive = TRUE)
}

cat("Agreement unit checks passed.\n")
