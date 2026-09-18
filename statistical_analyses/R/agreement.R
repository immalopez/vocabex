il_component_spec <- function() {
  data.frame(
    construct = rep("LIL", 6L),
    dimension = rep("motivation", 6L),
    unit = rep("learner_response", 6L),
    evidence_source = rep("learner_response", 6L),
    column = c(
      "goal_aligned", "relevant_goals",
      "interesting_content", "ownership", "understanding", "positive_emotion"
    ),
    stringsAsFactors = FALSE
  )
}

il_agreement_component_columns <- function() {
  il_component_spec()$column
}

normalize_agreement_text <- function(x) {
  result <- tolower(trimws(as.character(x)))
  result <- gsub("\\s+", " ", result)
  result[is.na(x)] <- NA_character_
  result
}

prepare_original_il_ratings <- function(data, id_columns) {
  spec <- il_component_spec()
  source_columns <- unique(c(
    id_columns, "why_solve", "belief",
    "overall_usefulness_score", "evaluation_score", "overall_experience_score", "understanding"
  ))
  missing_columns <- setdiff(source_columns, names(data))
  if (length(missing_columns)) {
    stop("Rater 1 source is missing columns: ", paste(missing_columns, collapse = ", "),
         call. = FALSE)
  }
  if (anyNA(data[id_columns])) stop("Rater 1 item identifiers must not be missing", call. = FALSE)
  if (any(duplicated(data[id_columns]))) {
    stop("Rater 1 has duplicate item identifiers", call. = FALSE)
  }

  result <- data[id_columns]

  ownership_yes <- c(
    "because i am aware i need to practice a lot", "helpful", "i needed the practice",
    "interesting and specific to my problems", "it was good practice",
    "personalized learning is a great opportunity", "to improve my level of spanish",
    "to improve myself", "to increase my error awareness and improve my spanish", "to learn",
    "to learn and work on recurrent mistakes", "to learn from my mistakes",
    "to learn in general and specifically from my mistakes",
    "to make the most out of this opportunity to learn", "to put my abilities to the test",
    "to reflect on my mistakes", "useful", "useful for practice"
  )
  ownership_no <- c("forgot about them", "it was easy", "it was required", "it was fun and required")
  why <- normalize_agreement_text(data$why_solve)
  result$ownership <- ifelse(
    is.na(why) | why == "", NA_integer_,
    ifelse(why %in% ownership_yes, 1L, ifelse(why %in% ownership_no, 0L, NA_integer_))
  )

  goal_positive <- c(
    "personalized", "excellent for", "helpful even for my l1", "interlinguistic compar",
    "made me think", "^it made me think$", "raised my error awareness",
    "^it raised my error awareness$", "^insightful$", "^relevant$",
    "^relevant and reflective$", "relevant and useful definitions", "^focused$",
    "creative, useful", "promotes idiomaticity", "^useful$", "very useful", "useful for",
    "useful example", "useful examples", "useful explanation", "useful rules",
    "useful revision", "useful for revision", "^helpful$", "good practice", "good drill",
    "challenging and very useful"
  )
  belief <- normalize_agreement_text(data$belief)
  positive <- !is.na(belief) & grepl(paste(goal_positive, collapse = "|"), belief, perl = TRUE)
  result$goal_aligned <- ifelse(is.na(belief) | belief == "", NA_integer_, as.integer(positive))

  threshold_binary <- function(x) {
    numeric <- suppressWarnings(as.numeric(x))
    ifelse(is.na(numeric), NA_integer_, as.integer(numeric > 2))
  }
  result$relevant_goals <- threshold_binary(data$overall_usefulness_score)
  result$interesting_content <- threshold_binary(data$evaluation_score)
  result$positive_emotion <- threshold_binary(data$overall_experience_score)
  result$understanding <- data$understanding
  result[c(id_columns, spec$column)]
}

combine_il_raters <- function(original_data, second_rater_data, id_columns,
                              rater_labels = c("original", "rater_2")) {
  if (length(rater_labels) != 2L || anyNA(rater_labels) || anyDuplicated(rater_labels)) {
    stop("rater_labels must contain two distinct labels", call. = FALSE)
  }
  component_columns <- il_agreement_component_columns()
  original <- prepare_original_il_ratings(original_data, id_columns)
  missing_columns <- setdiff(c(id_columns, component_columns), names(second_rater_data))
  if (length(missing_columns)) {
    stop("Rater 2 file is missing columns: ", paste(missing_columns, collapse = ", "),
         call. = FALSE)
  }
  second <- second_rater_data[c(id_columns, component_columns)]
  if (anyNA(second[id_columns])) stop("Rater 2 item identifiers must not be missing", call. = FALSE)
  if (any(duplicated(second[id_columns]))) stop("Rater 2 has duplicate item identifiers", call. = FALSE)
  original$rater <- rater_labels[1]
  second$rater <- rater_labels[2]
  rbind(original[c(id_columns, "rater", component_columns)],
        second[c(id_columns, "rater", component_columns)])
}

validate_il_ratings <- function(data, id_columns, rater_column, component_columns) {
  required <- unique(c(id_columns, rater_column, component_columns))
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns)) {
    stop("Missing required rating columns: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (anyNA(data[id_columns]) || anyNA(data[[rater_column]])) {
    stop("Item identifiers and rater identifiers must not be missing", call. = FALSE)
  }
  raters <- unique(as.character(data[[rater_column]]))
  if (length(raters) != 2L) {
    stop("This analysis requires exactly two raters; found ", length(raters), call. = FALSE)
  }
  duplicate_key <- duplicated(data[c(id_columns, rater_column)])
  if (any(duplicate_key)) {
    stop("Each item may occur only once per rater; found ", sum(duplicate_key),
         " duplicate item-rater rows", call. = FALSE)
  }
  invisible(raters)
}

binary_agreement_estimates <- function(x, y) {
  tab <- table(factor(x, levels = c(0, 1)), factor(y, levels = c(0, 1)))
  n <- sum(tab)
  if (!n) {
    return(c(percent_agreement = NA_real_, positive_agreement = NA_real_,
             negative_agreement = NA_real_, kappa = NA_real_, gwet_ac1 = NA_real_))
  }
  observed <- sum(diag(tab)) / n
  expected <- sum(rowSums(tab) * colSums(tab)) / n^2
  kappa <- if (isTRUE(all.equal(expected, 1))) NA_real_ else (observed - expected) / (1 - expected)
  positive_denominator <- 2 * tab[2, 2] + tab[1, 2] + tab[2, 1]
  negative_denominator <- 2 * tab[1, 1] + tab[1, 2] + tab[2, 1]
  positive <- if (positive_denominator == 0) NA_real_ else 2 * tab[2, 2] / positive_denominator
  negative <- if (negative_denominator == 0) NA_real_ else 2 * tab[1, 1] / negative_denominator

  category_one_prevalence <- (sum(x == 1) + sum(y == 1)) / (2 * n)
  ac1_expected <- 2 * category_one_prevalence * (1 - category_one_prevalence)
  ac1 <- if (isTRUE(all.equal(ac1_expected, 1))) NA_real_ else
    (observed - ac1_expected) / (1 - ac1_expected)
  c(percent_agreement = observed * 100, positive_agreement = positive,
    negative_agreement = negative, kappa = kappa, gwet_ac1 = ac1)
}

bootstrap_binary_agreement <- function(x, y, iterations = 2000L, seed = 42L) {
  if (iterations < 1L || length(x) < 2L) {
    return(c(kappa_low = NA_real_, kappa_high = NA_real_,
             gwet_ac1_low = NA_real_, gwet_ac1_high = NA_real_))
  }
  set.seed(seed)
  draws <- vapply(seq_len(iterations), function(iteration) {
    index <- sample.int(length(x), length(x), replace = TRUE)
    binary_agreement_estimates(x[index], y[index])[c("kappa", "gwet_ac1")]
  }, numeric(2))
  interval <- function(values) {
    values <- values[is.finite(values)]
    if (length(values) < max(20L, ceiling(iterations * 0.5))) return(c(NA_real_, NA_real_))
    unname(stats::quantile(values, c(0.025, 0.975), na.rm = TRUE))
  }
  kappa_interval <- interval(draws["kappa", ])
  ac1_interval <- interval(draws["gwet_ac1", ])
  c(kappa_low = kappa_interval[1], kappa_high = kappa_interval[2],
    gwet_ac1_low = ac1_interval[1], gwet_ac1_high = ac1_interval[2])
}

icc_absolute_single <- function(values) {
  values <- as.matrix(values)
  n <- nrow(values)
  k <- ncol(values)
  if (n < 2L || k < 2L || anyNA(values)) return(NA_real_)
  grand_mean <- mean(values)
  row_means <- rowMeans(values)
  column_means <- colMeans(values)
  ms_rows <- k * sum((row_means - grand_mean)^2) / (n - 1)
  ms_columns <- n * sum((column_means - grand_mean)^2) / (k - 1)
  residuals <- values - row_means - rep(column_means, each = n) + grand_mean
  ms_error <- sum(residuals^2) / ((n - 1) * (k - 1))
  denominator <- ms_rows + (k - 1) * ms_error + k * (ms_columns - ms_error) / n
  if (isTRUE(all.equal(denominator, 0))) NA_real_ else (ms_rows - ms_error) / denominator
}

bootstrap_icc <- function(values, iterations = 2000L, seed = 42L) {
  if (iterations < 1L || nrow(values) < 2L) return(c(NA_real_, NA_real_))
  set.seed(seed)
  draws <- replicate(iterations, {
    index <- sample.int(nrow(values), nrow(values), replace = TRUE)
    icc_absolute_single(values[index, , drop = FALSE])
  })
  draws <- draws[is.finite(draws)]
  if (length(draws) < max(20L, ceiling(iterations * 0.5))) return(c(NA_real_, NA_real_))
  unname(stats::quantile(draws, c(0.025, 0.975), na.rm = TRUE))
}

pair_rater_values <- function(data, id_columns, rater_column, value_columns, raters) {
  left <- data[data[[rater_column]] == raters[1], c(id_columns, value_columns), drop = FALSE]
  right <- data[data[[rater_column]] == raters[2], c(id_columns, value_columns), drop = FALSE]
  names(left)[match(value_columns, names(left))] <- paste0(value_columns, "__rater_1")
  names(right)[match(value_columns, names(right))] <- paste0(value_columns, "__rater_2")
  merge(left, right, by = id_columns, all = TRUE, sort = FALSE)
}

calculate_il_agreement <- function(data, id_columns, rater_column = "rater",
                                   bootstrap_iterations = 2000L, seed = 42L) {
  spec <- il_component_spec()
  component_columns <- spec$column
  construct_by_column <- setNames(spec$construct, spec$column)
  dimension_by_column <- setNames(spec$dimension, spec$column)
  unit_by_column <- setNames(spec$unit, spec$column)
  evidence_by_column <- setNames(spec$evidence_source, spec$column)
  raters <- validate_il_ratings(data, id_columns, rater_column, component_columns)
  paired <- pair_rater_values(data, id_columns, rater_column, component_columns, raters)

  summaries <- lapply(seq_along(component_columns), function(column_index) {
    column <- component_columns[column_index]
    raw_x <- paired[[paste0(column, "__rater_1")]]
    raw_y <- paired[[paste0(column, "__rater_2")]]
    x <- suppressWarnings(as.numeric(raw_x))
    y <- suppressWarnings(as.numeric(raw_y))
    conversion_failed <- (!is.na(raw_x) & is.na(x)) | (!is.na(raw_y) & is.na(y))
    complete <- !is.na(x) & !is.na(y)
    valid_values <- unique(c(x[complete], y[complete]))
    binary <- !any(conversion_failed) && all(valid_values %in% c(0, 1))
    base <- data.frame(
      construct = unname(construct_by_column[[column]]),
      dimension = unname(dimension_by_column[[column]]),
      unit = unname(unit_by_column[[column]]),
      evidence_source = unname(evidence_by_column[[column]]),
      column = column,
      rater_1 = raters[1], rater_2 = raters[2],
      n_items_total = nrow(paired), n_complete_pairs = sum(complete),
      n_missing_pairs = sum(!complete),
      prevalence_rater_1 = if (sum(complete)) mean(x[complete] == 1) else NA_real_,
      prevalence_rater_2 = if (sum(complete)) mean(y[complete] == 1) else NA_real_,
      status = if (any(conversion_failed)) "non_numeric" else if (!sum(complete))
        "no_complete_pairs" else if (!binary) "non_binary" else "ok",
      stringsAsFactors = FALSE
    )
    empty_metrics <- as.list(setNames(rep(NA_real_, 9), c(
      "percent_agreement", "positive_agreement", "negative_agreement", "kappa", "gwet_ac1",
      "kappa_low", "kappa_high", "gwet_ac1_low", "gwet_ac1_high"
    )))
    if (!sum(complete) || !binary) return(cbind(base, as.data.frame(empty_metrics)))
    estimates <- binary_agreement_estimates(x[complete], y[complete])
    intervals <- bootstrap_binary_agreement(
      x[complete], y[complete], bootstrap_iterations, seed + column_index
    )
    cbind(base, as.data.frame(as.list(c(estimates, intervals))))
  })
  by_column <- do.call(rbind, summaries)
  rownames(by_column) <- NULL

  disagreement_rows <- lapply(component_columns, function(column) {
    left_name <- paste0(column, "__rater_1")
    right_name <- paste0(column, "__rater_2")
    disagree <- !is.na(paired[[left_name]]) & !is.na(paired[[right_name]]) &
      paired[[left_name]] != paired[[right_name]]
    if (!any(disagree)) return(NULL)
    result <- paired[disagree, id_columns, drop = FALSE]
    result$construct <- unname(construct_by_column[[column]])
    result$dimension <- unname(dimension_by_column[[column]])
    result$unit <- unname(unit_by_column[[column]])
    result$evidence_source <- unname(evidence_by_column[[column]])
    result$column <- column
    result$rater_1 <- raters[1]
    result$rater_2 <- raters[2]
    result$value_rater_1 <- paired[[left_name]][disagree]
    result$value_rater_2 <- paired[[right_name]][disagree]
    result
  })
  disagreements <- do.call(rbind, disagreement_rows)
  if (is.null(disagreements)) {
    disagreements <- data.frame(matrix(nrow = 0L, ncol = length(id_columns) + 9L))
    names(disagreements) <- c(
      id_columns, "construct", "dimension", "unit", "evidence_source", "column",
      "rater_1", "rater_2", "value_rater_1", "value_rater_2"
    )
  }
  rownames(disagreements) <- NULL

  composite_rows <- lapply(unique(spec$construct), function(construct) {
    columns <- spec$column[spec$construct == construct]
    if (!length(columns)) return(NULL)
    invalid_columns <- by_column$column[
      by_column$construct == construct & by_column$status %in% c("non_binary", "non_numeric")
    ]
    if (length(invalid_columns)) {
      return(data.frame(
        construct = construct, rater_1 = raters[1], rater_2 = raters[2],
        n_components = length(columns), n_items_total = nrow(paired),
        n_complete_pairs = NA_integer_, n_missing_pairs = NA_integer_,
        icc_absolute_single = NA_real_, icc_low = NA_real_, icc_high = NA_real_,
        mean_difference_rater_2_minus_1 = NA_real_, sd_difference = NA_real_,
        bland_altman_low = NA_real_, bland_altman_high = NA_real_,
        status = paste0("invalid_components: ", paste(invalid_columns, collapse = ", ")),
        stringsAsFactors = FALSE
      ))
    }
    rater_totals <- lapply(raters, function(rater) {
      subset <- data[data[[rater_column]] == rater, c(id_columns, columns), drop = FALSE]
      numeric_values <- as.data.frame(lapply(subset[columns], function(x) suppressWarnings(as.numeric(x))))
      subset$total <- ifelse(rowSums(is.na(numeric_values)) == 0L, rowSums(numeric_values), NA_real_)
      subset[c(id_columns, "total")]
    })
    names(rater_totals[[1]])[ncol(rater_totals[[1]])] <- "rater_1_total"
    names(rater_totals[[2]])[ncol(rater_totals[[2]])] <- "rater_2_total"
    totals <- merge(rater_totals[[1]], rater_totals[[2]], by = id_columns, all = TRUE, sort = FALSE)
    complete <- stats::complete.cases(totals[c("rater_1_total", "rater_2_total")])
    values <- as.matrix(totals[complete, c("rater_1_total", "rater_2_total")])
    difference <- if (nrow(values)) values[, 2] - values[, 1] else numeric()
    icc <- icc_absolute_single(values)
    interval <- bootstrap_icc(values, bootstrap_iterations, seed + length(columns))
    difference_sd <- if (length(difference) > 1L) stats::sd(difference) else NA_real_
    data.frame(
      construct = construct,
      rater_1 = raters[1], rater_2 = raters[2], n_components = length(columns),
      n_items_total = nrow(totals), n_complete_pairs = nrow(values),
      n_missing_pairs = nrow(totals) - nrow(values),
      icc_absolute_single = icc, icc_low = interval[1], icc_high = interval[2],
      mean_difference_rater_2_minus_1 = if (length(difference)) mean(difference) else NA_real_,
      sd_difference = difference_sd,
      bland_altman_low = if (is.finite(difference_sd)) mean(difference) - 1.96 * difference_sd else NA_real_,
      bland_altman_high = if (is.finite(difference_sd)) mean(difference) + 1.96 * difference_sd else NA_real_,
      status = if (nrow(values) < 2L) "insufficient_complete_pairs" else "ok",
      stringsAsFactors = FALSE
    )
  })
  composites <- do.call(rbind, composite_rows)
  rownames(composites) <- NULL

  list(by_column = by_column, composites = composites, disagreements = disagreements,
       raters = raters, component_columns = component_columns)
}

write_il_agreement <- function(result, output_directory) {
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    by_column = file.path(output_directory, "il_agreement_by_column.csv"),
    composites = file.path(output_directory, "il_agreement_composites.csv"),
    disagreements = file.path(output_directory, "il_agreement_disagreements.csv")
  )
  utils::write.csv(result$by_column, paths[["by_column"]], row.names = FALSE, na = "")
  utils::write.csv(result$composites, paths[["composites"]], row.names = FALSE, na = "")
  utils::write.csv(result$disagreements, paths[["disagreements"]], row.names = FALSE, na = "")
  invisible(paths)
}

run_il_agreement <- function(rater_1_path, rater_2_path, output_directory, id_columns,
                             bootstrap_iterations = 2000L, seed = 42L,
                             rater_labels = c("original", "rater_2")) {
  missing_files <- c(rater_1_path, rater_2_path)[!file.exists(c(rater_1_path, rater_2_path))]
  if (length(missing_files)) {
    stop("Agreement input file does not exist: ", paste(missing_files, collapse = ", "), call. = FALSE)
  }
  read_ratings <- function(path) utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA")
  )
  data <- combine_il_raters(read_ratings(rater_1_path), read_ratings(rater_2_path),
                            id_columns, rater_labels)
  result <- calculate_il_agreement(data, id_columns, "rater",
                                   bootstrap_iterations, seed)
  paths <- write_il_agreement(result, output_directory)
  message("Wrote IL agreement outputs to ", normalizePath(output_directory, mustWork = TRUE))
  invisible(list(result = result, paths = paths))
}

til_component_spec <- function() {
  data.frame(
    construct = "TIL",
    dimension = "retrieval",
    unit = "exercise",
    evidence_source = "task_material",
    column = "recall",
    stringsAsFactors = FALSE
  )
}

combine_til_raters <- function(original_data, second_rater_data, id_columns,
                               rater_labels = c("original", "rater_2")) {
  if (length(rater_labels) != 2L || anyNA(rater_labels) || anyDuplicated(rater_labels)) {
    stop("rater_labels must contain two distinct labels", call. = FALSE)
  }
  required <- c(id_columns, "recall")
  missing_original <- setdiff(required, names(original_data))
  if (length(missing_original)) {
    stop("Rater 1 source is missing columns: ", paste(missing_original, collapse = ", "),
         call. = FALSE)
  }
  missing_second_ids <- setdiff(id_columns, names(second_rater_data))
  if (length(missing_second_ids)) {
    shared_ids <- intersect(id_columns, names(second_rater_data))
    if (!length(shared_ids)) {
      stop("Rater 2 must contain at least one item identifier column; expected one of: ",
           paste(id_columns, collapse = ", "), call. = FALSE)
    }
    if (anyNA(original_data[shared_ids]) || anyNA(second_rater_data[shared_ids])) {
      stop("Shared TIL item identifiers must not be missing", call. = FALSE)
    }
    if (any(duplicated(original_data[shared_ids])) ||
        any(duplicated(second_rater_data[shared_ids]))) {
      stop("The shared TIL identifiers are not unique: ",
           paste(shared_ids, collapse = ", "), call. = FALSE)
    }
    lookup <- original_data[c(shared_ids, missing_second_ids)]
    second_rater_data$.til_row_order <- seq_len(nrow(second_rater_data))
    second_rater_data <- merge(second_rater_data, lookup, by = shared_ids,
                               all.x = TRUE, sort = FALSE)
    second_rater_data <- second_rater_data[order(second_rater_data$.til_row_order), ]
    second_rater_data$.til_row_order <- NULL
  }
  missing_second <- setdiff(required, names(second_rater_data))
  if (length(missing_second)) {
    stop("Rater 2 file is missing columns: ", paste(missing_second, collapse = ", "),
         call. = FALSE)
  }
  first <- original_data[required]
  second <- second_rater_data[required]
  if (anyNA(first[id_columns]) || anyNA(second[id_columns])) {
    stop("TIL item identifiers must not be missing", call. = FALSE)
  }
  if (any(duplicated(first[id_columns]))) stop("Rater 1 has duplicate item identifiers", call. = FALSE)
  if (any(duplicated(second[id_columns]))) stop("Rater 2 has duplicate item identifiers", call. = FALSE)
  first$rater <- rater_labels[1]
  second$rater <- rater_labels[2]
  rbind(first[c(id_columns, "rater", "recall")],
        second[c(id_columns, "rater", "recall")])
}

bootstrap_binary_agreement_clustered <- function(x, y, cluster, iterations = 2000L,
                                                 seed = 42L) {
  empty <- c(kappa_low = NA_real_, kappa_high = NA_real_,
             gwet_ac1_low = NA_real_, gwet_ac1_high = NA_real_)
  clusters <- unique(as.character(cluster))
  if (iterations < 1L || length(clusters) < 2L) return(empty)
  indices <- split(seq_along(cluster), as.character(cluster))
  set.seed(seed)
  draws <- vapply(seq_len(iterations), function(iteration) {
    sampled_clusters <- sample(clusters, length(clusters), replace = TRUE)
    sampled_rows <- unlist(indices[sampled_clusters], use.names = FALSE)
    binary_agreement_estimates(x[sampled_rows], y[sampled_rows])[c("kappa", "gwet_ac1")]
  }, numeric(2))
  interval <- function(values) {
    values <- values[is.finite(values)]
    if (length(values) < max(20L, ceiling(iterations * 0.5))) return(c(NA_real_, NA_real_))
    unname(stats::quantile(values, c(0.025, 0.975), na.rm = TRUE))
  }
  kappa_interval <- interval(draws["kappa", ])
  ac1_interval <- interval(draws["gwet_ac1", ])
  c(kappa_low = kappa_interval[1], kappa_high = kappa_interval[2],
    gwet_ac1_low = ac1_interval[1], gwet_ac1_high = ac1_interval[2])
}

calculate_til_agreement <- function(data, id_columns, rater_column = "rater",
                                    cluster_column = "participant_ID",
                                    bootstrap_iterations = 2000L, seed = 42L) {
  spec <- til_component_spec()
  raters <- validate_il_ratings(data, id_columns, rater_column, spec$column)
  if (!cluster_column %in% id_columns) {
    stop("cluster_column must be one of the item identifier columns", call. = FALSE)
  }
  paired <- pair_rater_values(data, id_columns, rater_column, spec$column, raters)
  raw_x <- paired$recall__rater_1
  raw_y <- paired$recall__rater_2
  x <- suppressWarnings(as.numeric(raw_x))
  y <- suppressWarnings(as.numeric(raw_y))
  conversion_failed <- (!is.na(raw_x) & is.na(x)) | (!is.na(raw_y) & is.na(y))
  complete <- !is.na(x) & !is.na(y)
  valid_values <- unique(c(x[complete], y[complete]))
  binary <- !any(conversion_failed) && all(valid_values %in% c(0, 1))
  status <- if (any(conversion_failed)) "non_numeric" else if (!sum(complete))
    "no_complete_pairs" else if (!binary) "non_binary" else "ok"

  estimates <- setNames(rep(NA_real_, 5L), c(
    "percent_agreement", "positive_agreement", "negative_agreement", "kappa", "gwet_ac1"
  ))
  intervals <- setNames(rep(NA_real_, 4L), c(
    "kappa_low", "kappa_high", "gwet_ac1_low", "gwet_ac1_high"
  ))
  if (status == "ok") {
    estimates <- binary_agreement_estimates(x[complete], y[complete])
    intervals <- bootstrap_binary_agreement_clustered(
      x[complete], y[complete], paired[[cluster_column]][complete],
      bootstrap_iterations, seed
    )
  }
  by_column <- data.frame(
    construct = spec$construct,
    dimension = spec$dimension,
    unit = spec$unit,
    evidence_source = spec$evidence_source,
    column = spec$column,
    rater_1 = raters[1],
    rater_2 = raters[2],
    n_items_total = nrow(paired),
    n_complete_pairs = sum(complete),
    n_missing_pairs = sum(!complete),
    n_clusters = length(unique(paired[[cluster_column]][complete])),
    bootstrap_unit = cluster_column,
    prevalence_rater_1 = if (sum(complete)) mean(x[complete] == 1) else NA_real_,
    prevalence_rater_2 = if (sum(complete)) mean(y[complete] == 1) else NA_real_,
    status = status,
    as.list(c(estimates, intervals)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  disagree <- complete & x != y
  disagreements <- paired[disagree, id_columns, drop = FALSE]
  disagreements$construct <- spec$construct
  disagreements$dimension <- spec$dimension
  disagreements$unit <- spec$unit
  disagreements$evidence_source <- spec$evidence_source
  disagreements$column <- spec$column
  disagreements$rater_1 <- raters[1]
  disagreements$rater_2 <- raters[2]
  disagreements$value_rater_1 <- x[disagree]
  disagreements$value_rater_2 <- y[disagree]

  list(by_column = by_column, disagreements = disagreements, raters = raters)
}

write_til_agreement <- function(result, output_directory) {
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  paths <- c(
    by_column = file.path(output_directory, "til_agreement_by_column.csv"),
    disagreements = file.path(output_directory, "til_agreement_disagreements.csv")
  )
  utils::write.csv(result$by_column, paths[["by_column"]], row.names = FALSE, na = "")
  utils::write.csv(result$disagreements, paths[["disagreements"]], row.names = FALSE, na = "")
  invisible(paths)
}

run_til_agreement <- function(rater_1_path, rater_2_path, output_directory, id_columns,
                              cluster_column = "participant_ID",
                              bootstrap_iterations = 2000L, seed = 42L,
                              rater_labels = c("original", "rater_2")) {
  missing_files <- c(rater_1_path, rater_2_path)[!file.exists(c(rater_1_path, rater_2_path))]
  if (length(missing_files)) {
    stop("TIL agreement input file does not exist: ", paste(missing_files, collapse = ", "),
         call. = FALSE)
  }
  read_ratings <- function(path) utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA")
  )
  data <- combine_til_raters(read_ratings(rater_1_path), read_ratings(rater_2_path),
                             id_columns, rater_labels)
  result <- calculate_til_agreement(data, id_columns, "rater", cluster_column,
                                    bootstrap_iterations, seed)
  paths <- write_til_agreement(result, output_directory)
  message("Wrote TIL agreement outputs to ", normalizePath(output_directory, mustWork = TRUE))
  invisible(list(result = result, paths = paths))
}

validate_categorical_ratings <- function(data, rater_columns, analysis_name) {
  missing_columns <- setdiff(rater_columns, names(data))
  if (length(missing_columns)) {
    stop(analysis_name, " input is missing rater columns: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (!nrow(data)) stop(analysis_name, " input has no rating rows", call. = FALSE)
  missing_ratings <- vapply(data[rater_columns], function(x) sum(is.na(x) | trimws(x) == ""), integer(1))
  if (any(missing_ratings)) {
    details <- paste0(names(missing_ratings)[missing_ratings > 0L], "=",
                      missing_ratings[missing_ratings > 0L], collapse = ", ")
    stop(analysis_name, " ratings must be complete; missing values: ", details, call. = FALSE)
  }
  invisible(TRUE)
}

calculate_cohen_kappa <- function(data, rater_1 = "rater_1", rater_2 = "rater_2",
                                  analysis = NA_character_) {
  validate_categorical_ratings(data, c(rater_1, rater_2), "Cohen kappa")
  first <- as.character(data[[rater_1]])
  second <- as.character(data[[rater_2]])
  categories <- sort(unique(c(first, second)))
  confusion <- table(
    factor(first, levels = categories),
    factor(second, levels = categories),
    dnn = c("rater_1", "rater_2")
  )
  n <- sum(confusion)
  observed <- sum(diag(confusion)) / n
  expected <- sum(rowSums(confusion) * colSums(confusion)) / n^2
  denominator <- 1 - expected
  kappa <- if (isTRUE(all.equal(denominator, 0))) NA_real_ else
    (observed - expected) / denominator
  standard_error <- if (!is.finite(kappa) || denominator == 0) NA_real_ else
    sqrt(observed * (1 - observed) / (n * denominator^2))
  z_value <- if (!is.finite(standard_error) || standard_error == 0) NA_real_ else
    kappa / standard_error
  p_value <- if (is.na(z_value)) NA_real_ else 2 * stats::pnorm(-abs(z_value))

  summary <- data.frame(
    analysis = analysis,
    rater_1 = rater_1,
    rater_2 = rater_2,
    n_items = n,
    n_categories = length(categories),
    observed_agreement = observed,
    expected_agreement = expected,
    cohen_kappa = kappa,
    standard_error = standard_error,
    confidence_level = 0.95,
    ci_low = kappa - 1.96 * standard_error,
    ci_high = kappa + 1.96 * standard_error,
    z_value = z_value,
    p_value = p_value,
    stringsAsFactors = FALSE
  )
  list(summary = summary, confusion = confusion)
}

fleiss_kappa_from_counts <- function(counts) {
  counts <- as.matrix(counts)
  n_items <- nrow(counts)
  ratings_per_item <- rowSums(counts)
  if (!n_items || ncol(counts) < 2L || any(ratings_per_item < 2L) ||
      length(unique(ratings_per_item)) != 1L) return(NA_real_)
  n_raters <- ratings_per_item[[1]]
  item_agreement <- (rowSums(counts^2) - n_raters) / (n_raters * (n_raters - 1))
  category_proportions <- colSums(counts) / (n_items * n_raters)
  observed <- mean(item_agreement)
  expected <- sum(category_proportions^2)
  if (isTRUE(all.equal(expected, 1))) NA_real_ else (observed - expected) / (1 - expected)
}

aggregate_fleiss_ratings <- function(data, rater_columns) {
  validate_categorical_ratings(data, rater_columns, "Fleiss kappa")
  categories <- sort(unique(unlist(lapply(data[rater_columns], as.character), use.names = FALSE)))
  counts <- t(vapply(seq_len(nrow(data)), function(row_index) {
    tabulate(match(as.character(unlist(data[row_index, rater_columns], use.names = FALSE)), categories),
             nbins = length(categories))
  }, integer(length(categories))))
  colnames(counts) <- categories
  rownames(counts) <- NULL
  counts
}

calculate_fleiss_kappa <- function(data, rater_columns, analysis = NA_character_,
                                   bootstrap_iterations = 2000L, seed = 42L) {
  if (length(rater_columns) < 2L || anyDuplicated(rater_columns)) {
    stop("rater_columns must contain at least two distinct column names", call. = FALSE)
  }
  if (is.na(bootstrap_iterations) || bootstrap_iterations < 0L) {
    stop("bootstrap_iterations must be non-negative", call. = FALSE)
  }
  if (is.na(seed)) stop("seed must be an integer", call. = FALSE)
  counts <- aggregate_fleiss_ratings(data, rater_columns)
  kappa <- fleiss_kappa_from_counts(counts)

  bootstrap <- numeric()
  if (bootstrap_iterations > 0L && nrow(counts) > 1L) {
    set.seed(seed)
    bootstrap <- replicate(bootstrap_iterations, {
      index <- sample.int(nrow(counts), nrow(counts), replace = TRUE)
      fleiss_kappa_from_counts(counts[index, , drop = FALSE])
    })
    bootstrap <- bootstrap[is.finite(bootstrap)]
  }
  standard_error <- if (length(bootstrap) > 1L) stats::sd(bootstrap) else NA_real_
  z_value <- if (!is.finite(standard_error) || standard_error == 0) NA_real_ else
    kappa / standard_error
  confidence_interval <- if (length(bootstrap)) {
    unname(stats::quantile(bootstrap, c(0.025, 0.975), na.rm = TRUE))
  } else c(NA_real_, NA_real_)
  summary <- data.frame(
    analysis = analysis,
    raters = paste(rater_columns, collapse = ","),
    n_items = nrow(counts),
    n_raters = length(rater_columns),
    n_categories = ncol(counts),
    fleiss_kappa = kappa,
    bootstrap_iterations = bootstrap_iterations,
    seed = seed,
    bootstrap_standard_error = standard_error,
    confidence_level = 0.95,
    ci_low = confidence_interval[1],
    ci_high = confidence_interval[2],
    z_value = z_value,
    p_value = if (is.na(z_value)) NA_real_ else 2 * stats::pnorm(-abs(z_value)),
    stringsAsFactors = FALSE
  )
  list(summary = summary, counts = counts, bootstrap = bootstrap)
}

agreement_matrix_data_frame <- function(values, row_name) {
  row_labels <- rownames(values)
  if (is.null(row_labels)) row_labels <- as.character(seq_len(nrow(values)))
  result <- data.frame(row_labels, stringsAsFactors = FALSE)
  names(result) <- row_name
  cbind(result, as.data.frame.matrix(values, stringsAsFactors = FALSE, optional = TRUE))
}

run_categorical_agreement <- function(cohen_analyses, fleiss_analyses, output_directory,
                                      bootstrap_iterations = 2000L, seed = 42L) {
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  read_ratings <- function(path) {
    if (!file.exists(path)) stop("Agreement input file does not exist: ", path, call. = FALSE)
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
                    na.strings = c("", "NA"))
  }

  cohen_results <- lapply(names(cohen_analyses), function(name) {
    specification <- cohen_analyses[[name]]
    result <- calculate_cohen_kappa(
      read_ratings(specification$path), specification$rater_1, specification$rater_2, name
    )
    confusion_path <- file.path(output_directory, paste0("cohen_kappa_", name, "_confusion.csv"))
    utils::write.csv(agreement_matrix_data_frame(result$confusion, "rater_1_category"),
                     confusion_path, row.names = FALSE, na = "")
    result$confusion_path <- confusion_path
    result
  })
  cohen_summary <- do.call(rbind, lapply(cohen_results, `[[`, "summary"))
  cohen_summary_path <- file.path(output_directory, "cohen_kappa_summary.csv")
  utils::write.csv(cohen_summary, cohen_summary_path, row.names = FALSE, na = "")

  fleiss_results <- lapply(names(fleiss_analyses), function(name) {
    specification <- fleiss_analyses[[name]]
    result <- calculate_fleiss_kappa(
      read_ratings(specification$path), specification$rater_columns, name,
      bootstrap_iterations, seed
    )
    counts_path <- file.path(output_directory, paste0("fleiss_kappa_", name, "_counts.csv"))
    count_rows <- agreement_matrix_data_frame(result$counts, "item")
    count_rows$item <- seq_len(nrow(count_rows))
    utils::write.csv(count_rows, counts_path, row.names = FALSE, na = "")
    result$counts_path <- counts_path
    result
  })
  fleiss_summary <- do.call(rbind, lapply(fleiss_results, `[[`, "summary"))
  fleiss_summary_path <- file.path(output_directory, "fleiss_kappa_summary.csv")
  utils::write.csv(fleiss_summary, fleiss_summary_path, row.names = FALSE, na = "")

  paths <- c(
    cohen_summary = cohen_summary_path,
    vapply(cohen_results, `[[`, character(1), "confusion_path"),
    fleiss_summary = fleiss_summary_path,
    vapply(fleiss_results, `[[`, character(1), "counts_path")
  )
  message("Wrote categorical agreement outputs to ",
          normalizePath(output_directory, mustWork = TRUE))
  invisible(list(cohen = cohen_results, fleiss = fleiss_results, paths = paths))
}
