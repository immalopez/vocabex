run_registry_version <- "1.3.0"
run_cache_version <- 1L

portable_run_manifest <- function(manifest) {
  manifest$project_directory <- "."

  command <- unlist(manifest$command, use.names = FALSE)
  if (length(command)) {
    portable_basename <- function(path) basename(gsub("\\", "/", path, fixed = TRUE))
    command[[1]] <- portable_basename(command[[1]])
    file_arguments <- grep("^--file=", command)
    for (index in file_arguments) {
      command[[index]] <- paste0(
        "--file=", portable_basename(sub("^--file=", "", command[[index]]))
      )
    }
    manifest$command <- command
  }

  manifest
}

run_is_active <- function() nzchar(Sys.getenv("VOCABEX_RUN_DIR"))

run_id <- function(required = FALSE) {
  value <- Sys.getenv("VOCABEX_RUN_ID")
  if (required && !nzchar(value)) stop("VOCABEX_RUN_ID is not set", call. = FALSE)
  value
}

run_dir <- function(required = FALSE) {
  value <- Sys.getenv("VOCABEX_RUN_DIR")
  if (required && !nzchar(value)) stop("VOCABEX_RUN_DIR is not set", call. = FALSE)
  if (nzchar(value)) normalizePath(value, mustWork = FALSE) else value
}

run_path <- function(...) {
  path <- file.path(run_dir(required = TRUE), ...)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  path
}

run_input <- function(name) {
  if (run_is_active()) run_path("inputs", name) else name
}

run_output <- function(category, name, legacy = name) {
  if (run_is_active()) run_path(category, name) else legacy
}

sha256_file <- function(path) {
  if (!file.exists(path)) stop("Cannot hash missing file: ", path, call. = FALSE)
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

sha256_object <- function(object) digest::digest(object, algo = "sha256")

validate_run_data_id <- function(data_id) {
  if (length(data_id) != 1L || is.na(data_id) ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*$", data_id)) {
    stop("Invalid run data ID: ", data_id, call. = FALSE)
  }
  data_id
}

save_run_data <- function(data, data_id, filename = paste0(data_id, ".rds")) {
  if (!run_is_active()) stop("Saving identified data requires an active run", call. = FALSE)
  data_id <- validate_run_data_id(data_id)
  if (!identical(basename(filename), filename)) {
    stop("Run data filename must not contain a directory", call. = FALSE)
  }

  data_path <- run_path("inputs", "model_data", filename)
  saveRDS(data, data_path)
  metadata <- list(
    run_id = run_id(TRUE), data_id = data_id,
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    data_path = file.path("inputs", "model_data", filename),
    file_sha256 = sha256_file(data_path), object_sha256 = sha256_object(data),
    observations = if (is.data.frame(data)) nrow(data) else NULL,
    columns = if (is.data.frame(data)) ncol(data) else NULL
  )
  jsonlite::write_json(
    metadata, run_path("inputs", "model_data", paste0(data_id, ".json")),
    pretty = TRUE, auto_unbox = TRUE, null = "null"
  )
  invisible(data)
}

load_run_data <- function(data_id, expected_data = NULL) {
  if (!run_is_active()) stop("Loading identified data requires an active run", call. = FALSE)
  data_id <- validate_run_data_id(data_id)
  metadata_path <- run_path("inputs", "model_data", paste0(data_id, ".json"))
  if (!file.exists(metadata_path)) stop("Missing identified data metadata for ", data_id, call. = FALSE)

  metadata <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
  if (!identical(metadata$run_id, run_id(TRUE)) || !identical(metadata$data_id, data_id)) {
    stop("Data artifact belongs to another run or data ID", call. = FALSE)
  }
  data_path <- file.path(run_dir(TRUE), metadata$data_path)
  if (!file.exists(data_path) || !identical(metadata$file_sha256, sha256_file(data_path))) {
    stop("Data artifact checksum mismatch: ", data_id, call. = FALSE)
  }
  data <- readRDS(data_path)
  if (!identical(metadata$object_sha256, sha256_object(data))) {
    stop("Data artifact identity mismatch: ", data_id, call. = FALSE)
  }
  if (!is.null(expected_data) &&
      !identical(metadata$object_sha256, sha256_object(expected_data))) {
    stop("Saved data do not match the expected data: ", data_id, call. = FALSE)
  }
  data
}

read_run_manifest <- function(path = run_path("manifest.json")) {
  if (!file.exists(path)) stop("Missing run manifest: ", path, call. = FALSE)
  jsonlite::read_json(path, simplifyVector = FALSE)
}

write_run_manifest <- function(manifest, path = run_path("manifest.json")) {
  jsonlite::write_json(manifest, path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(manifest)
}

update_run_manifest <- function(...) {
  manifest <- read_run_manifest()
  changes <- list(...)
  for (name in names(changes)) manifest[[name]] <- changes[[name]]
  write_run_manifest(manifest)
}

file_record <- function(path, root = getwd()) {
  absolute <- normalizePath(path, mustWork = TRUE)
  root <- normalizePath(root, mustWork = TRUE)
  prefix <- paste0(root, .Platform$file.sep)
  if (!startsWith(absolute, prefix)) {
    stop("Cannot record a file outside the declared root: ", basename(absolute), call. = FALSE)
  }
  relative <- substring(absolute, nchar(prefix) + 1L)
  info <- file.info(absolute)
  list(path = relative, sha256 = sha256_file(absolute), bytes = unname(info$size))
}

snapshot_file <- function(path, category, root = getwd()) {
  record <- file_record(path, root = root)
  destination <- run_path(category, record$path)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(path, destination, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)) {
    stop("Could not snapshot ", path, call. = FALSE)
  }
  copied_hash <- sha256_file(destination)
  if (!identical(copied_hash, record$sha256)) stop("Snapshot hash mismatch for ", path, call. = FALSE)
  record$run_path <- file.path(category, record$path)
  record
}

canonical_formula_text <- function(value) {
  if (inherits(value, "brmsfit")) value <- value$formula
  if (inherits(value, "brmsformula")) value <- value$formula
  if (!inherits(value, "formula")) value <- stats::formula(value)
  environment(value) <- emptyenv()
  paste(deparse(value, width.cutoff = 500L), collapse = " ")
}

model_formula_text <- function(model) {
  canonical_formula_text(model)
}

family_record <- function(model) {
  if (inherits(model, "brmsfit")) {
    return(list(family = model$family$family, link = model$family$link))
  }
  family <- tryCatch(stats::family(model), error = function(e) NULL)
  if (is.null(family)) return(NULL)
  list(family = family$family, link = family$link)
}

model_group_counts <- function(model) {
  if (!inherits(model, "merMod")) return(NULL)
  as.list(lme4::ngrps(model))
}

tag_run_fit <- function(fit, model_id, data_hash, specification_hash, cache_key = NULL) {
  if (!run_is_active()) return(fit)
  attr(fit, "vocabex_run") <- list(
    run_id = run_id(required = TRUE), model_id = model_id,
    data_sha256 = data_hash, specification_sha256 = specification_hash,
    registry_version = run_registry_version, cache_key = cache_key
  )
  fit
}

save_run_fit <- function(fit, model_id, data, formula = stats::formula(fit),
                         priors = NULL, seed = NULL, settings = list(),
                         filename = paste0(model_id, ".rds"), cache_key = NULL,
                         cache_source = NULL) {
  formula_text <- canonical_formula_text(formula)
  prior_record <- if (is.null(priors)) NULL else as.data.frame(priors)
  data_hash <- sha256_object(data)
  specification <- list(
    formula = formula_text, family = family_record(fit), priors = prior_record,
    seed = seed, settings = settings, contrasts = getOption("contrasts")
  )
  specification_hash <- sha256_object(specification)
  fit <- tag_run_fit(fit, model_id, data_hash, specification_hash, cache_key = cache_key)
  fit_path <- run_output("fits", filename, legacy = filename)
  saveRDS(fit, fit_path)
  metadata <- list(
    run_id = if (run_is_active()) run_id(TRUE) else NULL,
    model_id = model_id, created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    fit_path = if (run_is_active()) file.path("fits", filename) else filename,
    fit_sha256 = sha256_file(fit_path), data_sha256 = data_hash,
    specification_sha256 = specification_hash, formula = formula_text,
    family = family_record(fit), priors = prior_record, seed = seed,
    settings = settings, contrasts = getOption("contrasts"),
    cache_key = cache_key, cache_source = cache_source,
    observations = tryCatch(stats::nobs(fit), error = function(e) nrow(data)),
    groups = model_group_counts(fit)
  )
  metadata_path <- run_output("models", paste0(model_id, ".json"),
                              legacy = paste0(model_id, "_metadata.json"))
  jsonlite::write_json(metadata, metadata_path, pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(fit)
}

package_version_record <- function(packages) {
  packages <- sort(unique(packages))
  versions <- lapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
    as.character(utils::packageVersion(package))
  })
  names(versions) <- packages
  if ("cmdstanr" %in% packages && requireNamespace("cmdstanr", quietly = TRUE)) {
    versions$CmdStan <- tryCatch(as.character(cmdstanr::cmdstan_version()),
                                 error = function(e) NA_character_)
  }
  versions
}

run_cache_key <- function(kind, id, inputs, settings = list(), packages = character(),
                          cache_version = run_cache_version) {
  digest::digest(list(
    cache_version = cache_version,
    kind = kind,
    id = id,
    inputs = inputs,
    settings = settings,
    package_versions = package_version_record(packages),
    r_version = R.version.string,
    contrasts = getOption("contrasts")
  ), algo = "sha256")
}

run_fit_signature <- function(fit) {
  identity <- attr(fit, "vocabex_run")
  if (!is.null(identity)) {
    return(identity[c("model_id", "data_sha256", "specification_sha256", "cache_key")])
  }
  list(object_sha256 = sha256_object(fit))
}

completed_run_directories <- function(active_run_dir = run_dir(required = TRUE)) {
  active_run_dir <- normalizePath(active_run_dir, mustWork = FALSE)
  parent <- dirname(active_run_dir)
  runs_root <- if (identical(basename(parent), ".incomplete")) dirname(parent) else parent
  candidates <- list.dirs(runs_root, recursive = FALSE, full.names = TRUE)
  candidates <- candidates[basename(candidates) != ".incomplete"]
  candidates[order(basename(candidates), decreasing = TRUE)]
}

valid_completed_manifest <- function(path) {
  manifest_path <- file.path(path, "manifest.json")
  if (!file.exists(manifest_path)) return(NULL)
  manifest <- tryCatch(jsonlite::read_json(manifest_path, simplifyVector = FALSE),
                       error = function(e) NULL)
  if (is.null(manifest) || !identical(manifest$status, "complete")) return(NULL)
  manifest
}

manifest_has_record <- function(manifest, section, relative_path, root) {
  records <- manifest[[section]]
  if (is.null(records)) return(FALSE)
  any(vapply(records, function(record) {
    identical(record$path, relative_path) &&
      file.exists(file.path(root, relative_path)) &&
      identical(record$sha256, sha256_file(file.path(root, relative_path)))
  }, logical(1)))
}

find_cached_run_fit <- function(model_id, cache_key, filename = paste0(model_id, ".rds")) {
  if (!run_is_active()) return(NULL)
  for (candidate in completed_run_directories()) {
    manifest <- valid_completed_manifest(candidate)
    if (is.null(manifest)) next
    metadata_path <- file.path(candidate, "models", paste0(model_id, ".json"))
    fit_path <- file.path(candidate, "fits", filename)
    if (!file.exists(metadata_path) || !file.exists(fit_path)) next
    metadata <- tryCatch(jsonlite::read_json(metadata_path, simplifyVector = FALSE),
                         error = function(e) NULL)
    if (is.null(metadata) || !identical(metadata$cache_key, cache_key) ||
        !identical(metadata$fit_sha256, sha256_file(fit_path)) ||
        !manifest_has_record(manifest, "models", file.path("models", paste0(model_id, ".json")), candidate)) next
    fit <- tryCatch(readRDS(fit_path), error = function(e) NULL)
    identity <- if (is.null(fit)) NULL else attr(fit, "vocabex_run")
    if (is.null(identity) || !identical(identity$run_id, manifest$run_id) ||
        !identical(identity$model_id, model_id) ||
        !identical(identity$data_sha256, metadata$data_sha256) ||
        !identical(identity$specification_sha256, metadata$specification_sha256)) next
    return(list(fit = fit, source = manifest$run_id))
  }
  NULL
}

cached_run_fit <- function(step_name, model_id, data, formula, family,
                           fit_function, priors = NULL, seed = NULL,
                           settings = list(), packages = character(),
                           filename = paste0(model_id, ".rds")) {
  prior_record <- if (is.null(priors)) NULL else as.data.frame(priors)
  key <- run_cache_key(
    "model_fit", model_id,
    inputs = list(data_sha256 = sha256_object(data), formula = canonical_formula_text(formula),
                  family = family, priors = prior_record),
    settings = c(list(seed = seed), settings), packages = packages
  )
  started_at <- Sys.time()
  cache_status <- "unknown"
  cache_source <- NULL
  if (exists("run_log", mode = "function")) run_log(paste0("Starting step '", step_name, "'"))
  tryCatch({
    cached <- find_cached_run_fit(model_id, key, filename)
    if (is.null(cached)) {
      fit <- fit_function()
      cache_status <- "calculated"
      cache_source <- NULL
    } else {
      fit <- cached$fit
      cache_status <- "cached"
      cache_source <- cached$source
    }
    fit <- save_run_fit(
      fit, model_id, data = data, formula = formula, priors = priors, seed = seed,
      settings = settings, filename = filename, cache_key = key,
      cache_source = cache_source
    )
    if (exists("record_step_timing", mode = "function")) {
      record_step_timing(step_name, started_at, cache_status = cache_status,
                         cache_source = cache_source)
    }
    fit
  }, error = function(e) {
    if (exists("record_step_timing", mode = "function")) {
      record_step_timing(step_name, started_at, status = "failed",
                         cache_status = cache_status, cache_source = cache_source,
                         error = conditionMessage(e))
    }
    stop(e)
  })
}

find_cached_run_artifact <- function(relative_path, cache_key) {
  if (!run_is_active()) return(NULL)
  active <- run_dir(TRUE)
  candidates <- c(active, completed_run_directories(active))
  for (candidate in candidates) {
    is_active_candidate <- identical(normalizePath(candidate, mustWork = FALSE), active)
    manifest <- if (is_active_candidate) read_run_manifest(file.path(candidate, "manifest.json")) else valid_completed_manifest(candidate)
    if (is.null(manifest)) next
    artifact_files <- list.files(file.path(candidate, "artifacts"), pattern = "\\.json$", full.names = TRUE)
    for (artifact_file in artifact_files) {
      record <- tryCatch(jsonlite::read_json(artifact_file, simplifyVector = FALSE),
                         error = function(e) NULL)
      if (is.null(record) || !identical(record$path, relative_path) ||
          !identical(record$settings$cache_key, cache_key)) next
      target <- file.path(candidate, relative_path)
      if (!file.exists(target) || !identical(record$sha256, sha256_file(target))) next
      if (!is_active_candidate &&
          !manifest_has_record(manifest, "diagnostics", relative_path, candidate)) next
      return(list(path = target, source = manifest$run_id))
    }
  }
  NULL
}

cached_run_rds <- function(step_name, artifact_id, relative_path, inputs,
                           compute_function, settings = list(), packages = character(),
                           source_models = character()) {
  if (!run_is_active()) return(compute_function())
  key <- run_cache_key("rds_artifact", artifact_id, inputs = inputs,
                       settings = settings, packages = packages)
  started_at <- Sys.time()
  cache_status <- "unknown"
  cache_source <- NULL
  if (exists("run_log", mode = "function")) run_log(paste0("Starting step '", step_name, "'"))
  tryCatch({
    cached <- find_cached_run_artifact(relative_path, key)
    if (is.null(cached)) {
      value <- compute_function()
      cache_status <- "calculated"
      cache_source <- NULL
    } else {
      value <- readRDS(cached$path)
      cache_status <- "cached"
      cache_source <- cached$source
    }
    destination <- run_path(relative_path)
    saveRDS(value, destination)
    record_run_artifact(
      destination, artifact_id, source_models = source_models,
      settings = c(settings, list(cache_key = key, cache_source = cache_source))
    )
    if (exists("record_step_timing", mode = "function")) {
      record_step_timing(step_name, started_at, cache_status = cache_status,
                         cache_source = cache_source)
    }
    value
  }, error = function(e) {
    if (exists("record_step_timing", mode = "function")) {
      record_step_timing(step_name, started_at, status = "failed",
                         cache_status = cache_status, cache_source = cache_source,
                         error = conditionMessage(e))
    }
    stop(e)
  })
}

load_run_fit <- function(model_id, filename = paste0(model_id, ".rds"),
                         expected_data = NULL, expected_formula = NULL) {
  if (!run_is_active()) stop("Loading an identified fit requires an active run", call. = FALSE)
  fit_path <- run_path("fits", filename)
  metadata_path <- run_path("models", paste0(model_id, ".json"))
  if (!file.exists(fit_path) || !file.exists(metadata_path)) {
    stop("Missing identified fit or metadata for ", model_id, call. = FALSE)
  }
  metadata <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
  if (!identical(metadata$run_id, run_id(TRUE))) stop("Fit belongs to another run", call. = FALSE)
  if (!identical(metadata$fit_sha256, sha256_file(fit_path))) stop("Fit checksum mismatch", call. = FALSE)
  if (!is.null(expected_formula)) {
    formula_text <- canonical_formula_text(expected_formula)
    if (!identical(metadata$formula, formula_text)) stop("Fit formula mismatch", call. = FALSE)
  }
  fit <- readRDS(fit_path)
  identity <- attr(fit, "vocabex_run")
  if (is.null(identity) || !identical(identity$run_id, run_id(TRUE)) ||
      !identical(identity$model_id, model_id)) stop("Fit identity mismatch", call. = FALSE)
  if (!is.null(expected_data) && !identical(identity$data_sha256, sha256_object(expected_data))) {
    stop("Fit data do not match the requested run data", call. = FALSE)
  }
  fit
}

record_run_artifact <- function(path, artifact_id, source_models = character(), settings = list()) {
  if (!run_is_active()) return(invisible(NULL))
  absolute <- normalizePath(path, mustWork = TRUE)
  root <- paste0(run_dir(TRUE), .Platform$file.sep)
  if (!startsWith(absolute, root)) stop("Artifact is outside the active run", call. = FALSE)
  record <- list(
    run_id = run_id(TRUE), artifact_id = artifact_id,
    path = substring(absolute, nchar(root) + 1L), sha256 = sha256_file(absolute),
    source_models = as.list(source_models), settings = settings,
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  jsonlite::write_json(record, run_path("artifacts", paste0(artifact_id, ".json")),
                       pretty = TRUE, auto_unbox = TRUE, null = "null")
  invisible(record)
}

validate_run_directory <- function(path, require_complete = TRUE) {
  path <- normalizePath(path, mustWork = TRUE)
  manifest_path <- file.path(path, "manifest.json")
  if (!file.exists(manifest_path)) stop("Missing run manifest", call. = FALSE)
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  if (require_complete && !identical(manifest$status, "complete")) {
    stop("Run is not complete", call. = FALSE)
  }
  if (!identical(manifest$run_id, basename(path))) stop("Run directory and manifest ID differ", call. = FALSE)

  verify_record_tree <- function(node) {
    if (!is.list(node)) return(invisible(NULL))
    if (!is.null(node$sha256) && (!is.null(node$path) || !is.null(node$run_path))) {
      relative <- if (!is.null(node$run_path)) node$run_path else node$path
      candidate <- file.path(path, relative)
      if (!file.exists(candidate)) stop("Missing recorded file: ", relative, call. = FALSE)
      if (!identical(node$sha256, sha256_file(candidate))) {
        stop("Recorded checksum mismatch: ", relative, call. = FALSE)
      }
      return(invisible(NULL))
    }
    for (child in node) verify_record_tree(child)
  }
  verify_record_tree(manifest$source)
  verify_record_tree(manifest$inputs)
  verify_record_tree(manifest$outputs)
  verify_record_tree(manifest$models)
  verify_record_tree(manifest$diagnostics)
  verify_record_tree(manifest$supporting_files)
  verify_record_tree(manifest$environment)
  verify_record_tree(manifest$logs)

  model_files <- list.files(file.path(path, "models"), pattern = "\\.json$", full.names = TRUE)
  for (metadata_path in model_files) {
    metadata <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
    if (!identical(metadata$run_id, manifest$run_id)) stop("Model run ID mismatch", call. = FALSE)
    fit_path <- file.path(path, metadata$fit_path)
    if (!file.exists(fit_path) || !identical(metadata$fit_sha256, sha256_file(fit_path))) {
      stop("Model fit checksum mismatch: ", metadata$model_id, call. = FALSE)
    }
    identity <- attr(readRDS(fit_path), "vocabex_run")
    if (is.null(identity) || !identical(identity$run_id, manifest$run_id) ||
        !identical(identity$model_id, metadata$model_id) ||
        !identical(identity$data_sha256, metadata$data_sha256) ||
        !identical(identity$specification_sha256, metadata$specification_sha256)) {
      stop("Embedded model identity mismatch: ", metadata$model_id, call. = FALSE)
    }
  }

  model_data_dir <- file.path(path, "inputs", "model_data")
  data_files <- if (dir.exists(model_data_dir)) {
    list.files(model_data_dir, pattern = "\\.json$", full.names = TRUE)
  } else {
    character()
  }
  for (metadata_path in data_files) {
    metadata <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
    if (!identical(metadata$run_id, manifest$run_id)) stop("Data run ID mismatch", call. = FALSE)
    data_path <- file.path(path, metadata$data_path)
    if (!file.exists(data_path) || !identical(metadata$file_sha256, sha256_file(data_path))) {
      stop("Data artifact checksum mismatch: ", metadata$data_id, call. = FALSE)
    }
    if (!identical(metadata$object_sha256, sha256_object(readRDS(data_path)))) {
      stop("Data artifact identity mismatch: ", metadata$data_id, call. = FALSE)
    }
  }

  artifact_files <- list.files(file.path(path, "artifacts"), pattern = "\\.json$", full.names = TRUE)
  for (artifact_path in artifact_files) {
    artifact <- jsonlite::read_json(artifact_path, simplifyVector = FALSE)
    if (!identical(artifact$run_id, manifest$run_id)) stop("Artifact run ID mismatch", call. = FALSE)
    target <- file.path(path, artifact$path)
    if (!file.exists(target) || !identical(artifact$sha256, sha256_file(target))) {
      stop("Artifact checksum mismatch: ", artifact$artifact_id, call. = FALSE)
    }
  }
  invisible(TRUE)
}
