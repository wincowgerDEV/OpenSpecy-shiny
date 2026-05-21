# Browser-specific Open Specy wrappers for static webR usage.
#
# These functions intentionally avoid Shiny concepts. JavaScript owns the UI,
# user files, progress messages, and rendering; R receives coarse-grained calls
# and returns JSON-friendly results.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

openspecy_available_libraries <- function() {
  c("medoid", "model")
}

openspecy_validate_browser_library <- function(name) {
  name <- as.character(name %||% "medoid")[[1]]
  supported <- openspecy_available_libraries()

  if (!name %in% supported) {
    stop(
      "Unsupported browser reference library: '", name, "'. ",
      "The static webR build only supports: ",
      paste(shQuote(supported), collapse = ", "),
      ". The full Open Specy reference library is intentionally not bundled.",
      call. = FALSE
    )
  }

  name
}

openspecy_browser_library_path <- function(name) {
  name <- openspecy_validate_browser_library(name)
  switch(
    name,
    medoid = "/data/openspecy-medoid.rds",
    model = "/data/openspecy-model.rds"
  )
}

openspecy_required_functions <- function() {
  c(
    "as_OpenSpecy",
    "c_spec",
    "conform_spec",
    "filter_spec",
    "manage_na",
    "match_spec",
    "process_spec",
    "read_any",
    "restrict_range",
    "sig_noise"
  )
}

openspecy_namespace_function <- function(name) {
  if (!requireNamespace("OpenSpecy", quietly = TRUE)) {
    stop(
      "The OpenSpecy package is not available in webR. ",
      "Build or mount the static webR package assets before analysis starts.",
      call. = FALSE
    )
  }

  ns <- asNamespace("OpenSpecy")
  if (!exists(name, envir = ns, inherits = FALSE)) {
    stop("Required OpenSpecy function is not available: ", name, call. = FALSE)
  }

  get(name, envir = ns, inherits = FALSE)
}

openspecy_webr_init <- function() {
  required_packages <- c("OpenSpecy", "jsonlite")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages)) {
    stop(
      "Missing required webR package(s): ",
      paste(missing_packages, collapse = ", "),
      ". These packages must be available as static webR assets.",
      call. = FALSE
    )
  }

  required_functions <- openspecy_required_functions()
  missing_functions <- required_functions[
    !vapply(required_functions, function(name) {
      exists(name, envir = asNamespace("OpenSpecy"), inherits = FALSE)
    }, logical(1))
  ]

  if (length(missing_functions)) {
    stop(
      "The loaded OpenSpecy package is missing required function(s): ",
      paste(missing_functions, collapse = ", "),
      call. = FALSE
    )
  }

  options(
    OpenSpecy.runtime = "webr-static",
    OpenSpecy.local_file_access = FALSE,
    OpenSpecy.fast_upload = FALSE
  )

  list(
    ok = TRUE,
    runtime = "webr-static",
    packages = required_packages,
    functions = required_functions,
    available_libraries = openspecy_available_libraries(),
    mounted_libraries = stats::setNames(
      vapply(
        openspecy_available_libraries(),
        function(name) file.exists(openspecy_browser_library_path(name)),
        logical(1)
      ),
      openspecy_available_libraries()
    )
  )
}

openspecy_load_browser_library <- function(name) {
  name <- openspecy_validate_browser_library(name)
  path <- openspecy_browser_library_path(name)

  if (!file.exists(path)) {
    stop(
      "The '", name, "' browser reference library is not mounted at ", path, ". ",
      "Only medoid/model assets are supported in the static webR site.",
      call. = FALSE
    )
  }

  readRDS(path)
}

openspecy_option <- function(options, name, default = NULL) {
  options <- as.list(options %||% list())
  value <- options[[name]]

  if (is.null(value) || length(value) == 0) {
    default
  } else {
    value
  }
}

openspecy_option_character <- function(options, name, default = NULL) {
  value <- openspecy_option(options, name, default)
  if (is.null(value)) {
    return(NULL)
  }
  as.character(value)[[1]]
}

openspecy_option_logical <- function(options, name, default = FALSE) {
  value <- openspecy_option(options, name, default)
  isTRUE(value) || identical(tolower(as.character(value)[[1]]), "true")
}

openspecy_option_number <- function(options, name, default = NA_real_) {
  value <- suppressWarnings(as.numeric(openspecy_option(options, name, default))[[1]])
  if (is.na(value)) {
    default
  } else {
    value
  }
}

openspecy_option_integer <- function(options, name, default = NA_integer_) {
  as.integer(round(openspecy_option_number(options, name, default)))
}

openspecy_library_strategy <- function(options) {
  strategy <- openspecy_option_character(options, "library_strategy", NULL)
  strategy <- strategy %||% openspecy_option_character(options, "id_strategy", NULL)

  if (is.null(strategy)) {
    strategy <- if (openspecy_option_logical(options, "baseline", FALSE)) {
      "nobaseline"
    } else {
      "derivative"
    }
  }

  strategy <- tolower(strategy)
  if (grepl("nobaseline|no_baseline|no-baseline", strategy)) {
    "nobaseline"
  } else if (grepl("deriv|smooth", strategy)) {
    "derivative"
  } else {
    "nobaseline"
  }
}

openspecy_select_library_variant <- function(asset, name, options = list()) {
  strategy <- openspecy_library_strategy(options)

  candidates <- if (is.list(asset) && !is.null(asset$libraries)) {
    asset$libraries
  } else if (is.list(asset) && any(c("derivative", "nobaseline") %in% names(asset))) {
    asset
  } else {
    NULL
  }

  library <- if (!is.null(candidates)) {
    candidates[[strategy]] %||% candidates[[switch(strategy, derivative = "deriv", strategy)]]
  } else {
    asset
  }

  if (is.null(library)) {
    stop(
      "The '", name, "' browser library asset does not contain the requested ",
      strategy, " variant.",
      call. = FALSE
    )
  }

  if (identical(name, "model") && is.list(library) && is.null(library$spectra)) {
    spectrum_type <- tolower(openspecy_option_character(options, "spectrum_type", "both"))
    spectrum_type <- sub("^ftir$", "ftir", spectrum_type)
    spectrum_type <- sub("^raman$", "raman", spectrum_type)

    if (spectrum_type %in% names(library)) {
      return(library[[spectrum_type]])
    }
    if ("both" %in% names(library)) {
      return(library[["both"]])
    }
  }

  library
}

openspecy_window_points <- function(wavenumber, window_width) {
  ns <- if (requireNamespace("OpenSpecy", quietly = TRUE)) {
    asNamespace("OpenSpecy")
  } else {
    NULL
  }

  if (!is.null(ns) && exists("calc_window_points", envir = ns, inherits = FALSE)) {
    return(get("calc_window_points", envir = ns)(wavenumber, window_width))
  }

  step <- stats::median(abs(diff(sort(unique(as.numeric(wavenumber))))), na.rm = TRUE)
  if (!is.finite(step) || step <= 0) {
    return(9L)
  }

  points <- max(3L, as.integer(round(window_width / step)))
  if (points %% 2 == 0) {
    points <- points + 1L
  }
  points
}

openspecy_read_browser_file <- function(path, options = list()) {
  if (!file.exists(path)) {
    stop("Uploaded file is not available in the webR filesystem: ", path, call. = FALSE)
  }

  read_any <- openspecy_namespace_function("read_any")
  c_spec <- openspecy_namespace_function("c_spec")
  manage_na <- openspecy_namespace_function("manage_na")

  spectra <- read_any(file = path)

  conform <- openspecy_option_logical(options, "conform", TRUE)
  if (conform) {
    spectra <- c_spec(
      spectra,
      range = openspecy_option_character(options, "conform_range", "common"),
      res = openspecy_option_number(options, "conform_res", 8)
    )
  }

  manage_na(spectra, ig = c(NA, 0), type = "remove")
}

openspecy_preprocess_spectra <- function(spectra, options = list()) {
  process_spec <- openspecy_namespace_function("process_spec")

  smooth <- openspecy_option_logical(options, "smooth", FALSE)
  baseline <- openspecy_option_logical(options, "baseline", FALSE)
  active <- openspecy_option_logical(options, "preprocess", FALSE) || smooth || baseline

  if (!active) {
    return(spectra)
  }

  derivative <- openspecy_option_integer(options, "derivative", 0L)
  window_width <- openspecy_option_number(options, "smoother_window", 90)
  window_points <- openspecy_window_points(spectra$wavenumber, window_width)

  process_spec(
    x = spectra,
    active = TRUE,
    adj_intens = openspecy_option_logical(options, "adjust_intensity", FALSE),
    adj_intens_args = list(
      type = openspecy_option_character(options, "intensity_correction", "none")
    ),
    conform_spec = FALSE,
    restrict_range = openspecy_option_logical(options, "restrict_range", FALSE),
    restrict_range_args = list(
      min = openspecy_option_number(options, "range_min", min(spectra$wavenumber, na.rm = TRUE)),
      max = openspecy_option_number(options, "range_max", max(spectra$wavenumber, na.rm = TRUE))
    ),
    flatten_range = openspecy_option_logical(options, "flatten_range", FALSE),
    flatten_range_args = list(
      min = openspecy_option_number(options, "flatten_min", 2200),
      max = openspecy_option_number(options, "flatten_max", 2400)
    ),
    subtr_baseline = baseline,
    subtr_baseline_args = list(
      type = "polynomial",
      degree = openspecy_option_integer(options, "baseline_degree", 8L),
      raw = FALSE,
      refit_at_end = openspecy_option_logical(options, "baseline_refit", TRUE),
      iterations = openspecy_option_integer(options, "baseline_iterations", 10L),
      baseline = NULL
    ),
    smooth_intens = smooth || derivative > 0L,
    smooth_intens_args = list(
      polynomial = openspecy_option_integer(options, "smoother_polynomial", 3L),
      window = window_points,
      derivative = derivative,
      abs = openspecy_option_logical(options, "derivative_abs", FALSE)
    ),
    make_rel = openspecy_option_logical(options, "make_relative", TRUE)
  )
}

openspecy_spectra_summary <- function(spectra) {
  n_spectra <- if (!is.null(spectra$spectra)) {
    ncol(as.data.frame(spectra$spectra))
  } else {
    NA_integer_
  }

  list(
    n_spectra = n_spectra,
    n_wavenumber = length(spectra$wavenumber %||% numeric()),
    wavenumber_min = suppressWarnings(min(spectra$wavenumber, na.rm = TRUE)),
    wavenumber_max = suppressWarnings(max(spectra$wavenumber, na.rm = TRUE))
  )
}

openspecy_top_correlation_matches <- function(correlation, spectra, library, options = list()) {
  if (is.null(dim(correlation))) {
    correlation <- matrix(correlation, ncol = 1)
  }

  spectrum_names <- colnames(correlation) %||% names(as.data.frame(spectra$spectra))
  if (is.null(spectrum_names) || length(spectrum_names) != ncol(correlation)) {
    spectrum_names <- paste0("spectrum_", seq_len(ncol(correlation)))
  }

  sample_names <- rownames(correlation) %||% names(as.data.frame(library$spectra))
  if (is.null(sample_names) || length(sample_names) != nrow(correlation)) {
    sample_names <- paste0("library_", seq_len(nrow(correlation)))
  }

  metadata <- as.data.frame(library$metadata %||% data.frame())
  meta_key <- if ("sample_name" %in% names(metadata)) {
    as.character(metadata$sample_name)
  } else {
    rownames(metadata) %||% character()
  }

  metadata_value <- function(field, idx) {
    if (field %in% names(metadata) && length(idx)) {
      as.character(metadata[[field]][idx])
    } else {
      rep(NA_character_, length(idx))
    }
  }

  top_n <- max(1L, openspecy_option_integer(options, "top_n", 5L))
  min_match <- openspecy_option_number(options, "min_match", -Inf)
  rows <- list()

  for (column in seq_len(ncol(correlation))) {
    values <- as.numeric(correlation[, column])
    order_idx <- order(values, decreasing = TRUE, na.last = NA)
    if (!length(order_idx)) {
      next
    }

    order_idx <- order_idx[seq_len(min(top_n, length(order_idx)))]
    selected_samples <- sample_names[order_idx]
    meta_idx <- match(selected_samples, meta_key)
    match_values <- signif(values[order_idx], 4)

    keep <- is.na(match_values) | match_values >= min_match
    if (!any(keep)) {
      next
    }

    rows[[length(rows) + 1L]] <- data.frame(
      object_id = spectrum_names[[column]],
      rank = seq_along(order_idx),
      sample_name = selected_samples,
      material_class = metadata_value("material_class", meta_idx),
      spectrum_identity = metadata_value("spectrum_identity", meta_idx),
      organization = metadata_value("organization", meta_idx),
      spectrum_type = metadata_value("spectrum_type", meta_idx),
      match_val = match_values,
      passed_threshold = match_values >= min_match,
      stringsAsFactors = FALSE
    )[keep, , drop = FALSE]
  }

  if (!length(rows)) {
    return(data.frame(
      object_id = character(),
      rank = integer(),
      sample_name = character(),
      material_class = character(),
      spectrum_identity = character(),
      organization = character(),
      spectrum_type = character(),
      match_val = numeric(),
      passed_threshold = logical(),
      stringsAsFactors = FALSE
    ))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

openspecy_model_matches <- function(spectra, library, options = list()) {
  as_OpenSpecy <- openspecy_namespace_function("as_OpenSpecy")
  conform_spec <- openspecy_namespace_function("conform_spec")
  match_spec <- openspecy_namespace_function("match_spec")

  variables <- suppressWarnings(as.numeric(unique(library$all_variables)))
  if (!length(variables) || all(is.na(variables))) {
    stop("The selected model library does not expose usable all_variables.", call. = FALSE)
  }

  fill_values <- rep.int(mean(unlist(spectra$spectra), na.rm = TRUE), length(variables))
  fill <- as_OpenSpecy(variables, spectra = data.frame(mean = fill_values))
  conformed <- conform_spec(spectra, range = fill$wavenumber, res = NULL)
  prediction <- match_spec(conformed, library = library, na.rm = TRUE, fill = fill)

  if (is.data.frame(prediction)) {
    out <- prediction
  } else {
    values <- prediction$value %||% prediction$posterior %||% numeric()
    names_out <- prediction$name %||% prediction$class %||% names(values)
    object_id <- names(as.data.frame(spectra$spectra))
    n <- max(length(values), length(names_out), length(object_id))

    out <- data.frame(
      object_id = rep(object_id %||% paste0("spectrum_", seq_len(n)), length.out = n),
      material_class = rep(names_out %||% NA_character_, length.out = n),
      match_val = rep(as.numeric(values), length.out = n),
      stringsAsFactors = FALSE
    )
  }

  if (!"rank" %in% names(out)) {
    out$rank <- 1L
  }
  if (!"passed_threshold" %in% names(out)) {
    min_match <- openspecy_option_number(options, "min_match", -Inf)
    out$passed_threshold <- as.numeric(out$match_val) >= min_match
  }

  out
}

openspecy_run_match <- function(spectra, options = list()) {
  library_name <- openspecy_validate_browser_library(
    openspecy_option_character(options, "library", "medoid")
  )
  asset <- openspecy_load_browser_library(library_name)
  library <- openspecy_select_library_variant(asset, library_name, options)

  if (!identical(library_name, "model")) {
    restrict_range <- openspecy_namespace_function("restrict_range")
    filter_spec <- openspecy_namespace_function("filter_spec")
    match_spec <- openspecy_namespace_function("match_spec")

    library <- restrict_range(
      library,
      min = min(spectra$wavenumber, na.rm = TRUE),
      max = max(spectra$wavenumber, na.rm = TRUE),
      make_rel = FALSE
    )
    library <- filter_spec(
      library,
      logic = !vapply(library$spectra, function(x) all(is.na(x)), logical(1))
    )

    spectrum_type <- tolower(openspecy_option_character(options, "spectrum_type", "both"))
    if (!identical(spectrum_type, "both") &&
        is.data.frame(library$metadata) &&
        "spectrum_type" %in% names(library$metadata)) {
      library <- filter_spec(
        library,
        logic = tolower(library$metadata$spectrum_type) == spectrum_type
      )
    }

    correlation <- match_spec(spectra, library = library, na.rm = TRUE)
    matches <- openspecy_top_correlation_matches(correlation, spectra, library, options)
  } else {
    matches <- openspecy_model_matches(spectra, library, options)
  }

  list(
    ok = TRUE,
    library = library_name,
    strategy = openspecy_library_strategy(options),
    match_method = if (identical(library_name, "model")) "model" else "correlation",
    matches = matches,
    summary = openspecy_spectra_summary(spectra)
  )
}

openspecy_analyze_file <- function(path, options = list()) {
  warnings <- character()

  result <- withCallingHandlers({
    spectra <- openspecy_read_browser_file(path, options)
    spectra <- openspecy_preprocess_spectra(spectra, options)
    openspecy_run_match(spectra, options)
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  result$file <- list(
    path = path,
    name = basename(path),
    size = suppressWarnings(file.info(path)$size)
  )
  result$warnings <- unique(warnings)
  result
}

openspecy_match_spectrum <- function(x, wavenumber, options = list()) {
  as_OpenSpecy <- openspecy_namespace_function("as_OpenSpecy")
  manage_na <- openspecy_namespace_function("manage_na")
  warnings <- character()

  withCallingHandlers({
    spectra <- as.data.frame(x)
    if (!nrow(spectra) && length(x)) {
      spectra <- data.frame(intensity = as.numeric(x))
    }

    spec <- as_OpenSpecy(as.numeric(wavenumber), spectra = spectra)
    spec <- manage_na(spec, ig = c(NA, 0), type = "remove")
    spec <- openspecy_preprocess_spectra(spec, options)
    result <- openspecy_run_match(spec, options)
    result$warnings <- unique(warnings)
    result
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
}
