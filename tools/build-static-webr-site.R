#!/usr/bin/env Rscript

root_dir <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
args <- commandArgs(trailingOnly = TRUE)
site_dir <- file.path(root_dir, "site")
analysis_src_dir <- file.path(root_dir, "src", "analysis")
public_data_dir <- file.path(root_dir, "public", "data")
dest_analysis_dir <- file.path(site_dir, "analysis")
dest_r_dir <- file.path(site_dir, "r")
dest_data_dir <- file.path(site_dir, "data")
dest_assets_dir <- file.path(site_dir, "assets")

truthy <- function(x) {
  tolower(x %||% "") %in% c("1", "true", "yes", "y")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

has_arg <- function(flag) {
  any(args %in% flag)
}

arg_value <- function(prefix, default = NULL) {
  match <- args[startsWith(args, prefix)]
  if (length(match)) {
    sub(paste0("^", prefix), "", match[[length(match)]])
  } else {
    default
  }
}

copy_path <- function(from, to) {
  if (!file.exists(from)) {
    stop("Required build input is missing: ", from, call. = FALSE)
  }

  if (dir.exists(from)) {
    if (dir.exists(to)) {
      unlink(to, recursive = TRUE, force = TRUE)
    }
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    entries <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(entries)) {
      ok <- file.copy(entries, to, overwrite = TRUE, recursive = TRUE, copy.date = TRUE)
      if (!all(ok)) {
        stop("Failed to copy ", from, " to ", to, call. = FALSE)
      }
    }
  } else {
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
    if (!ok) {
      stop("Failed to copy ", from, " to ", to, call. = FALSE)
    }
  }

  invisible(TRUE)
}

browser_library_specs <- list(
  medoid = list(
    derivative = list(
      id = "medoid_derivative",
      revision = "iThmNyMeUKhkWMvbBxQqpf1sESdQBFTs"
    ),
    nobaseline = list(
      id = "medoid_nobaseline",
      revision = "CLJCDpeFCMZw4hFUW4Y1QFT2cj23W1Yz"
    )
  ),
  model = list(
    derivative = list(
      id = "model_derivative",
      revision = "Wk7H.Zjj4coxiMGlqQlXjV5smmZou.IH"
    ),
    nobaseline = list(
      id = "model_nobaseline",
      revision = "rtJY7zQTDzRISfGpvYrU0bcj8nnRYs26"
    )
  )
)

allowed_browser_library_ids <- unlist(
  lapply(browser_library_specs, function(variants) {
    vapply(variants, `[[`, character(1), "id")
  }),
  use.names = FALSE
)

fetch_open_specy_library <- function(id, revision) {
  if (!id %in% allowed_browser_library_ids) {
    stop(
      "Refusing to export unsupported browser library: ", id,
      ". Static webR builds may only use medoid/model libraries.",
      call. = FALSE
    )
  }

  check_lib <- getExportedValue("OpenSpecy", "check_lib")
  get_lib <- getExportedValue("OpenSpecy", "get_lib")
  load_lib <- getExportedValue("OpenSpecy", "load_lib")

  missing <- tryCatch({
    check_lib(id)
    FALSE
  }, warning = function(w) TRUE, error = function(e) TRUE)

  if (isTRUE(missing)) {
    message("Downloading small browser reference library: ", id)
    get_lib(id, revision = revision, aws = TRUE)
  }

  load_lib(id)
}

export_browser_libraries <- function(output_dir) {
  if (!requireNamespace("OpenSpecy", quietly = TRUE)) {
    stop(
      "The OpenSpecy package is required to export browser reference libraries.",
      call. = FALSE
    )
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  for (library_name in names(browser_library_specs)) {
    variants <- browser_library_specs[[library_name]]
    libraries <- list()

    for (variant_name in names(variants)) {
      spec <- variants[[variant_name]]
      libraries[[variant_name]] <- fetch_open_specy_library(spec$id, spec$revision)
    }

    asset <- list(
      kind = library_name,
      browser_supported = TRUE,
      includes_full_reference_library = FALSE,
      allowed_static_libraries = c("medoid", "model"),
      generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      source_library_ids = vapply(variants, `[[`, character(1), "id"),
      libraries = libraries
    )

    saveRDS(
      asset,
      file.path(output_dir, sprintf("openspecy-%s.rds", library_name)),
      compress = "xz"
    )
  }

  invisible(TRUE)
}

browser_library_files <- function(path) {
  file.path(path, sprintf("openspecy-%s.rds", c("medoid", "model")))
}

validate_browser_library_assets <- function(path, required = FALSE) {
  assets <- browser_library_files(path)
  missing <- assets[!file.exists(assets)]

  if (length(missing)) {
    message(
      "Static webR reference library asset(s) missing: ",
      paste(basename(missing), collapse = ", ")
    )
    if (required) {
      stop(
        "Browser reference library assets are required for this build. ",
        "Generate only medoid/model assets; do not use the full reference library.",
        call. = FALSE
      )
    }
    return(FALSE)
  }

  max_mb <- as.numeric(
    arg_value("--max-library-mb=", Sys.getenv("OPENSPECY_BROWSER_LIBRARY_MAX_MB", "50"))
  )
  max_bytes <- max_mb * 1024^2
  sizes <- file.info(assets)$size
  too_large <- assets[sizes > max_bytes]

  if (length(too_large)) {
    stop(
      "Browser reference library asset(s) exceed ",
      max_mb,
      " MB: ",
      paste(basename(too_large), collapse = ", "),
      ". Larger reference libraries must not be bundled in the static site.",
      call. = FALSE
    )
  }

  TRUE
}

if (!dir.exists(site_dir)) {
  dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)
}

copy_path(analysis_src_dir, dest_analysis_dir)
copy_path(
  file.path(root_dir, "inst", "webr", "openspecy_engine.R"),
  file.path(dest_r_dir, "openspecy_engine.R")
)

if (file.exists(file.path(root_dir, "www", "logo.png"))) {
  copy_path(file.path(root_dir, "www", "logo.png"), file.path(dest_assets_dir, "logo.png"))
}

export_libraries <- has_arg("--export-libs") ||
  truthy(Sys.getenv("OPENSPECY_EXPORT_BROWSER_LIBS", "false"))
require_libraries <- has_arg("--require-libs") ||
  truthy(Sys.getenv("OPENSPECY_REQUIRE_BROWSER_LIBS", "false")) ||
  export_libraries

if (export_libraries) {
  export_browser_libraries(public_data_dir)
}

has_assets <- validate_browser_library_assets(public_data_dir, required = require_libraries)
dir.create(dest_data_dir, recursive = TRUE, showWarnings = FALSE)

if (has_assets) {
  for (asset in browser_library_files(public_data_dir)) {
    copy_path(asset, file.path(dest_data_dir, basename(asset)))
  }
}

message("Static webR analysis page written to: ", dest_analysis_dir)
