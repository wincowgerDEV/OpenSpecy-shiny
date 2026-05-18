# Runtime helpers for keeping local Shiny, Shinylive, and hosted Shiny separate.

detect_runtime <- function() {
  runtime <- getOption("OpenSpecy.runtime", NULL)
  if (!is.null(runtime) && nzchar(runtime)) {
    return(runtime)
  }

  platform <- paste(
    tolower(R.version$platform %||% ""),
    tolower(Sys.info()[["sysname"]] %||% ""),
    collapse = " "
  )

  if (grepl("emscripten|wasm|webassembly", platform)) {
    return("shinylive")
  }

  "server"
}

supports_local_paths <- function() {
  shiny_files_pkg <- paste0("shiny", "Files")

  identical(detect_runtime(), "local") &&
    isTRUE(getOption("OpenSpecy.local_file_access", FALSE)) &&
    isTRUE(getOption("OpenSpecy.fast_upload", TRUE)) &&
    requireNamespace(shiny_files_pkg, quietly = TRUE)
}

openspecy_has_internet <- function() {
  if (identical(detect_runtime(), "shinylive")) {
    return(FALSE)
  }

  curl_pkg <- paste0("cu", "rl")

  if (!requireNamespace(curl_pkg, quietly = TRUE)) {
    return(FALSE)
  }

  getExportedValue(curl_pkg, "has_internet")()
}

openspecy_default_file_roots <- function(roots = NULL) {
  if (!is.null(roots)) {
    return(roots)
  }

  option_roots <- getOption("OpenSpecy.local_file_roots", NULL)
  if (!is.null(option_roots)) {
    return(option_roots)
  }

  list(
    Home = normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE),
    Working = normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  )
}

run_app <- function(app_dir = getwd(),
                    local_file_access = interactive(),
                    fast_upload = TRUE,
                    local_file_roots = NULL,
                    ...) {
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)

  old_options <- options(
    OpenSpecy.runtime = "local",
    OpenSpecy.local_file_access = isTRUE(local_file_access),
    OpenSpecy.fast_upload = isTRUE(fast_upload),
    OpenSpecy.local_file_roots = local_file_roots
  )
  on.exit(options(old_options), add = TRUE)

  shiny::runApp(appDir = app_dir, ...)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}
