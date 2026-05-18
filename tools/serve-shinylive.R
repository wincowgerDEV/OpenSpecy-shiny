#!/usr/bin/env Rscript

root_dir <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
site_dir <- file.path(root_dir, "site")

if (!dir.exists(site_dir)) {
  stop("No site/ directory found. Run Rscript tools/build-shinylive.R first.", call. = FALSE)
}

if (!requireNamespace("httpuv", quietly = TRUE)) {
  stop("The httpuv package is required to serve the Shinylive site locally.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
host <- Sys.getenv("OPENSPECY_SHINYLIVE_HOST", "127.0.0.1")
port <- if (length(args)) as.integer(args[[1]]) else httpuv::randomPort()

if (is.na(port)) {
  stop("Port must be an integer.", call. = FALSE)
}

url <- sprintf("http://%s:%s", host, port)
message("Serving Shinylive site at ", url)
message("Press Ctrl+C to stop.")

httpuv::runStaticServer(
  dir = site_dir,
  host = host,
  port = port,
  browse = interactive()
)
