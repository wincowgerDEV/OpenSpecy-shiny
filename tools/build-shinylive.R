#!/usr/bin/env Rscript

root_dir <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
dest_dir <- file.path(root_dir, "site")

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("The shinylive package is required. Install it with install.packages('shinylive').", call. = FALSE)
}

options(
  OpenSpecy.runtime = "shinylive",
  OpenSpecy.local_file_access = FALSE,
  OpenSpecy.fast_upload = FALSE
)

copy_app_path <- function(from, to) {
  if (!file.exists(from)) {
    return(invisible(FALSE))
  }

  if (dir.exists(from)) {
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    entries <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)

    if (length(entries)) {
      ok <- file.copy(entries, to, overwrite = TRUE, recursive = TRUE, copy.date = TRUE)
      if (!all(ok)) {
        stop("Failed to copy ", from, " into the Shinylive build directory.", call. = FALSE)
      }
    }

    return(invisible(TRUE))
  }

  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(from, to, overwrite = TRUE, copy.date = TRUE)
  if (!ok) {
    stop("Failed to copy ", from, " into the Shinylive build directory.", call. = FALSE)
  }

  invisible(TRUE)
}

app_dir <- tempfile("openspecy-shinylive-app-")
dir.create(app_dir, recursive = TRUE)
on.exit(unlink(app_dir, recursive = TRUE, force = TRUE), add = TRUE)

for (file in c("global.R", "ui.R", "server.R", "config.yml")) {
  copy_app_path(file.path(root_dir, file), file.path(app_dir, file))
}

for (dir in c("R", "www", "data")) {
  copy_app_path(file.path(root_dir, dir), file.path(app_dir, dir))
}

if (dir.exists(dest_dir)) {
  unlink(dest_dir, recursive = TRUE, force = TRUE)
}

wasm_contrib_url <- "https://repo.r-wasm.org/bin/emscripten/contrib/4.4"
wasm_package_fallback_versions <- c(
  Matrix = "1.7-0",
  MASS = "7.3-61"
)

wasm_package_version <- function(package, contrib_url = wasm_contrib_url) {
  available <- tryCatch(
    utils::available.packages(contriburl = contrib_url),
    error = function(e) {
      message("Could not read WebR package index: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(available) && package %in% rownames(available)) {
    return(unname(available[package, "Version"]))
  }

  fallback <- unname(wasm_package_fallback_versions[package])
  if (!is.na(fallback)) {
    return(fallback)
  }

  stop("Could not determine a WebR package version for ", package, ".", call. = FALSE)
}

ensure_wasm_package <- function(package, contrib_url = wasm_contrib_url) {
  version <- wasm_package_version(package, contrib_url)
  tarball <- sprintf("%s_%s.tgz", package, version)
  url <- paste0(contrib_url, "/", tarball)
  package_dir <- file.path(dest_dir, "shinylive", "webr", "packages", package)
  destination <- file.path(package_dir, tarball)

  dir.create(package_dir, recursive = TRUE, showWarnings = FALSE)

  if (!file.exists(destination) || file.info(destination)$size == 0) {
    message("Adding missing WebR package: ", package, " ", version)
    utils::download.file(url, destination, mode = "wb", quiet = FALSE)
  }

  if (!file.exists(destination) || file.info(destination)$size == 0) {
    stop("Failed to download WebR package from ", url, ".", call. = FALSE)
  }

  metadata_path <- file.path(dest_dir, "shinylive", "webr", "packages", "metadata.rds")
  metadata <- readRDS(metadata_path)
  metadata[[package]] <- list(
    name = package,
    version = version,
    ref = sprintf("%s@%s", package, version),
    cached = TRUE,
    assets = list(list(filename = tarball, url = url)),
    type = "package",
    path = paste("packages", package, tarball, sep = "/")
  )
  saveRDS(metadata, metadata_path)

  invisible(TRUE)
}

loader_head <- '
<style>
  #openspecy-loader {
    align-items: center;
    background: #111827;
    color: #f8fafc;
    display: flex;
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    inset: 0;
    justify-content: center;
    opacity: 1;
    padding: 2rem;
    position: fixed;
    transition: opacity 0.45s ease;
    z-index: 2147483647;
  }
  #openspecy-loader.done {
    opacity: 0;
    pointer-events: none;
  }
  #openspecy-loader .openspecy-loader-card {
    max-width: 34rem;
    text-align: center;
  }
  #openspecy-loader .openspecy-loader-mark {
    border: 0.22rem solid rgba(248, 250, 252, 0.24);
    border-top-color: #38bdf8;
    border-radius: 999px;
    height: 3.5rem;
    margin: 0 auto 1.25rem;
    width: 3.5rem;
    animation: openspecy-loader-spin 1s linear infinite;
  }
  #openspecy-loader h1 {
    font-size: clamp(1.8rem, 4vw, 3rem);
    font-weight: 700;
    letter-spacing: 0;
    margin: 0 0 0.75rem;
  }
  #openspecy-loader p {
    color: #cbd5e1;
    font-size: 1.05rem;
    line-height: 1.55;
    margin: 0;
  }
  #openspecy-loader code {
    background: rgba(248, 250, 252, 0.12);
    border-radius: 0.35rem;
    color: #e0f2fe;
    font-size: 0.95em;
    padding: 0.12rem 0.35rem;
  }
  #openspecy-loader .openspecy-loader-help,
  #openspecy-loader .openspecy-loader-slow {
    display: none;
    margin-top: 0.9rem;
  }
  #openspecy-loader.local-file .openspecy-loader-mark {
    display: none;
  }
  #openspecy-loader.local-file .openspecy-loader-help,
  #openspecy-loader.slow .openspecy-loader-slow {
    display: block;
  }
  @keyframes openspecy-loader-spin {
    to { transform: rotate(360deg); }
  }
</style>
<script>
  window.OpenSpecyRuntime = "shinylive";
</script>
'

loader_body <- '
<div id="openspecy-loader" role="status" aria-live="polite">
  <div class="openspecy-loader-card">
    <div class="openspecy-loader-mark" aria-hidden="true"></div>
    <h1>Open Specy is getting ready</h1>
    <p>The app is loading R, packages, and analysis tools in your browser. Please refresh only if this takes longer than 2 minutes.</p>
    <p class="openspecy-loader-help">This Shinylive app must be served over <code>http://</code> or <code>https://</code>. Run <code>Rscript tools/serve-shinylive.R</code> from the project root and open the printed localhost URL.</p>
    <p class="openspecy-loader-slow">Still loading? Open the browser console for the first error and refresh after clearing the site cache.</p>
  </div>
</div>
<script>
(function () {
  const loader = document.getElementById("openspecy-loader");

  function hideLoader() {
    if (!loader) return;
    loader.classList.add("done");
    setTimeout(function () {
      if (loader && loader.parentNode) loader.parentNode.removeChild(loader);
    }, 500);
  }

  document.addEventListener("shiny:connected", function () {
    setTimeout(hideLoader, 250);
  });

  if (window.location.protocol === "file:") {
    loader.classList.add("local-file");
    return;
  }

  setTimeout(function () {
    if (loader && loader.parentNode) loader.classList.add("slow");
  }, 120000);

  window.addEventListener("load", function () {
    setTimeout(function () {
      const visibleShiny =
        document.querySelector(".shiny-bound-output") ||
        document.querySelector(".shiny-input-container") ||
        document.querySelector("#shinyapp") ||
        document.querySelector(".content-wrapper");

      if (visibleShiny) hideLoader();
    }, 1000);
  });
})();
</script>
'

shinylive::export(
  appdir = app_dir,
  destdir = dest_dir,
  template_params = list(
    title = "Open Specy",
    include_in_head = loader_head,
    include_before_body = loader_body
  )
)

# `shinylive::export()` can leave empty directories for some recommended
# packages when local R package versions do not match the WebR binary
# repository. OpenSpecy needs Matrix at namespace load time, and MASS is a
# common recommended dependency in the same path, so vendor their WASM
# tarballs explicitly without making local app startup or GitHub Actions touch
# any user-selected files.
ensure_wasm_package("Matrix")
ensure_wasm_package("MASS")

message("Shinylive site written to: ", dest_dir)
