# Manual Validation

## Local run_app() with direct file access

1. Install `shinyFiles`.
2. Start the app with `source("R/runtime.R"); run_app(local_file_access = TRUE)`.
3. Confirm the upload area shows a local file picker.
4. Select a large spectra file and confirm the returned handle has `mode = "local_path"`.
5. Confirm the selected path is the original file path, not a Shiny temp upload path.

## Local fallback without shinyFiles

1. Start the app without `shinyFiles` installed, or run `run_app(local_file_access = FALSE)`.
2. Confirm the standard `fileInput()` control is shown.
3. Upload a supported spectra file and confirm the handle has `mode = "shiny_upload"`.

## Shinylive export

1. Run `Rscript tools/build-shinylive.R`.
2. Confirm `site/index.html` includes `Open Specy is getting ready`.
3. Confirm the message says to refresh only if loading takes longer than 2 minutes.
4. Confirm `site/shinylive/webr/packages/Matrix/` and `site/shinylive/webr/packages/MASS/` each contain a non-empty `.tgz` file.
5. Confirm `site/shinylive/webr/packages/metadata.rds` includes `path` entries for `Matrix` and `MASS`, for example with:
   `Rscript -e "x <- readRDS('site/shinylive/webr/packages/metadata.rds'); print(x$Matrix$path); print(x$MASS$path)"`
6. Do not open `site/index.html` directly with `file://`; Shinylive needs an HTTP server.
7. Run `Rscript tools/serve-shinylive.R` and open the printed localhost URL.
8. If you are retesting after a rebuild, use a fresh port or clear the browser site data so the previous service worker cache is not reused.
9. Confirm the loading page disappears after the app connects.
10. Confirm direct `file://` loading shows the local-server diagnostic.
11. Confirm no local `shinyFiles` picker is required during export or browser loading.

## GitHub Pages workflow

1. Run the `Build and deploy Shinylive app` workflow manually.
2. Confirm it installs R dependencies with `install.packages()`.
3. Confirm it runs `Rscript tools/build-shinylive.R`.
4. Confirm it uploads the generated `site/` directory and deploys to Pages.
5. Confirm the workflow does not call `run_app()` or request local user files.
