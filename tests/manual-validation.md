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
4. Confirm it runs `Rscript tools/build-static-webr-site.R --export-libs`.
5. Confirm it uploads the generated `site/` directory and deploys to Pages.
6. Confirm the workflow does not call `run_app()` or request local user files.

## Static webR analysis path

1. Run `Rscript tools/build-shinylive.R`.
2. Run `Rscript tools/build-static-webr-site.R --export-libs`.
3. Confirm `site/analysis/index.html` exists.
4. Confirm `site/r/openspecy_engine.R` exists.
5. Confirm only these static browser library files are present in `site/data/`:
   `openspecy-medoid.rds` and `openspecy-model.rds`.
6. Confirm no full-library files such as `derivative.rds`, `nobaseline.rds`, or
   `full*.rds` are copied into `site/data/`.
7. Serve the site with `Rscript tools/serve-shinylive.R` and open
   `/analysis/`.
8. In browser devtools, confirm no request for `webr.mjs`, `R.bin.wasm`, or
   `openspecy-*.rds` happens before clicking `Analyze`.
9. Choose a small CSV spectrum file and select `Medoid`.
10. Click `Analyze` and confirm staged messages appear:
    `Preparing the Open Specy analysis engine...`,
    `Loading R analysis functions...`,
    `Loading the selected reference library...`, and `Analyzing spectra...`.
11. Confirm a table of JSON-parsed match results appears.
12. Repeat with the `Model` library.
13. In the browser console, run a direct unsupported-library call against the
    engine if needed and confirm requesting `full` errors clearly.
14. Confirm the existing Shinylive app at `/` still starts.
15. Open `src/analysis/index.html` directly and confirm the page explains that
    the source file cannot run analysis; open the generated `site/analysis/`
    page over HTTP instead.
