const SUPPORTED_LIBRARIES = Object.freeze(["medoid", "model"]);

const DEFAULT_CONFIG = Object.freeze({
  webRModuleUrl: "../shinylive/webr/webr.mjs",
  webRBaseUrl: "../shinylive/webr/",
  packageAssetBaseUrl: "../shinylive/webr/",
  rWrapperUrl: "../r/openspecy_engine.R",
  referenceLibraryBaseUrl: "../data/",
  maxReferenceLibraryBytes: 50 * 1024 * 1024
});

let enginePromise = null;

export function supportedOpenSpecyLibraries() {
  return [...SUPPORTED_LIBRARIES];
}

export async function getOpenSpecyEngine(config = {}) {
  const mergedConfig = {
    ...DEFAULT_CONFIG,
    ...(globalThis.OpenSpecyWebRConfig || {}),
    ...config
  };

  if (!enginePromise) {
    enginePromise = (async () => {
      const engine = new OpenSpecyWebREngine(mergedConfig);
      await engine.initialize();
      return engine;
    })();
  }

  return enginePromise;
}

export class OpenSpecyWebREngine {
  constructor(config = {}) {
    this.config = { ...DEFAULT_CONFIG, ...config };
    this.webR = null;
    this.initialized = false;
    this.packageAssetsMounted = false;
    this.wrapperLoaded = false;
    this.loadedLibraries = new Set();
    this.initInfo = null;
  }

  async initialize() {
    if (this.initialized) {
      return this.initInfo;
    }

    const status = this.config.onStatus || (() => {});

    try {
      status("Preparing the Open Specy analysis engine...");
      const webRModuleUrl = this.absoluteUrl(this.config.webRModuleUrl);
      const webRBaseUrl = this.ensureTrailingSlash(this.absoluteUrl(this.config.webRBaseUrl));
      const { WebR } = await import(webRModuleUrl);

      status("This can take a little while the first time because R is loading in your browser.");
      this.webR = new WebR({
        baseUrl: webRBaseUrl,
        serviceWorkerUrl: new URL("webr-serviceworker.js", webRBaseUrl).href
      });
      await this.webR.init();

      status("Loading R analysis functions...");
      await this.mountStaticPackageAssets();
      await this.sourceOpenSpecyWrapper();
      this.initInfo = await this.evalJson("openspecy_webr_init()");
      this.initialized = true;
      return this.initInfo;
    } catch (error) {
      enginePromise = null;
      throw this.wrapError(
        error,
        "Open Specy could not start the browser analysis engine."
      );
    }
  }

  async analyzeFile(file, options = {}, onStatus = null) {
    if (!file || typeof file.arrayBuffer !== "function") {
      throw new Error("Choose a spectrum file before starting analysis.");
    }

    await this.initialize();

    const status = onStatus || this.config.onStatus || (() => {});
    const library = this.validateLibrary(options.library || "medoid");

    status("Loading the selected reference library...");
    await this.ensureReferenceLibrary(library);

    status("Preparing file in browser storage...");
    const uploadPath = await this.writeUploadFile(file);
    const optionsPath = await this.writeOptionsFile({ ...options, library });

    status("Analyzing spectra...");
    return this.evalJson(`
      .openspecy_options_json <- paste(readLines(${rString(optionsPath)}, warn = FALSE), collapse = "\\n")
      .openspecy_options <- jsonlite::fromJSON(.openspecy_options_json, simplifyVector = FALSE)
      openspecy_analyze_file(${rString(uploadPath)}, .openspecy_options)
    `);
  }

  async matchSpectrum(x, wavenumber, options = {}, onStatus = null) {
    await this.initialize();

    const status = onStatus || this.config.onStatus || (() => {});
    const library = this.validateLibrary(options.library || "medoid");

    status("Loading the selected reference library...");
    await this.ensureReferenceLibrary(library);

    const payloadPath = `/uploads/spectrum-${Date.now()}.json`;
    await this.ensureDir("/uploads");
    await this.writeTextFile(payloadPath, JSON.stringify({ x, wavenumber, options: { ...options, library } }));

    status("Analyzing spectra...");
    return this.evalJson(`
      .openspecy_payload_json <- paste(readLines(${rString(payloadPath)}, warn = FALSE), collapse = "\\n")
      .openspecy_payload <- jsonlite::fromJSON(.openspecy_payload_json, simplifyVector = FALSE)
      openspecy_match_spectrum(
        x = .openspecy_payload$x,
        wavenumber = .openspecy_payload$wavenumber,
        options = .openspecy_payload$options
      )
    `);
  }

  async mountStaticPackageAssets() {
    if (this.packageAssetsMounted) {
      return;
    }

    const packageBaseUrl = this.ensureTrailingSlash(
      this.absoluteUrl(this.config.packageAssetBaseUrl)
    );

    await this.evalRVoid(`
      .openspecy_or <- function(x, y) {
        if (is.null(x) || length(x) == 0) y else x
      }

      .openspecy_package_base_url <- ${rString(packageBaseUrl)}
      .openspecy_package_lib <- "/openspecy/webr/packages"
      dir.create(.openspecy_package_lib, recursive = TRUE, showWarnings = FALSE)
      .libPaths(unique(c(.openspecy_package_lib, .libPaths())))

      if (!requireNamespace("webr", quietly = TRUE)) {
        stop("The webR support package is not available in this runtime.", call. = FALSE)
      }
      if (exists("shim_install", envir = asNamespace("webr"), inherits = FALSE)) {
        webr::shim_install()
      }

      .openspecy_install_pkg_tgz <- function(path, lib) {
        tmp <- tempfile(fileext = ".tgz")
        on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
        utils::download.file(path, tmp, quiet = TRUE, mode = "wb")
        utils::untar(
          tmp,
          exdir = lib,
          tar = "internal",
          extras = "--no-same-permissions"
        )
      }

      .openspecy_mount_static_package_assets <- function(base_url) {
        if (!grepl("/$", base_url)) {
          base_url <- paste0(base_url, "/")
        }

        metadata_url <- paste0(base_url, "packages/metadata.rds")
        metadata_path <- file.path(.openspecy_package_lib, "metadata.rds")
        ok <- tryCatch({
          utils::download.file(metadata_url, metadata_path, quiet = TRUE, mode = "wb")
          TRUE
        }, error = function(e) FALSE)

        if (!ok || !file.exists(metadata_path)) {
          stop(
            "Could not load static webR package metadata from ", metadata_url, ". ",
            "Run the Shinylive build before the static webR build so package assets exist.",
            call. = FALSE
          )
        }

        metadata <- readRDS(metadata_path)
        invisible(lapply(metadata, function(data) {
          name <- .openspecy_or(data$name, "")
          path <- .openspecy_or(data$path, "")
          type <- .openspecy_or(data$type, "package")
          available <- isTRUE(data$cached) && length(.openspecy_or(data$assets, list())) > 0
          mountpoint <- file.path(.openspecy_package_lib, name)

          if (!nzchar(name) || !nzchar(path) || !available || file.exists(mountpoint)) {
            return(invisible(FALSE))
          }

          asset_url <- paste0(base_url, path)
          tryCatch({
            webr::mount(mountpoint, asset_url)
          }, error = function(cnd) {
            if (grepl("[.]tgz$", path)) {
              .openspecy_install_pkg_tgz(asset_url, .openspecy_package_lib)
            } else {
              stop(cnd)
            }
          })

          if (identical(type, "library")) {
            .libPaths(unique(c(.libPaths(), mountpoint)))
          }

          invisible(TRUE)
        }))
      }

      .openspecy_mount_static_package_assets(.openspecy_package_base_url)
    `);

    this.packageAssetsMounted = true;
  }

  async sourceOpenSpecyWrapper() {
    if (this.wrapperLoaded) {
      return;
    }

    const wrapperUrl = this.absoluteUrl(this.config.rWrapperUrl);
    const response = await fetch(wrapperUrl, { cache: "no-cache" });
    if (!response.ok) {
      throw new Error(`Could not load Open Specy R wrapper from ${wrapperUrl}.`);
    }

    await this.ensureDir("/r");
    await this.writeTextFile("/r/openspecy_engine.R", await response.text());
    await this.evalRVoid('source("/r/openspecy_engine.R", local = .GlobalEnv)');
    this.wrapperLoaded = true;
  }

  async ensureReferenceLibrary(name) {
    const library = this.validateLibrary(name);
    if (this.loadedLibraries.has(library)) {
      return;
    }

    const filename = `openspecy-${library}.rds`;
    const libraryUrl = new URL(
      filename,
      this.ensureTrailingSlash(this.absoluteUrl(this.config.referenceLibraryBaseUrl))
    ).href;

    const response = await fetch(libraryUrl, { cache: "force-cache" });
    if (!response.ok) {
      throw new Error(
        `Could not load the '${library}' browser reference library from ${libraryUrl}. ` +
        "The static webR site only supports medoid/model RDS assets."
      );
    }

    const contentLength = Number(response.headers.get("content-length") || 0);
    if (contentLength > this.config.maxReferenceLibraryBytes) {
      throw new Error(
        `The '${library}' browser reference library is too large for the static webR build.`
      );
    }

    const bytes = new Uint8Array(await response.arrayBuffer());
    if (bytes.byteLength > this.config.maxReferenceLibraryBytes) {
      throw new Error(
        `The '${library}' browser reference library is too large for the static webR build.`
      );
    }

    await this.ensureDir("/data");
    await this.webR.FS.writeFile(`/data/${filename}`, bytes);
    this.loadedLibraries.add(library);
  }

  async writeUploadFile(file) {
    await this.ensureDir("/uploads");
    const safeName = sanitizeFilename(file.name || "spectrum.csv");
    const path = `/uploads/${Date.now()}-${safeName}`;
    const bytes = new Uint8Array(await file.arrayBuffer());
    await this.webR.FS.writeFile(path, bytes);
    return path;
  }

  async writeOptionsFile(options) {
    await this.ensureDir("/uploads");
    const path = `/uploads/options-${Date.now()}.json`;
    await this.writeTextFile(path, JSON.stringify(options));
    return path;
  }

  async writeTextFile(path, text) {
    await this.webR.FS.writeFile(path, new TextEncoder().encode(text));
  }

  async ensureDir(path) {
    const pieces = path.split("/").filter(Boolean);
    let current = "";

    for (const piece of pieces) {
      current += `/${piece}`;
      try {
        if (typeof this.webR.FS.lookupPath === "function") {
          await this.webR.FS.lookupPath(current);
          continue;
        }
      } catch (error) {
        // Directory does not exist yet.
      }

      try {
        await this.webR.FS.mkdir(current);
      } catch (error) {
        // Another operation may have created it between lookup and mkdir.
      }
    }
  }

  async evalJson(expression) {
    const json = await this.webR.evalRString(`
      jsonlite::toJSON(
        tryCatch({
          ${expression}
        }, error = function(e) {
          list(
            ok = FALSE,
            error = list(
              message = conditionMessage(e),
              class = class(e)
            )
          )
        }),
        dataframe = "rows",
        auto_unbox = TRUE,
        na = "null",
        null = "null",
        digits = 8
      )
    `);

    const parsed = JSON.parse(json);
    if (parsed && parsed.ok === false) {
      const message = parsed.error && parsed.error.message
        ? parsed.error.message
        : "R analysis failed.";
      throw new Error(message);
    }

    return parsed;
  }

  async evalRVoid(code) {
    await this.webR.evalRVoid(code);
  }

  validateLibrary(name) {
    const library = String(name || "medoid").toLowerCase();
    if (!SUPPORTED_LIBRARIES.includes(library)) {
      throw new Error(
        `Unsupported browser reference library: '${library}'. ` +
        "The static webR build only supports 'medoid' and 'model'."
      );
    }
    return library;
  }

  absoluteUrl(path) {
    return new URL(path, document.baseURI).href;
  }

  ensureTrailingSlash(path) {
    return path.endsWith("/") ? path : `${path}/`;
  }

  wrapError(error, prefix) {
    const message = error && error.message ? error.message : String(error);
    return new Error(`${prefix} ${message}`);
  }
}

function sanitizeFilename(name) {
  const basename = String(name).split(/[\\/]/).pop() || "spectrum.csv";
  const sanitized = basename.replace(/[^A-Za-z0-9._+-]/g, "_").slice(0, 120);
  return sanitized || "spectrum.csv";
}

function rString(value) {
  return JSON.stringify(String(value));
}
