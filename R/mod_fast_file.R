# Unified upload helpers.
#
# The app can receive spectra in three runtime modes:
# - local_path: run_app(local_file_access = TRUE) with the optional local file
#   picker package installed. The local R process reads the selected file
#   directly from disk and avoids Shiny's temporary upload copy for large files.
# - shiny_upload: standard Shiny fileInput(). This is the safe fallback for
#   hosted/server Shiny and for Shinylive when no browser VFS bridge is found.
# - shinylive_vfs: a guarded browser/webR fast path. JavaScript writes the
#   selected browser file into webR's virtual filesystem and sends R only the
#   virtual path and metadata, not the large file contents.

openspecy_supported_filetypes <- function() {
  c("", "csv", "tsv", "h5", "txt", "img", "dx", "hdr", "dat", "rds", "json",
    "yml", "zip", "asp", "jdx", "spc", "0", "spa", "RData")
}

openspecy_upload_accept <- function() {
  c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    ".csv", ".asp", ".tsv", ".spc", ".jdx", ".dx", ".RData",
    ".spa", ".0", ".zip", ".img", ".h5", ".txt",
    ".json", ".rds", ".yml", ".hdr", ".dat"
  )
}

file_handle_type <- function(path, name = basename(path)) {
  ext <- tolower(tools::file_ext(name %||% path))
  ifelse(
    ext %in% c("csv", "tsv", "txt"), "csv",
    ifelse(
      ext %in% c("rds", "rdata"), "rds",
      ifelse(ext %in% c("dat", "hdr"), ext, "binary")
    )
  )
}

make_file_handle <- function(mode = c("local_path", "shiny_upload", "shinylive_vfs"),
                             path = NULL,
                             name = NULL,
                             size = NULL,
                             type = NULL,
                             upload = NULL) {
  mode <- match.arg(mode)

  if (!is.null(upload)) {
    path <- upload$datapath
    name <- upload$name
    size <- upload$size
  }

  path <- as.character(path %||% character())
  name <- as.character(name %||% basename(path))

  if (length(path) == 0 || any(!nzchar(path))) {
    return(NULL)
  }

  if (is.null(size)) {
    size <- suppressWarnings(file.info(path)$size)
  }

  if (is.null(type)) {
    type <- file_handle_type(path, name)
  }

  structure(
    list(
      mode = mode,
      path = unname(path),
      name = unname(name),
      size = unname(as.numeric(size)),
      type = unname(as.character(type))
    ),
    class = c("openspecy_file_handle", "list")
  )
}

make_shinylive_vfs_file_handle <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NULL)
  }

  if (is.data.frame(value)) {
    return(make_file_handle(
      mode = "shinylive_vfs",
      path = value$path,
      name = value$name,
      size = value$size,
      type = value$type
    ))
  }

  items <- if (!is.null(value$path)) list(value) else value
  path <- vapply(items, function(x) as.character(x$path %||% ""), character(1))
  name <- vapply(items, function(x) as.character(x$name %||% basename(x$path)), character(1))
  size <- vapply(items, function(x) as.numeric(x$size %||% NA_real_), numeric(1))
  type <- vapply(items, function(x) as.character(x$type %||% file_handle_type(x$path, x$name)), character(1))

  make_file_handle(
    mode = "shinylive_vfs",
    path = path,
    name = name,
    size = size,
    type = type
  )
}

file_handle_supported <- function(file_handle,
                                  filetypes = openspecy_supported_filetypes()) {
  if (is.null(file_handle)) {
    return(FALSE)
  }

  filetypes <- tolower(filetypes[nzchar(filetypes)])
  ext <- tolower(tools::file_ext(file_handle$name %||% file_handle$path))

  all(ext %in% filetypes | grepl("^[0-9]+$", ext))
}

file_handle_has_extensions <- function(file_handle, extensions) {
  ext <- tolower(tools::file_ext(file_handle$name %||% file_handle$path))
  any(ext %in% tolower(extensions))
}

read_uploaded_spectra <- function(file_handle,
                                  conform_decision = FALSE,
                                  conform_res = 8) {
  if (is.null(file_handle)) {
    stop("No uploaded spectra file handle was provided.", call. = FALSE)
  }

  # All current modes provide a readable path. The mode is kept explicit so
  # future binary, RDS, or ENVI readers can dispatch without changing UI code.
  read_any(file = as.character(file_handle$path)) |>
    c_spec(
      range = "common",
      res = if (isTRUE(conform_decision)) conform_res else 8
    ) |>
    manage_na(ig = c(NA, 0), type = "remove")
}

fast_file_ui <- function(id,
                         label = "Upload data",
                         multiple = TRUE,
                         filetypes = openspecy_supported_filetypes(),
                         accept = openspecy_upload_accept(),
                         placeholder = ".csv, .zip, .asp, .jdx, .spc, .spa, ...") {
  ns <- NS(id)
  local_enabled <- supports_local_paths()
  shiny_files_pkg <- paste0("shiny", "Files")

  div(
    id = ns("container"),
    class = "openspecy-fast-file",
    `data-openspecy-fast-file` = "true",
    tags$head(
      tags$style(HTML("
        .openspecy-fast-file .progress { margin-bottom: 0; }
        .openspecy-upload-status {
          color: #adb5bd;
          font-size: 0.85rem;
          margin-top: 0.35rem;
          min-height: 1.2rem;
        }
        .openspecy-local-upload button {
          width: 100%;
        }
      ")),
      tags$script(HTML(openspecy_fast_file_js()))
    ),
    div(
      class = "openspecy-local-upload",
      `data-openspecy-local` = "true",
      style = if (local_enabled) NULL else "display:none;",
      if (local_enabled) {
        getExportedValue(shiny_files_pkg, paste0("shiny", "FilesButton"))(
          id = ns("local_file_select"),
          label = label,
          title = "Select spectra files",
          multiple = multiple,
          buttonType = "default"
        )
      },
      textOutput(ns("local_file_name"), container = span)
    ),
    div(
      class = "openspecy-shinylive-upload",
      `data-openspecy-shinylive` = "true",
      style = "display:none;",
      tags$label(label, `for` = ns("shinylive_file")),
      tags$input(
        id = ns("shinylive_file"),
        type = "file",
        multiple = if (multiple) "multiple" else NULL,
        accept = paste(accept, collapse = ","),
        class = "form-control"
      ),
      div(id = ns("shinylive_status"), class = "openspecy-upload-status")
    ),
    div(
      class = "openspecy-standard-upload",
      `data-openspecy-standard` = "true",
      style = if (local_enabled) "display:none;" else NULL,
      fileInput(
        ns("file"),
        NULL,
        multiple = multiple,
        placeholder = placeholder,
        accept = accept
      )
    )
  )
}

fast_file_server <- function(id,
                             roots = NULL,
                             filetypes = openspecy_supported_filetypes()) {
  moduleServer(id, function(input, output, session) {
    selected_handle <- reactiveVal(NULL)
    shiny_files_pkg <- paste0("shiny", "Files")

    if (supports_local_paths()) {
      roots <- openspecy_default_file_roots(roots)
      getExportedValue(shiny_files_pkg, paste0("shiny", "FileChoose"))(
        input,
        "local_file_select",
        roots = roots,
        session = session,
        filetypes = filetypes
      )

      observeEvent(input$local_file_select, {
        selected <- getExportedValue(shiny_files_pkg, "parseFilePaths")(roots, input$local_file_select)
        if (!nrow(selected)) {
          return(NULL)
        }

        selected_handle(make_file_handle(
          mode = "local_path",
          path = selected$datapath,
          name = selected$name
        ))
      }, ignoreNULL = TRUE)
    }

    observeEvent(input$file, {
      selected_handle(make_file_handle(mode = "shiny_upload", upload = input$file))
    }, ignoreNULL = TRUE)

    observeEvent(input$shinylive_handles, {
      selected_handle(make_shinylive_vfs_file_handle(input$shinylive_handles))
    }, ignoreNULL = TRUE)

    output$local_file_name <- renderText({
      handle <- selected_handle()
      if (is.null(handle) || !identical(handle$mode, "local_path")) {
        return("")
      }
      paste(handle$name, collapse = ", ")
    })

    selected_handle
  })
}

reset_fast_file_upload <- function(session, id) {
  ns <- NS(id)
  shinyjs::reset(ns("file"))
  session$sendCustomMessage("openspecyFastFileReset", list(id = ns("")))
}

openspecy_fast_file_js <- function() {
  "
  (function () {
    if (window.OpenSpecyFastFileLoaded) return;
    window.OpenSpecyFastFileLoaded = true;

    function findWebRFS() {
      const candidates = [
        window.webR,
        window.shinylive && window.shinylive.webR,
        window.Shiny && window.Shiny.shinylive && window.Shiny.shinylive.webR
      ];

      for (const candidate of candidates) {
        if (candidate && candidate.FS && typeof candidate.FS.writeFile === 'function') {
          return candidate.FS;
        }
      }

      return null;
    }

    function safeName(name) {
      return String(name || 'upload.dat').replace(/[^A-Za-z0-9._+-]/g, '_');
    }

    async function ensureUploadDir(fs) {
      try {
        if (typeof fs.lookupPath === 'function') {
          await fs.lookupPath('/uploads');
          return;
        }
      } catch (error) {}

      try {
        await fs.mkdir('/uploads');
      } catch (error) {}
    }

    function setVisible(el, visible) {
      if (!el) return;
      el.style.display = visible ? '' : 'none';
    }

    function initContainer(container) {
      if (!container || container.dataset.openspecyBound === 'true') return;
      container.dataset.openspecyBound = 'true';

      const live = container.querySelector('[data-openspecy-shinylive]');
      const standard = container.querySelector('[data-openspecy-standard]');
      const input = live && live.querySelector('input[type=\"file\"]');
      const status = live && live.querySelector('.openspecy-upload-status');

      if (window.OpenSpecyRuntime !== 'shinylive') return;

      const fs = findWebRFS();
      if (!fs || !input || !window.Shiny || typeof window.Shiny.setInputValue !== 'function') {
        setVisible(live, false);
        setVisible(standard, true);
        return;
      }

      setVisible(live, true);
      setVisible(standard, false);

      input.addEventListener('change', async function () {
        const files = Array.from(input.files || []);
        if (!files.length) return;

        if (status) status.textContent = 'Preparing file in browser storage...';

        try {
          await ensureUploadDir(fs);
          const handles = [];
          for (let i = 0; i < files.length; i += 1) {
            const file = files[i];
            const name = safeName(file.name);
            const path = '/uploads/' + Date.now() + '-' + i + '-' + name;
            const bytes = new Uint8Array(await file.arrayBuffer());
            await fs.writeFile(path, bytes);
            handles.push({
              mode: 'shinylive_vfs',
              path: path,
              name: file.name,
              size: file.size,
              type: (file.name.split('.').pop() || 'binary').toLowerCase()
            });
          }

          window.Shiny.setInputValue(
            input.id.replace(/-shinylive_file$/, '-shinylive_handles'),
            handles,
            { priority: 'event' }
          );
          if (status) status.textContent = files.map(function (file) { return file.name; }).join(', ');
        } catch (error) {
          setVisible(live, false);
          setVisible(standard, true);
          if (status) status.textContent = '';
          console.warn('OpenSpecy Shinylive VFS upload unavailable; using fileInput fallback.', error);
        }
      });
    }

    function initAll() {
      document
        .querySelectorAll('[data-openspecy-fast-file=\"true\"]')
        .forEach(initContainer);
    }

    document.addEventListener('shiny:connected', initAll);
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', initAll);
    } else {
      initAll();
    }

    if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === 'function') {
      window.Shiny.addCustomMessageHandler('openspecyFastFileReset', function (message) {
        const container = document.getElementById(message.id + 'container');
        if (!container) return;
        const inputs = container.querySelectorAll('input[type=\"file\"]');
        inputs.forEach(function (input) { input.value = ''; });
        const status = container.querySelector('.openspecy-upload-status');
        if (status) status.textContent = '';
      });
    }
  })();
  "
}
