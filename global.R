# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
translate <- file.exists("www/googletranslate.html")

#remotes::install_github("wincowgerDEV/OpenSpecy-package@vignettes")

# Libraries ----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(data.table)
library(DT)
library(digest)
#library(curl)
#library(loggit)
library(bs4Dash)
library(ggplot2)
library(reshape2)

library(OpenSpecy)
#library(glmnet)

# App metadata ----
metadata_file <- ".openspecy-shiny-metadata.rds"

read_app_metadata <- function(path = metadata_file) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch(readRDS(path), error = function(...) NULL)
}

first_nonempty_string <- function(value) {
  if (is.null(value) || !length(value)) {
    return(NULL)
  }

  candidate <- value[1]
  if (is.null(candidate) || is.na(candidate)) {
    return(NULL)
  }

  candidate <- trimws(as.character(candidate))
  if (!nzchar(candidate)) {
    return(NULL)
  }

  candidate
}

format_metadata_timestamp <- function(value) {
  if (is.null(value) || !length(value)) {
    return(NULL)
  }

  if (inherits(value, "POSIXt")) {
    return(format(value[1], "%Y-%m-%d %H:%M %Z"))
  }

  if (inherits(value, "Date")) {
    return(format(value[1], "%Y-%m-%d"))
  }

  if (is.numeric(value)) {
    numeric_value <- suppressWarnings(as.numeric(value[1]))
    if (!is.na(numeric_value)) {
      parsed <- as.POSIXct(numeric_value, origin = "1970-01-01", tz = "UTC")
      if (!is.na(parsed)) {
        return(format(parsed, "%Y-%m-%d %H:%M %Z"))
      }
    }
  }

  if (is.character(value)) {
    candidate <- first_nonempty_string(value)
    if (is.null(candidate)) {
      return(NULL)
    }

    parsed <- suppressWarnings(as.POSIXct(candidate, tz = "UTC"))
    if (!is.na(parsed)) {
      return(format(parsed, "%Y-%m-%d %H:%M %Z"))
    }

    parsed_date <- suppressWarnings(as.Date(candidate))
    if (!is.na(parsed_date)) {
      return(format(parsed_date, "%Y-%m-%d"))
    }

    return(candidate)
  }

  NULL
}

build_version_display <- function(metadata) {
  default_href <- "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/openspecy?tab=readme-ov-file#version-history"
  default_text <- paste0("Last Updated: ", format(Sys.Date()))
  default_title <- "Click here to view older versions of this app"

  if (is.null(metadata)) {
    return(list(text = default_text, href = default_href, title = default_title))
  }

  commit <- first_nonempty_string(metadata$commit)
  ref <- first_nonempty_string(metadata$ref)
  owner <- first_nonempty_string(metadata$owner)
  repo <- first_nonempty_string(metadata$repo)
  downloaded <- format_metadata_timestamp(metadata$downloaded_at)

  version_parts <- character()
  if (!is.null(ref)) {
    version_parts <- c(version_parts, ref)
  }
  if (!is.null(commit)) {
    version_parts <- c(version_parts, paste0("commit ", substr(commit, 1, 7)))
  }
  if (!length(version_parts)) {
    version_parts <- "unknown version"
  }

  text <- paste0("Version: ", paste(version_parts, collapse = " • "))
  if (!is.null(downloaded)) {
    text <- paste0(text, " — Downloaded: ", downloaded)
  }

  href <- default_href
  if (!is.null(owner) && !is.null(repo)) {
    if (!is.null(commit)) {
      href <- sprintf("https://github.com/%s/%s/commit/%s", owner, repo, commit)
    } else if (!is.null(ref)) {
      href <- sprintf("https://github.com/%s/%s/tree/%s", owner, repo, ref)
    }
  }

  title <- default_title
  if (!is.null(downloaded)) {
    title <- paste0("Downloaded on ", downloaded)
  }

  list(text = text, href = href, title = title)
}

app_metadata <- read_app_metadata()
app_version_display <- build_version_display(app_metadata)

# Define the custom theme
  theme_black_minimal <- function(base_size = 11, base_family = "") {
    theme_minimal(base_size = base_size, base_family = base_family) +
      theme(
            plot.background = element_rect(fill = "black", color = NA),
            panel.background = element_rect(fill = "black", color = NA),
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_line(color = "white"),
            axis.line = element_line(color = "white"),
            axis.ticks = element_line(color = "white"),
            axis.text = element_text(color = "white"),
            axis.title = element_text(color = "white"),
            plot.title = element_text(color = "white", hjust = 0.5),
            plot.subtitle = element_text(color = "white", hjust = 0.5),
            plot.caption = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white"),
            legend.background = element_rect(fill = "black"),
            legend.key = element_rect(fill = "black"),
            strip.background = element_rect(fill = "black", color = NA),
      strip.text = element_text(color = "white")
    )
  }

# Helper to create collapsible footnotes ----
footnote <- function(summary, ...) {
  tags$details(
    tags$summary(summary),
    tags$small(...)
  )
}

# Load all data ----
load_data <- function() {
  data("raman_hdpe")
 
  testdata <-  data.table(wavenumber = raman_hdpe$wavenumber, 
                 intensity = raman_hdpe$spectra$intensity)

  # Inject variables into the parent environment
  invisible(list2env(as.list(environment()), parent.frame()))
}

# Name keys for human readable column names ----

version <- paste0("Open Specy v", packageVersion("OpenSpecy"))
citation <- HTML(
  'Cowger, W., Karapetrova, A., Lincoln, C., Chamas, A., Sherrod, H., Leong, N., Lasdin, K. S., 
  Knauss, C., Teofilović, V., Arienzo, M. M., Steinmetz, Z., Primpke, S., 
  Darjany, L., Murphy-Hagan, C., Moore, S., Moore, C., Lattin, G., 
  Gray, A., Kozloski, R., Bryksa, J., Maurer, B. (2025). 
  "Open Specy 1.0: Automated (Hyper)spectroscopy for Microplastics." 
  <i>Analytical Chemistry.</i> doi:
  <a href="https://doi.org/10.1021/acs.analchem.5c00962">10.1021/acs.analchem.5c00962</a>.'
)


# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# Define the custom theme
theme_black_minimal <- function(base_size = 11, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      panel.grid.major = element_line(color = "white"),
      panel.grid.minor = element_line(color = "white"),
      axis.line = element_line(color = "white"),
      axis.ticks = element_line(color = "white"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      plot.title = element_text(color = "white", hjust = 0.5),
      plot.subtitle = element_text(color = "white", hjust = 0.5),
      plot.caption = element_text(color = "white"),
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.background = element_rect(fill = "black"),
      legend.key = element_rect(fill = "black"),
      strip.background = element_rect(fill = "black", color = NA),
      strip.text = element_text(color = "white")
    )
}
