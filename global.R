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

build_version_display <- function(metadata) {
  default_href <- "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/openspecy?tab=readme-ov-file#version-history"
  default_text <- paste0("Last Updated: ", format(Sys.Date()))
  default_title <- "Click here to view older versions of this app"

  if (is.null(metadata)) {
    return(list(text = default_text, href = default_href, title = default_title))
  }

  commit <- metadata$commit
  ref <- metadata$ref
  owner <- metadata$owner
  repo <- metadata$repo

  downloaded_time <- metadata$downloaded_at

  text <- paste0("Last Pulled: ", downloaded_time)
  commit_display <- NULL
  if (!is.null(commit)) {
    commit_display <- substr(commit, 1, min(nchar(commit), 7))
    text <- paste0(text, " • Commit ", commit_display)
  }

  href <- default_href
  if (!is.null(owner) && !is.null(repo)) {
    href <- sprintf("https://github.com/%s/%s/commits", owner, repo)
    if (!is.null(ref)) {
      href <- sprintf("%s/%s", href, utils::URLencode(ref, reserved = TRUE))
    }
  }

  title <- default_title
  if (!is.null(downloaded_time) || !is.null(commit)) {
    parts <- c()
    if (!is.null(downloaded_time)) {
      parts <- c(parts, paste0("Last pulled ", downloaded_time))
    }
    if (!is.null(commit)) {
      parts <- c(parts, paste0("Commit ", commit))
    }
    if (length(parts)) {
      title <- paste(parts, collapse = " — ")
    }
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
