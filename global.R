library(shiny)
library(shinyWidgets)
library(bslib)
library(caTools)
library(data.table)
library(glmnet)
library(hyperSpec)
library(mmand)
library(plotly)
library(signal)
library(bs4Dash)
library(digest)
library(shinyjs)
library(dplyr)
library(shinyBS)
library(jsonlite)
library(OpenSpecy)
library(DT)


load_data <- function() {
  data("raman_hdpe")
  testdata <- data.table(wavenumber = raman_hdpe$wavenumber,
                         intensity = raman_hdpe$spectra$intensity)
  
  # Inject variables into the parent environment
  invisible(list2env(as.list(environment()), parent.frame()))
}


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
