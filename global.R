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
#webr::install("OpenSpecyWebr", repos = "https://moore-institute-4-plastic-pollution-res.github.io/OpenSpecyWebr/")
#library(OpenSpecyWebr)

# Load in all functions in R folder
lapply(list.files("R", full.names = TRUE), source)


# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}


# Load all data ----

load_data <- function() {
    data("raman_hdpe")
    testdata <- data.table(wavenumber = raman_hdpe$wavenumber,
                           intensity = raman_hdpe$spectra$intensity)
    # Inject variables into the parent environment
    invisible(list2env(as.list(environment()), parent.frame()))

}

# # Name keys for human readable column names ----
# #version <- paste0("Open Specy v", packageVersion("OpenSpecyWebr"))
# citation <- HTML(
#   "Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S, De
#   Frond H, Rochman C, Herodotou O (2021). “Microplastic Spectral
#   Classification Needs an Open Source Community: Open Specy to the Rescue!”
#   <i>Analytical Chemistry</i>, <b>93</b>(21), 7543–7548. doi:
#   <a href='https://doi.org/10.1021/acs.analchem.1c00123'>10.1021/acs.analchem.1c00123</a>."
# )
