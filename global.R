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
library(OpenSpecy)
library(bs4Dash)
library(ggplot2)


if(is(tryCatch(check_lib(),error=function(e) e, warning=function(w) w), "warning") & !all(file.exists("data/mediod.rds"), file.exists("data/model.rds"), file.exists("data/nobaseline.rds"), file.exists("data/derivative.rds"))){
    download.file(url = "https://osf.io/download/2qbkt/", destfile = "derivative.rds")
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
  "Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S, De
  Frond H, Rochman C, Herodotou O (2021). “Microplastic Spectral
  Classification Needs an Open Source Community: Open Specy to the Rescue!”
  <i>Analytical Chemistry</i>, <b>93</b>(21), 7543–7548. doi:
  <a href='https://doi.org/10.1021/acs.analchem.1c00123'>10.1021/acs.analchem.1c00123</a>."
)


