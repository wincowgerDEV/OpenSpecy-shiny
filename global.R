library(shiny)
library(bslib)
library(caTools)
library(data.table)
library(glmnet)
library(hyperSpec)
library(mmand)
library(plotly)
library(signal)
webr::install("OpenSpecyWebr", repos = "https://moore-institute-4-plastic-pollution-res.github.io/OpenSpecyWebr/")
library(OpenSpecyWebr)

# Load all data ----
load_data <- function() {
  data("raman_hdpe")
  
  load_data <- function() {
    data("raman_hdpe")
    testdata <- data.table(wavenumber = raman_hdpe$wavenumber, 
                           intensity = raman_hdpe$spectra$intensity)
    return(testdata)
  }
  
}