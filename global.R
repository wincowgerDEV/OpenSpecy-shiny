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

# Logging ----
if(is(tryCatch(check_lib(c("derivative", 
                           "nobaseline", 
                           "medoid_derivative", 
                           "medoid_nobaseline", 
                           "model_derivative", 
                           "model_nobaseline")),error=function(e) e, warning=function(w) w), "warning") &
   !all(file.exists("data/mediod_derivative.rds"), 
        file.exists("data/model_derivative.rds"), 
        file.exists("data/mediod_nobaseline.rds"), 
        file.exists("data/model_nobaseline.rds"),
        file.exists("data/nobaseline.rds"), 
        file.exists("data/derivative.rds"))){
    get_lib(type = c("derivative", 
                     "nobaseline", 
                     "medoid_derivative", 
                     "medoid_nobaseline", 
                     "model_derivative", 
                     "model_nobaseline"))
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


