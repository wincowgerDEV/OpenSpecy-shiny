# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
droptoken <- file.exists("s3_cred.csv") #file.exists("data/droptoken.rds") #remove for prototyping with maps
db <- F #file.exists(".db_url") #reminder, this will break if you login to a new wifi network even with the token.
translate <- file.exists("www/googletranslate.html")

#remotes::install_github("wincowgerDEV/OpenSpecy-package@v1.0-prep")

# Libraries ----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(data.table)
library(DT)
library(digest)
library(curl)
library(config)
library(mongolite)
library(loggit)
library(hyperSpec)
library(OpenSpecy)
library(bs4Dash)
library(shinyStorePlus)
library(qs)
library(glmnet)
library(tidyr)
library(ggplot2)

if(droptoken) library(aws.s3)



#OpenSpecy AI ----
model <- qread("data/all_lasso.qs")
nobaseline_library <- read_any("data/both_nobaseline.rds")
derivative_library <- read_any("data/both_derivative.rds")

#Test only

ai_classify <- function(data, wavenumbers, model){
    #preprocessing
    spectra_processed <- data %>%
                                dplyr::mutate(wavenumber = wavenumbers) %>%
                                right_join(means) %>%
                                dplyr::mutate(dplyr::across(where(is.numeric), ~fifelse(is.na(.x), mean, .x))) %>%
                                select(-mean) %>%
                                data.table::transpose(., make.names = "wavenumber") %>%
                                select(as.character(seq(400, 3995, by = 5))) %>%
                                as.matrix(.)
    #prediction
                    predict(model$model, 
                            newx = spectra_processed, 
                            min(model$model$lambda), 
                            type = "response") %>% 
                        as.data.table() %>%
                        mutate(V1 = as.integer(V1),
                               V2 = as.integer(V2)) %>%
                        right_join(data.table(V1 = 1:dim(spectra_processed)[1])) %>%
                        group_by(V1) %>%
                        filter(value == max(value, na.rm = T) | is.na(value)) %>%
                        ungroup() %>%
                        left_join(model$dimension_conversion, by = c("V2" = "factor_num")) %>%
                        arrange(V1)
}

# Global config ----
conf <- config::get() #Add config = "shinyapps" for ec2 server

# Logging ----
if(conf$log) {
  if(db) {
    database <- mongo(url = readLines(".db_url"))
  } else {
    set_logfile(file.path(tempdir(), "OpenSpecy.log"))
  }
}

# Load all data ----
load_data <- function() {
  data("raman_hdpe")
  
  testdata <-  data.table(wavenumber = raman_hdpe$wavenumber, 
                 intensity = raman_hdpe$spectra$intensity)

  if(droptoken) {
    creds <- read.csv("s3_cred.csv")

    Sys.setenv(
      "AWS_ACCESS_KEY_ID" = creds$Access.key.ID,
      "AWS_SECRET_ACCESS_KEY" = creds$Secret.access.key,
      "AWS_DEFAULT_REGION" = "us-east-2"
    )
  }
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


