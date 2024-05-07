# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
droptoken <- file.exists("s3_cred.csv") #file.exists("data/droptoken.rds") #remove for prototyping with maps
db <- file.exists("mongo.txt") #reminder, this will break if you login to a new wifi network even with the token.
translate <- file.exists("www/googletranslate.html")
config_exists <- file.exists("config.yml")

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
library(config)
library(mongolite)
library(loggit)
library(OpenSpecy)
library(bs4Dash)
#library(glmnet)
library(ggplot2)

if(droptoken) library(aws.s3)


# Global config ----
if(config_exists){
    conf <- config::get() #Add config = "shinyapps" for ec2 server
}

# Logging ----
if(isTruthy(conf$log)) {
  if(db) {
    database <- mongo(url = readLines("mongo.txt"))
  } else {
    set_logfile(file.path(tempdir(), "OpenSpecy.log"))
  }
}

if(is(tryCatch(check_lib(),error=function(e) e, warning=function(w) w), "warning") & !all(file.exists("data/mediod.rds"), file.exists("data/model.rds"), file.exists("data/nobaseline.rds"), file.exists("data/derivative.rds"))){
    get_lib()
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


