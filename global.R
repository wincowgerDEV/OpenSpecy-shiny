# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
droptoken <- file.exists("s3_cred.csv") #file.exists("data/droptoken.rds") #remove for prototyping with maps
db <- F #file.exists(".db_url") #reminder, this will break if you login to a new wifi network even with the token.
translate <- file.exists("www/googletranslate.html")

# Libraries ----
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(dplyr)
library(plotly)
library(data.table)
library(DT)
library(prompter)
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


library(TTR)
if(droptoken) library(aws.s3)

#plan(multisession) ## Run in parallel on local computer when processing full map
std_wavenumbers <- seq(100, 4000, by = 5)

#Download Data Functions ----
# Name keys for human readable column names
load("data/namekey.RData")
meta <- qread("data/joined_metadata.qs") %>%
    dplyr::filter(Organization != "Win Cowger and Sebastian Primpke") %>%
    distinct(sample_name, .keep_all = T) #Can make a few different options of these that can be loaded when needed and overwrite the existing file.

#OpenSpecy AI ----

model <- qread("data/all_lasso.qs")
means <- read.csv("data/means.csv") %>%
    select(-X) %>%
    pivot_longer(cols = everything(), names_to = "wavenumbers", values_to = "mean") %>%
    mutate(wavenumber = as.numeric(gsub("X", "", wavenumbers))) %>%
    select(-wavenumbers)

#Test only

ai_classify <- function(data, wavenumbers, model){
    #preprocessing
    spectra_processed <- data %>%
                                dplyr::mutate(wavenumber = wavenumbers) %>%
                                right_join(means) %>%
                                dplyr::mutate(dplyr::across(where(is.numeric), ~fifelse(is.na(.x), mean, .x))) %>%
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

#test <- ai_classify(data = library, wavenumbers = std_wavenumbers, model = model)

#Read spectra functions ----
read_map <- function(filename, share, id, std_wavenumbers){
  files <- unzip(zipfile = filename, list = TRUE)
  unzip(filename, exdir = tempdir())
  if(nrow(files) == 2 & any(grepl("\\.dat$", ignore.case = T, files$Name)) & any(grepl("\\.hdr$", ignore.case = T, files$Name))){
    hs_envi <- hyperSpec::read.ENVI.Nicolet(file = paste0(tempdir(), "/", files$Name[grepl("\\.dat$", ignore.case = T, files$Name)]),
                                            headerfile = paste0(tempdir(), "/", files$Name[grepl("\\.hdr$", ignore.case = T, files$Name)]))

    list(
      "wavenumber" = hs_envi@wavelength,
      "spectra" = transpose(as.data.table(hs_envi@data$spc)),
      "coords" = data.table(x = hs_envi@data$x, y = hs_envi@data$y, filename = gsub(".*/", "", hs_envi@data$filename))
    )
  }
  else if(nrow(files) == 1 & any(grepl("\\.RData$", ignore.case = T, files$Name))){
    assign("file", base::get(load(paste0(tempdir(), "/", files$Name))))
    dt <- gen_grid(x = ncol(file))
    list(
      "wavenumber" =  std_wavenumbers,
      "spectra" = file,
      "coords" = gen_grid(x = ncol(file))[,filename := files$Name])
  }

  else{

    file <- bind_cols(lapply(paste0(tempdir(), "/", files$Name), read_spectrum, share = F, id = "sdfad"))

    list(
      "wavenumber" = file$wavenumber...1,
      "spectra" = file %>%
        select(-starts_with("wave")),
      "coords" = gen_grid(nrow(files))[,filename := files$Name])
  }
}

gen_grid <- function(x) {
    base <- sqrt(x)
    expand.grid(x = 1:ceiling(base), y = 1:ceiling(base))[1:x,] %>%
        as.data.table
}

conform_res <- function(x, res = 5) {
    seq(adj_res(min(x), res, ceiling), adj_res(max(x), res, floor), by = res)
}

adj_res <- function(x, res = 1, fun = round) {
    fun(x / res) * res
}

read_any <- function(filename, share, id, std_wavenumbers){
  if(grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)", ignore.case = T, filename)){
    read_formatted_spectrum(filename = filename, share = share, id = id)
    #single_data$data <- TRUE
  }

  else if(grepl("\\.zip$", ignore.case = T, filename)) {
    read_map(filename = filename, share = NULL, id = id, std_wavenumbers = std_wavenumbers)

  }
}

read_spectrum <- function(filename, share = NULL, id = "test") {

  as.data.table(
    if(grepl("\\.csv$", ignore.case = T, filename)) {
      tryCatch(read_text_2(file = filename,
                           method = "fread",
                           share = share,
                           id = id),
               error = function(e) {e})
    }
    else if(grepl("\\.[0-9]$", ignore.case = T, filename)) {
      tryCatch(read_0(filename, share = share, id = id),
               error = function(e) {e})
    }

    else {
      ex <- strsplit(basename(filename), split="\\.")[[1]]

      tryCatch(do.call(paste0("read_", tolower(ex[-1])),
                       list(filename, share = share, id = id)),
               error = function(e) {e})
    }
  )
}

read_text_2 <- function(file = ".", cols = NULL, method = "read.csv",
                        share = NULL, id = paste(digest(Sys.info()),
                                                 digest(sessionInfo()),
                                                 sep = "/"), ...) {
  df <- do.call(method, list(file, ...))

  if (all(grepl("^X[0-9]*", names(df)))) stop("missing header: ",
                                              "use 'header = FALSE' or an ",
                                              "alternative read method")

  # Try to guess column names
  if (is.null(cols)) {
    #if (all(grepl("^V[0-9]*", names(df)))) {
    #    cols <- 1:2
    #    warning("missing header: guessing 'wavenumber' and 'intensity' data ",
    #            "from the first two columns; use 'cols' to supply user-defined ",
    #            "columns")
    #} else {
    cols <- c(names(df)[grep("(wav*)", ignore.case = T, names(df))][1L])#,
    #names(df)[grep("(transmit*)|(reflect*)|(abs*)|(intens*)",
    #              ignore.case = T, names(df))][1L])
    #}
  }
  if (any(is.na(cols))){
    cols <- c(names(df)[1L])#,
    warning("undefined columns selected; one column should be named wavenumber; guessing first.")
  }
  #if (cols[1] == cols[2]) stop("inconsistent input format")

  df <- df %>%
    rename("wavenumber" = cols)

  # Check if columns are numeric
  if (!all(sapply(df, is.numeric))){
    df <- df[,c(sapply(df, is.numeric)), with = F]
    warning("Dropping non-numeric columns")
  }

  #names(df) <- c("wavenumber", "intensity")

  #if (!is.null(share)) share_spec_2(df, file = file, share = share, id = id)

  return(df)
}

read_formatted_spectrum <- function(filename, share, id){
  spectra <- read_spectrum(filename = filename, share = share, id = id)
  list("wavenumber" =
         spectra$wavenumber,
       "spectra" =
         spectra %>% select(-wavenumber),
       "coords" = gen_grid(x = ncol(spectra) - 1)[,filename := gsub(".*/", "", filename)]
  )
}



#Conform spectra functions ----
# TODO: to be removed in favor of conform_intens()
conform_spectra <- function(df, wavenumber, correction){
  df[,lapply(.SD, conform_intensity, wavenumber = wavenumber, correction = correction)]
}

conform_intensity <- function(intensity, wavenumber, correction){
  adj_intens(x = conform_res(wavenumber),
             y = clean_spec(x = wavenumber, y = intensity, out = conform_res(wavenumber)),
             type = correction,
             na.rm = T)[,"intensity"]
}

clean_spec <- function(x, y, out){
  c(
    approx(x = x, y = y, xout = out)$y
  )
}



#Process spectra functions ----
process_intensity <- function(intensity, wavenumber, active_preprocessing = T,
                              range_decision = F, min_range = 0,
                              max_range = 6000,
                              carbon_dioxide_decision = F,
                              smooth_decision = F, smoother = 3,
                              baseline_decision = F,
                              baseline_selection = "Polynomial", baseline = 8,
                              derivative_decision = T, trace = NULL) {

  test <- wavenumber %in% wavenumber[!is.na(intensity)]
  place <- rep(NA, length.out= length(wavenumber))

  #set initial conditions
  intensity_cor <- intensity[!is.na(intensity)]
  wavenumber_cor <- wavenumber[!is.na(intensity)]
  test2 <-  length(wavenumber_cor[wavenumber_cor > min_range & wavenumber_cor < max_range]) > 11

  #Range criteria
  if(range_decision & test2) {
    #assumes that all the wavenumbers exist, but they don't . Might be problematic when we try to use the wavenumber range for correlation afterward or
    test <- test &  wavenumber >= min_range & wavenumber <= max_range
    intensity_cor <- intensity_cor[wavenumber_cor >= min_range & wavenumber_cor <= max_range]
    wavenumber_cor <- wavenumber_cor[wavenumber_cor >= min_range & wavenumber_cor <= max_range]
    #test <- std_wavenumbers %in% std_wavenumbers[std_wavenumbers >= min(wavenumber_cor) & std_wavenumbers <= max(wavenumber_cor)]

  }

  #CO2 criteria
  if(carbon_dioxide_decision) {
    #assumes that all the wavenumbers exist, but they don't
    intensity_cor[wavenumber_cor >= 2200 & wavenumber_cor <= 2420] <- mean(intensity_cor[wavenumber_cor %in% c(2200, 2420)])
    #test <- std_wavenumbers %in% std_wavenumbers[std_wavenumbers >= min(wavenumber_cor) & std_wavenumbers <= max(wavenumber_cor)]

  }

  #Smooth criteria
  if(smooth_decision) {
    intensity_cor <- smooth_intens(wavenumber_cor, intensity_cor, p = smoother)$intensity
  }
  #Baseline criteria
  if(baseline_decision & baseline_selection == "Polynomial") {
    intensity_cor <- subtr_bg(wavenumber_cor, intensity_cor, degree = baseline)$intensity
  }
  else if(baseline_decision & baseline_selection == "Manual" & !is.null(trace$data)){
    intensity_cor <-  intensity_cor - approx(trace$data$wavenumber, trace$data$intensity, xout = wavenumber_cor, rule = 2, method = "linear", ties = mean)$y
  }

  #Derivative
  if(derivative_decision) {
    intensity_cor <-  process_cor_os(intensity_cor)
  }

  place[test] <- intensity_cor#try using this for other function

  place

}


process_spectra <- function(df, wavenumber, active_preprocessing = T, range_decision = F, min_range = 0, max_range = 6000, carbon_dioxide_decision = F, smooth_decision = F, smoother = 3, baseline_decision = F, baseline_selection = "Polynomial", baseline = 8, derivative_decision = T, trace = NULL){
  df[,lapply(.SD, process_intensity, wavenumber = wavenumber, active_preprocessing = active_preprocessing, range_decision = range_decision, min_range = min_range, max_range = max_range, carbon_dioxide_decision = carbon_dioxide_decision, smooth_decision = smooth_decision, smoother = smoother, baseline_decision = baseline_decision, baseline_selection = baseline_selection, baseline = baseline, derivative_decision = derivative_decision, trace = trace)]
}

#signal to noise ratio

signal_to_noise <- function(wavenumber, intensity, noise_min = 2200, noise_max = 2420, signal_min = 1900, signal_max = 2700, method = "Auto", return = "signal_times_noise") {
    if(method == "Manual"){
        signal_values <- wavenumber >= signal_min & wavenumber <= signal_max
        noise_values <- wavenumber >= noise_min & wavenumber <= noise_max 
        signal = mean(intensity[signal_values], na.rm = T)
        noise = sd(intensity[noise_values], na.rm = T)    
    }
    if(method == "Auto"){
        if(length(intensity[!is.na(intensity)]) < 20){
        return(NA)
    }
    else{
        max = runMax(intensity[!is.na(intensity)], n = 20)
        signal = max(max, na.rm = T)
        noise = median(max[max != 0], na.rm = T)
        }
    }
    if(return == "signal"){
        return(signal)
    }
    if(return == "noise"){
        return(noise)
    }
    if(return == "signal_times_noise"){
        return(abs(signal*noise))
    }
    if(return == "signal_to_noise"){
        return(abs(signal/noise))
    }
}


#Correlate functions ----
correlate_intensity <- function(intensity, search_wavenumbers, lib){
  c(cor(intensity, lib, use = "everything"))
}

correlate_spectra <- function(data, search_wavenumbers, std_wavenumbers, library){
    cor(data[search_wavenumbers %in% std_wavenumbers,][,lapply(.SD, mean_replace)], 
        library[std_wavenumbers %in% search_wavenumbers,][,lapply(.SD, mean_replace)])
}

mean_replace <- function(intensity){
  fifelse(is.na(intensity), mean(intensity, na.rm = T), intensity)
}

get_all_metadata <- function(sample_name, rsq, metadata) {
  left_join(data.table(sample_name = sample_name, rsq = rsq), metadata) %>%
    filter(!is.na(rsq)) %>%
    arrange(desc(rsq)) %>%
    mutate(rsq = round(rsq, 2))
}

#library(future)
#library(bslib)

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


render_tweet <- function(x){renderUI({
  div(class = "inline-block",
      style = "display:inline-block; margin-left:4px;",
      tags$blockquote(class = "twitter-tweet", `data-theme` = "dark",
                      style = "width: 600px; display:inline-block;" ,
                      tags$a(href = x)),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
  )
})
}

# Load all data ----
load_data <- function() {
  data("raman_hdpe")
  
  testdata <-  data.table(wavenumber = raman_hdpe$wavenumber, 
                 intensity = raman_hdpe$spectra$intensity)

  tweets <- c("https://twitter.com/EnviroMichaela/status/1471622640183959555",
              #"https://twitter.com/OpenSpecy/status/1472361269093023744",
              "https://twitter.com/DSemensatto/status/1461038613903380484",
              "https://twitter.com/SETAC_plastics/status/1460738878101356544",
              "https://twitter.com/AliciaMateos_/status/1460197329760313344",
              "https://twitter.com/Irreverent_KUP/status/1454418069036568578",
              "https://twitter.com/PeterPuskic/status/1454267818166210561",
              "https://twitter.com/JannesJegminat/status/1427257468384681985",
              "https://twitter.com/pnwmicroplastic/status/1415730821730734080",
              #"https://twitter.com/OpenSpecy/status/1408391168745000961",
              "https://twitter.com/ToMExApp/status/1399859256615079936",
              "https://twitter.com/kat_lasdin/status/1399576094622175241",
              "https://twitter.com/an_chem/status/1397621113421803521",
              "https://twitter.com/WarrierAnish/status/1395245636967014401",
              "https://twitter.com/EnviroMichaela/status/1395199312645300233",
              "https://twitter.com/SocAppSpec/status/1392883693027430400",
              #"https://twitter.com/zsteinmetz_/status/1387677422028480512",
              #"https://twitter.com/OpenSpecy/status/1382820319635775488",
              #"https://twitter.com/zsteinmetz_/status/1377222029250822146",
              #"https://twitter.com/OpenSpecy/status/1318214558549372928",
              "https://twitter.com/YokotaLimnoLab/status/1311069417892184065") %>%
    sample(2)

  goals <- tibble(
    Status =      c("Revolutionizing",
                    "Thriving",
                    "Maintaining",
                    "Supporting",
                    "Saving"),
    Description = c("A paid team that is pushing Open Specy closer to the ultimate goal of 100% accurate spectral identification and deep spectral diagnostics with a single click",
                    "A single paid staff person working to update and build the community and the tool",
                    "Maintenance costs and minor ad-hoc updates and bug fixes",
                    "Keeping the app online and essential maintenance",
                    "Long term storage only"),
    'Annual Need'  = c(">100,000$",
                       "10,000–100,000$",
                       "1,000–10,000$",
                       "100–1,000$",
                       "<100$")
  )
  # Check if spectral library is present and load
  #test_lib <- class(tryCatch(check_lib(path = conf$library_path),
  #                           warning = function(w) {w}))

  #if(any(test_lib == "warning")) get_lib(path = conf$library_path)


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

process_cor_os <- function(x){
  abs(
    c(
      scale(
        signal::sgolayfilt(x,
                           p = 3, n = 11, m = 1
        )
      )
    )
  )
}



is_empty <- function(x, first.only = TRUE, all.na.empty = TRUE) {
  # do we have a valid vector?
  if (!is.null(x)) {
    # if it's a character, check if we have only one element in that vector
    if (is.character(x)) {
      # characters may also be of length 0
      if (length(x) == 0) return(TRUE)
      # else, check all elements of x
      zero_len <- nchar(x) == 0
      # return result for multiple elements of character vector
      if (first.only) {
        zero_len <- .is_true(zero_len[1])
        if (length(x) > 0) x <- x[1]
      } else {
        return(unname(zero_len))
      }
      # we have a non-character vector here. check for length
    } else if (is.list(x)) {
      x <- purrr::compact(x)
      zero_len <- length(x) == 0
    } else {
      zero_len <- length(x) == 0
    }
  }

  any(is.null(x) || zero_len || (all.na.empty && all(is.na(x))))
}

.is_true <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && x
}


map_type <- function(filename){
  files <- unzip(zipfile = filename, list = TRUE)
  if(nrow(files) == 2 & any(grepl("\\.dat$", ignore.case = T, files$Name)) & any(grepl("\\.hdr$", ignore.case = T, files$Name))){
    "envi"
  }
  else if(nrow(files) == 1 & any(grepl("\\.RData$", ignore.case = T, files$Name))){
    "rdata"
  }
  else{
    "multiple"
  }
}



#' Shiny app server




# Name keys for human readable column names ----

version <- paste0("Open Specy v", packageVersion("OpenSpecy"))
citation <- HTML(
  "Cowger W, Steinmetz Z, Gray A, Munno K, Lynch J, Hapich H, Primpke S, De
  Frond H, Rochman C, Herodotou O (2021). “Microplastic Spectral
  Classification Needs an Open Source Community: Open Specy to the Rescue!”
  <i>Analytical Chemistry</i>, <b>93</b>(21), 7543–7548. doi:
  <a href='https://doi.org/10.1021/acs.analchem.1c00123'>10.1021/acs.analchem.1c00123</a>."
)

# Functions ----
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

inputUserid <- function(inputId, value="") {
  #   print(paste(inputId, "=", value))
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js",
                                    type="text/javascript"))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js",
                                    type="text/javascript"))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "userid", value=as.character(value),
               type = "text", style = "display:none;")
  )
}

inputIp <- function(inputId, value=""){
  tagList(
    singleton(tags$head(tags$script(src = "js/md5.js",
                                    type="text/javascript"))),
    singleton(tags$head(tags$script(src = "js/shinyBindings.js",
                                    type="text/javascript"))),
    tags$body(onload="setvalues()"),
    tags$input(id = inputId, class = "ipaddr", value=as.character(value),
               type = "text", style = "display:none;")
  )
}

