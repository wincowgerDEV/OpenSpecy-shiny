#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'
#'
# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
droptoken <- F # file.exists("data/droptoken.rds") remove for prototyping with maps 
db <- F#file.exists(".db_url") #reminder, this will break if you login to a new wifi network even with the token.
translate <- file.exists("www/googletranslate.html")

# Libraries ----
library(shiny)
library(shinyjs)
library(dplyr)
library(plotly)
# library(viridis)
library(data.table)
library(DT)
library(digest)
library(curl)
library(config)
library(mongolite)
library(loggit)
library(hyperSpec)
library(TTR)
if(droptoken) library(rdrop2)

#devtools::install_github("wincowgerDEV/OpenSpecy")
library(OpenSpecy)

round_any <- function(x, accuracy, f = round){
  f(x / accuracy) * accuracy
}


conform_spectra <- function(df, wavenumber, std_wavenumbers, correction){
    setcolorder(df[,2:ncol(df)][,lapply(.SD, conform_intensity, wavenumber = wavenumber, correction = correction, std_wavenumbers = std_wavenumbers)][,wavenumber := std_wavenumbers], "wavenumber")
}

conform_intensity <- function(intensity, wavenumber, correction, std_wavenumbers){
    test <- std_wavenumbers %in% conform_wavenumber(wavenumber)
    new_wavenumbers <- std_wavenumbers[test]
    place <- rep(NA, length.out= length(std_wavenumbers))
    vec <- adjust_intensity(x = new_wavenumbers,
                            y = clean_spec(x = wavenumber, y = intensity, out = new_wavenumbers),
                            type = correction,
                            na.rm = T)[,"intensity"]
    place[test] <- vec
    place
}

adjust_intensity <- function(x, y, type = "none", make_rel = F, ...) {
    yadj <- switch(type,
                   "reflectance" = (1 - y/100)^2 / (2 * y/100),
                   "transmittance" = log10(1/adj_neg(y, ...)),
                   "none" = adj_neg(y, ...)
    )
    if (make_rel) yout <- make_rel(yadj) else yout <- yadj
    
    data.frame(wavenumber = x, intensity = yout)
}


conform_wavenumber <- function(wavenumber){
    seq(round_any(min(wavenumber), 5, ceiling), round_any(max(wavenumber), 5, floor), by = 5)
}

clean_spec <- function(x, y, out){
    c(
        approx(x = x, y = y, xout = out)$y
    )
}


#Process spectra functions 

process_intensity <- function(intensity, wavenumber, active_preprocessing, range_decision, min_range, max_range, smooth_decision, smoother, baseline_decision, baseline_selection, baseline, derivative_decision, trace, std_wavenumbers) {
    
    test <- std_wavenumbers %in% std_wavenumbers[!is.na(intensity)]
    place <- rep(NA, length.out= length(std_wavenumbers))
    
    #set innitial conditions
    intensity_cor <- intensity[!is.na(intensity)]
    wavenumber_cor <- wavenumber[!is.na(intensity)]
    test2 <-  length(wavenumber_cor[wavenumber_cor > min_range & wavenumber_cor < max_range]) > 11
    
    #Range criteria   
    if(range_decision & test2) {
        #assumes that all the wavenumbers exist, but they don't 
        intensity_cor <- intensity_cor[wavenumber_cor >= min_range & wavenumber_cor <= max_range]
        wavenumber_cor <- wavenumber_cor[wavenumber_cor >= min_range & wavenumber_cor <= max_range]
        test <- std_wavenumbers %in% std_wavenumbers[std_wavenumbers >= min(wavenumber_cor) & std_wavenumbers <= max(wavenumber_cor)]
        
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

process_spectra <- function(df, wavenumber, active_preprocessing, range_decision, min_range, max_range, smooth_decision, smoother, baseline_decision, baseline_selection, baseline, derivative_decision, trace, std_wavenumbers){
    setcolorder(df[,2:ncol(df)][,lapply(.SD, process_intensity, wavenumber = wavenumber, active_preprocessing = active_preprocessing, range_decision = range_decision, min_range = min_range, max_range = max_range, smooth_decision = smooth_decision, smoother = smoother, baseline_decision = baseline_decision, baseline_selection = baseline_selection, baseline = baseline, derivative_decision = derivative_decision, trace = trace, std_wavenumbers = std_wavenumbers)][,wavenumber := std_wavenumbers], "wavenumber")
}

#signal to noise ratio
snr <- function(x) {
    max  = runMax(x[!is.na(x)], n = 20) 
    max[(length(max) - 19):length(max)] <- NA
    #mean = runMean(x[!is.na(x)], n = 10)
    #mean[(length(mean) - 9):length(mean)] <- NA
    signal = max[which.max(max)]#/mean(x, na.rm = T)
    noise = max[which.min(max[max != 0])]
    signal/noise
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

  testdata <- raman_hdpe

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
  std_wavenumbers <- seq(405, 3995, by = 5)
  
  if(droptoken) {
    drop_auth(rdstoken = "data/droptoken.rds")
  }

  # Name keys for human readable column names
  load("data/namekey.RData")
  load("data/metadata.RData") #Can make a few different options of these that can be loaded when needed and overwrite the existing file. 
  

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

read_map <- function(filename, share, id, std_wavenumbers){
    files <- unzip(zipfile = filename, list = TRUE)
    unzip(filename, exdir = tempdir())
    if(nrow(files) == 2 & any(grepl("\\.dat$", ignore.case = T, files$Name)) & any(grepl("\\.hdr$", ignore.case = T, files$Name))){
        hs_envi <- hyperSpec::read.ENVI.Nicolet(file = paste0(tempdir(), "/", files$Name[grepl("\\.dat$", ignore.case = T, files$Name)]),
                                                headerfile = paste0(tempdir(), "/", files$Name[grepl("\\.hdr$", ignore.case = T, files$Name)]))@data
        
        list("spectra" = transpose(as.data.table(hs_envi$spc), keep.names = "wavenumber") %>%
                 mutate(wavenumber = as.numeric(wavenumber)), 
             "coords" = data.table(x = hs_envi$x, y = hs_envi$y))
    }
    else if(nrow(files) == 1 & any(grepl("\\.RData$", ignore.case = T, files$Name))){
        assign("file", base::get(load(paste0(tempdir(), "/", files$Name))))
        base <- sqrt(ncol(file)-1)
        file$wavenumber <- std_wavenumbers
        list("spectra" = file %>%
                 select(wavenumber, everything()), 
             "coords" = expand.grid(x = 1:round_any(base, 1, ceiling), y = 1:round_any(base, 1, ceiling))[1:(ncol(file)-1),])
    }
    
    else{
        base <- sqrt(nrow(files))
        
        list("spectra" = bind_cols(lapply(paste0(tempdir(), "/", files$Name), read_spectrum, share = F, id = "sdfad")) %>%
                 select(wavenumber...1, starts_with("intens")) %>%
                 rename(wavenumber = wavenumber...1), 
             "coords" = expand.grid(x = 1:round_any(base, 1, ceiling), y = 1:round_any(base, 1, ceiling))[1:nrow(files),])
    }
}

read_any <- function(filename, share, id, std_wavenumbers){
    if(grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)", ignore.case = T, filename)){
        read_spectrum(filename = filename, share = share, id = id)
        #single_data$data <- TRUE
    }
    
    else if(grepl("\\.zip$", ignore.case = T, filename)) {
        read_map(filename = filename, share = share, id = id, std_wavenumbers = std_wavenumbers)

    }
}

read_spectrum <- function(filename, share, id) {
    
    list("spectra" =     
             as.data.table(
                 if(grepl("\\.csv$", ignore.case = T, filename)) {
                     tryCatch(read_text(filename, method = "fread",
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
             ),
         "coords" = data.table(x = 1, y = 1)
    )
}

                         
# This is the actual server functions, all functions before this point are not
# reactive
server <- shinyServer(function(input, output, session) {
    
  session_id <- digest(runif(10))

  # Loading overlay
  load_data()
  hide(id = "loading_overlay", anim = TRUE, animType = "fade")
  show("app_content")

# For desktop version of the app.
#  if (!interactive()) {
#    session$onSessionEnded(function() {
#      stopApp()
#      q("no")
#    })
#  }

  output$event_goals <- DT::renderDataTable({
    datatable(goals,
              options = list(
                             dom = "t",
                             ordering = FALSE,
                             paging = FALSE,
                             searching = FALSE
                             #sDom  = '<"top">lrt<"bottom">ip',

                             ),
              caption = "Progress (current status selected)",
              style = "bootstrap",
              class = 'row-border',
              escape = FALSE,
              rownames = FALSE,
              #formatStyle(c("Annual Need"), backgroundColor = styleColorBar(color = clrs)),
              selection = list(mode = "single", selected = c(2)))
  })

  #Reading Data and Startup ----
  # Sharing ID
  id <- reactive({
    if (!is.null(input$fingerprint)) {
      paste(input$fingerprint, session_id, sep = "/")
    } else {
      paste(digest(Sys.info()), digest(sessionInfo()), sep = "/")
    }
  })
  
  #Reactive Values ----
  preprocessed <- reactiveValues(data = NULL)
  trace <- reactiveValues(data = NULL)

  
observeEvent(input$file1, {
  # Read in data when uploaded based on the file type
  req(input$file1)
  file <- input$file1
  filename$data <- as.character(file$datapath)
  
  if (!grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.RData$)|(\\.zip$)|(\\.[0-9]$)",
             ignore.case = T, filename$data)) {
    show_alert(
      title = "Data type not supported!",
      text = paste0("Uploaded data type is not currently supported; please
                      check tooltips and 'About' tab for details."),
      type = "warning")
    return(NULL)
  }
 
  if (input$share_decision & curl::has_internet()) {
    share <- conf$share
    progm <- "Sharing Spectrum to Community Library"
  } else {
    share <- NULL
    progm <- "Reading Spectrum"
  }
  
  withProgress(message = progm, value = 3/3, {
      rout <- read_any(
          filename = filename$data, share = share, id = id(), std_wavenumbers = std_wavenumbers
      )
    
    if (inherits(rout, "simpleError")) {
      reset("file1")
      show_alert(
        title = "Something went wrong :-(",
        text = paste0("R says: '", rout$message, "'. ",
                      "If you uploaded a text/csv file, make sure that the ",
                      "columns are numeric and named 'wavenumber' and ",
                      "'intensity'."),
        type = "error"
      )
      return(NULL)
    } 
    else {
      preprocessed$data <- rout
    }
})
})

  # Corrects spectral intensity units using the user specified correction
  data <- reactive({
    req(input$file1)
      conform_spectra(df = preprocessed$data$spectra, 
                      wavenumber = preprocessed$data$spectra$wavenumber, 
                      correction = input$intensity_corr, 
                      std_wavenumbers = std_wavenumbers)
    })

  #Preprocess Spectra ----
  # All cleaning of the data happens here. Range selection, Smoothing, and Baseline removing
  baseline_data <- reactive({
     req(input$file1)
    if(!length(data()) | !input$active_preprocessing) {
      data.table(wavenumber = numeric(), intensity = numeric(), SpectrumIdentity = factor())
    }
    else{
    processed <- process_spectra(df = data(), 
                    wavenumber = data()$wavenumber,
                    active_preprocessing = input$active_preprocessing, 
                    range_decision = input$range_decision, 
                    min_range = input$MinRange, 
                    max_range = input$MaxRange, 
                    smooth_decision = input$smooth_decision, 
                    smoother = input$smoother, 
                    baseline_decision = input$baseline_decision, 
                    baseline_selection = input$baseline_selection, 
                    baseline = input$baseline, 
                    derivative_decision = input$derivative_decision,
                    trace = trace,
                    std_wavenumbers = std_wavenumbers)
    
    preprocessed$data$coords$snr <-  log10(unlist(lapply(processed[,2:ncol(processed)], snr)))
    
    processed
    
    }
  })


observeEvent(input$go, {
  pathinfo <- event_data(event = "plotly_relayout", source = "B")$shapes$path
  if (is.null(pathinfo)) trace$data <- NULL
  else {
   nodes <- unlist(strsplit(
             gsub("(L)|(M)", "_",
                  paste(unlist(pathinfo), collapse = "")),
             "(,)|(_)"))
   nodes = nodes[-1]
   df <- as.data.frame(matrix(nodes, ncol = 2, byrow = T))
   names(df) <- c("wavenumber", "intensity")
   trace$data <- df
  }
})

observeEvent(input$reset, {
  trace$data <- NULL
})

  # Choose which spectrum to use
  DataR <- reactive({
      if(input$active_preprocessing) {
        baseline_data()
    }
    else {
        data()
    }
  })
  
  DataR_plot <- reactive({
    if(!input$active_identification) {
      data.table(wavenumber = numeric(), 
                 intensity = numeric(), 
                 SpectrumIdentity = factor())    }
    else{
      data.table(wavenumber = std_wavenumbers,
                 intensity = make_rel(DataR()[[data_click()]], na.rm = T),
                 SpectrumIdentity = factor()) %>%
        dplyr::filter(!is.na(intensity))
    }
    
  })
  
  libraryR <- reactive({
    req(input$file1)
    req(input$active_identification)
    if(input$derivative_decision & input$active_preprocessing) {
      load("data/library_deriv.RData") #Nest these in here so that they don't load automatically unless needed. 
        
    }
    else{
        load("data/library.RData") #Nest these in here so that they don't load automatically unless needed. 
    }
    
    if(input$Spectra == "both") {
      library
    }
    else if (input$Spectra == "ftir"){
      cols <- meta %>% dplyr::filter(SpectrumType == "FTIR") %>% pull(sample_name)
      library[, ..cols] 
    }
    else if (input$Spectra == "raman"){
      cols <- meta %>% dplyr::filter(SpectrumType == "Raman") %>% pull(sample_name)
      library[, ..cols] 
    }
  })
  
  match_selected <- reactive({# Default to first row if not yet clicked
    req(input$file1)
    if(!length(data()) | !input$active_identification) {
        data.table(intensity = numeric(), wavenumber = numeric())
    }
    else{
        id_select <- ifelse(is.null(input$event_rows_selected),
                            MatchSpectra()[[1,
                                            "sample_name"]],
                            MatchSpectra()[[input$event_rows_selected,
                                            "sample_name"]])
        # Get data from find_spec
        current_spectrum <- data.table(wavenumber = std_wavenumbers, 
                                       intensity = libraryR()[[id_select]], 
                                       sample_name = id_select)
        
        current_spectrum %>%
            inner_join(meta, by = "sample_name") %>%
            select(wavenumber, intensity, SpectrumIdentity) %>%
            mutate(intensity = make_rel(intensity, na.rm = T)) #%>%
    }
        
      })
  
  correlation <- reactive({
      cor <- cor(DataR()[,2:ncol(DataR())][!is.na(DataR()[[2]]),], libraryR()[!is.na(DataR()[[2]]),], use = "pairwise.complete.obs")
      preprocessed$data$coords$max_cor <- round(apply(cor, 1, function(x) max(x, na.rm = T)), 2)
      cor
  })
  
  
  
  # Identify Spectra function ----
  # Joins their spectrum to the internal database and computes correlation.
  MatchSpectra <- reactive({
    req(input$file1)
    req(input$active_identification)
    input
    withProgress(message = 'Analyzing Spectrum', value = 1/3, {

      incProgress(1/3, detail = "Finding Match")
      
        
      Lib <- left_join(data.table(sample_name = names(correlation()[(data_click() - 1),]), rsq = correlation()[(data_click() - 1),]), meta) %>%
          mutate(rsq = round(rsq, 2)) %>%
          filter(!is.na(rsq)) %>%
          arrange(desc(rsq))
      
      incProgress(1/3, detail = "Making Plot")

    })
    return(Lib)
  })

  top_matches <- reactive({
      MatchSpectra() %>%
          dplyr::rename("Material" = SpectrumIdentity) %>%
          dplyr::rename("Pearson's r" = rsq) %>%
          dplyr::select(if(input$id_level == "deep"){"Material"} 
                        else if(input$id_level == "pp_optimal"){"polymer"}
                        else if(input$id_level == "pp_groups"){"polymer_class"}
                        else{"plastic_or_not"}, `Pearson's r`, sample_name)
  })
  
  # Create the data tables for all matches
  output$event <- DT::renderDataTable({
    req(input$active_identification)
    datatable(top_matches(),
              options = list(searchHighlight = TRUE,
                             scrollX = TRUE,
                             sDom  = '<"top">lrt<"bottom">ip',
                             lengthChange = FALSE, pageLength = 5),
              rownames = FALSE,
              filter = "top", caption = "Selectable Matches",
              style = "bootstrap",
              
              selection = list(mode = "single", selected = c(1)))
  })
  
match_metadata <- reactive({
    MatchSpectra()[input$event_rows_selected,] %>%
        select(where(~!any(is_empty(.))))
})
    #Metadata for the selected value
 output$eventmetadata <- DT::renderDataTable({
     req(input$active_identification)
    datatable(match_metadata(),
              escape = FALSE,
              options = list(dom = 't', bSort = F, 
                             scrollX = TRUE,
                             lengthChange = FALSE,
                             info = FALSE),
              rownames = FALSE,
              style = 'bootstrap', caption = "Selection Metadata",
              selection = list(mode = 'none'))
  })
  
  data_click <- reactive({
      if(is.null(event_data("plotly_click", source = "heat_plot"))){
            2  
          } 
      else{
          event_data("plotly_click", source = "heat_plot")[["pointNumber"]] + 2
          
      }
      
  })
  

  # Display matches based on table selection ----
  output$MyPlotC <- renderPlotly({
    req(input$file1)
    #if(grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)",
     #         ignore.case = T, filename$data)){
        #req(single_data$data)
      plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
        add_trace(x = data()[["wavenumber"]], y = make_rel(data()[[data_click()]], na.rm = T),
                  name = 'Uploaded Spectrum',
                  line = list(color = 'rgba(240,236,19,0.8)')) %>%
          add_trace(x = if(input$active_preprocessing){baseline_data()[["wavenumber"]]} else{NULL}, y = if(input$active_preprocessing){make_rel(baseline_data()[[data_click()]], na.rm = T)} else{NULL},
                    name = 'Processed Spectrum',
                    line = list(color = 'rgb(240,19,207)')) %>%
          add_trace(data = match_selected(), x = ~wavenumber, y = ~intensity,
                    name = 'Selected Match',
                    line = list(color = 'rgb(255,255,255)')) %>%
          add_trace(data = DataR_plot(), x = ~wavenumber, y = ~intensity,
                    name = 'Matched Spectrum',
                    line = list(color = 'rgb(125,249,255)')) %>%
        # Dark blue rgb(63,96,130)
        # https://www.rapidtables.com/web/color/RGB_Color.html https://www.color-hex.com/color-names.html
        layout(yaxis = list(title = "absorbance intensity [-]"),
               xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                            autorange = "reversed"),
               plot_bgcolor = 'rgb(17,0,73)',
               paper_bgcolor = 'rgba(0,0,0,0.5)',
               font = list(color = '#FFFFFF')) %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    #}
    })
  
  output$heatmap <- renderPlotly({
      req(input$file1)
      req(ncol(data()) > 2)
        plot_ly(source = "heat_plot") %>%
            add_heatmap(
                x = preprocessed$data$coords$x, #Need to update this with the new rout format. 
                y = preprocessed$data$coords$y, 
                z = if(input$active_identification){preprocessed$data$coords$max_cor} else if(input$active_preprocessing){ preprocessed$data$coords$snr
} else{1:length(preprocessed$data$coords$y)}#,
                #text = paste0(bind_matches$names, bind_matches$identity)
            ) %>%
            layout(plot_bgcolor = 'rgb(17,0,73)',
                   paper_bgcolor = 'rgba(0,0,0,0.5)',
                   font = list(color = '#FFFFFF'),
                   title = if(input$active_identification)"Correlation"  else if(input$active_preprocessing) "Signal to Noise"  else "Spectrum Number") %>%
            event_register("plotly_click") 
  })
     
      
      

  # Data Download options
  output$downloadData5 <- downloadHandler(
    filename = function() {"ftir_library.csv"},
    content = function(file) {fwrite(spec_lib[["ftir"]][["library"]], file)}
  )

  output$downloadData6 <- downloadHandler(
    filename = function() {"raman_library.csv"},
    content = function(file) {fwrite(spec_lib[["raman"]][["library"]], file)}
  )

  output$downloadData4 <- downloadHandler(
    filename = function() {"raman_metadata.csv"},
    content = function(file) {fwrite(spec_lib[["raman"]][["metadata"]], file)}
  )

  output$downloadData3 <- downloadHandler(
    filename = function() {"ftir_metadata.csv"},
    content = function(file) {fwrite(spec_lib[["ftir"]][["metadata"]], file)}
  )

  output$download_testdata <- downloadHandler(
    filename = function() {"testdata.csv"},
    content = function(file) {fwrite(testdata, file)}
  )

  ## Download own data ----
  
  output$download_conformed <- downloadHandler(
      filename = function() {paste('data-conformed-', human_ts(), '.csv', sep='')},
      content = function(file) {fwrite(data(), file)}
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {paste('data-processed-', human_ts(), '.csv', sep='')},
    content = function(file) {fwrite(baseline_data(), file)}
  )
  
  ## Download selected data ----
  output$download_selected <- downloadHandler(
    filename = function() {paste('data-selected-', human_ts(), '.csv', sep='')},
    content = function(file) {fwrite(match_selected(), file)}
  )

  ## Download matched data ----
  output$download_matched <- downloadHandler(
    filename = function() {paste('data-matched-', human_ts(), '.csv', sep='')},
    content = function(file) {fwrite(DataR_plot(), file)}
  )
  
  ## Download matched data ----
  output$download_metadata <- downloadHandler(
    filename = function() {paste('data-analysis-metadata-', human_ts(), '.csv', sep='')},
    content = function(file) {fwrite(user_metadata(), file)}
  )
  
  ## Download validation data ----
  output$validation_download <- downloadHandler(
      filename = function() {paste('data-analysis-validation-', human_ts(), '.csv', sep='')},
      content = function(file) {fwrite(validation$data, file)}
  )
  
  ## Sharing data ----
  # Hide functions which shouldn't exist when there is no internet or
  # when the API token doesn't exist

  observe({
    if (input$baseline_selection == "Polynomial") {
      show("baseline")
      hide("go")
      hide("reset")
    } else {
      hide("baseline")
      show("go")
      show("reset")
    }
  })
  
  observe({
    if (is.null(preprocessed$data)) {
      show("placeholder1")
      show("placeholder2")
      show("placeholder3")
    } else {
      hide("placeholder1")
      hide("placeholder2")
      hide("placeholder3")
    }
  })

  output$translate <- renderUI({
    if(translate & curl::has_internet()) {
      includeHTML("www/googletranslate.html")
    }
  })



  output$tweet1 <- renderUI({
    render_tweet(tweets[1])
  })

  output$tweet2 <- renderUI({
    render_tweet(tweets[2])
  })

  #Validate the app functionality for default identification ----
  
  observeEvent(input$validate, {
    load("data/library.RData") 
    base <- 10
    cols <- sample(1:ncol(library), 100, replace = F)
    preprocessed$data$spectra <- setcolorder(library[,..cols][,wavenumber := std_wavenumbers], "wavenumber")
    preprocessed$data$coords <- expand.grid(x = 1:10, y = 1:10)
    
 })
  
  # Log events ----

  observeEvent(input$go, {
    if(conf$log) {
      if(db) {
        database$insert(data.frame(user_name = input$fingerprint,
                                   session_name = session_id,
                                   wavenumber = trace$data$wavenumber,
                                   intensity = trace$data$intensity,
                                   data_id = digest::digest(preprocessed$data,
                                                            algo = "md5"),
                                   ipid = input$ipid,
                                   time = human_ts()))
        }
    }
  })
  
  user_metadata <- reactive({
    data.frame(
             user_name = input$fingerprint,
             time = human_ts(),
             session_name = session_id,
             data_id = digest::digest(preprocessed$data, algo = "md5"),
             ipid = input$ipid,
             active_preprocessing = input$active_preprocessing,
             intensity_adj = input$intensity_corr,
             smooth_decision = input$smooth_decision,
             smoother = input$smoother,
             baseline_decision = input$baseline_decision,
             baseline_type = input$baseline_selection,
             baseline = input$baseline,
             range_decision = input$range_decision,
             max_range = input$MinRange,
             min_range = input$MaxRange,
             active_identification = input$active_identification,
             spectra_type = input$Spectra,
             #analyze_type = input$Data,
             #region_type = input$Library,
             id_level = input$id_level)
  })

  observe({
    req(input$file1)
    req(input$share_decision)
    if(conf$log) {
      if(db) {
        database$insert(user_metadata())
      } else {
        loggit("INFO", "trigger",
               user_name = input$fingerprint,
               session_name = session_id,
               intensity_adj = input$intensity_corr,
               smoother = input$smoother,
               smooth_decision = input$smooth_decision,
               baseline = input$baseline,
               baseline_decision = input$baseline_decision,
               max_range = input$MinRange,
               min_range = input$MaxRange,
               range_decision = input$range_decision,
               data_id = digest::digest(preprocessed$data, algo = "md5"),
               spectra_type = input$Spectra,
               #analyze_type = input$Data,
               #region_type = input$Library,
               ipid = input$ipid,
               time = human_ts())
      }
    }

  })
  
  #Test ----
  output$event_test <- renderPrint({
      print(data_click())
      print(dim(data()))
      print(input$active_preprocessing)
      print(input$range_decision) 
      print(input$MinRange)
      print(input$MaxRange)
      print(input$smooth_decision)
      print(input$smoother)
      print(input$baseline_decision) 
      print(input$baseline_selection) 
      print(input$baseline)
      print(preprocessed$data$coords$snr)
      print(baseline_data())
  })

})

