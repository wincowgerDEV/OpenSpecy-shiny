#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#'
#'
# Check for Auth Tokens and setup, you can change these to test the triggering
# of functions without removing the files.
droptoken <- file.exists("data/droptoken.rds")
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
if(droptoken) library(rdrop2)

#devtools::install_github("wincowgerDEV/OpenSpecy")
library(OpenSpecy)

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
  test_lib <- class(tryCatch(check_lib(path = conf$library_path),
                             warning = function(w) {w}))

  if(any(test_lib == "warning")) get_lib(path = conf$library_path)

  
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
                           #approx(x = x, y = y, xout = seq(round_up(min(x), 5), round_down(max(x), 5), by = 5))$y#,
                           p = 3, n = 11, m = 1
        )
      ) 
    )
  )
}

clean_spec <- function(x, y){
  #abs(
  c(
    #scale( 
    #signal::sgolayfilt(
    approx(x = x, y = y, xout = seq(round_up(min(x), 5), round_down(max(x), 5), by = 5))$y#,
    #p = 3, n = 11, m = 1
    #      )
    #) 
    #  )
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

#Round Down
round_down <- function(x, b){
  x - x %% b
}
#Round Up
round_up <- function(x, b){
  x + b - x %% b  
}



# This is the actual server functions, all functions before this point are not
# reactive
server <- shinyServer(function(input, output, session) {
  #For theming
  #bs_themer()
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

  #brks <- seq(5, 320000, 1000)
  clrs <- colorRampPalette(c("white", "#6baed6"))(5 + 1)

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

  # Save the metadata and data submitted upon pressing the button
  observeEvent(input$submit, {
    if (input$share_decision & !is.null(data()) & curl::has_internet()) {
      withProgress(message = "Sharing Metadata",
                   value = 3/3, {
        sout <- tryCatch(share_spec(
          data = preprocessed_data(),
          metadata = sapply(names(namekey)[c(1:24,32)], function(x) input[[x]]),
          share = conf$share,
          id = id()),
          warning = function(w) {w}, error = function(e) {e})

        if (inherits(sout, "simpleWarning") | inherits(sout, "simpleError"))
          mess <- sout$message

        if (is.null(sout)) {
          show_alert(
            title = "Thank you for sharing your data!",
            text = "Your data will soon be available at https://osf.io/stmv4/",
            type = "success"
          )
        } else {
          show_alert(
            title = "Something went wrong :-(",
            text = paste0("All mandatory data added? R says: '", mess, "'. ",
                          "Try again."),
            type = "warning"
          )
        }
      })
    }
  })

  # Read in data when uploaded based on the file type
  preprocessed_data <- reactive({
    req(input$file1)
    file <- input$file1
    filename <- as.character(file$datapath)

    if (!grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)",
              ignore.case = T, filename)) {
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
      if(grepl("\\.csv$", ignore.case = T, filename)) {
        rout <- tryCatch(read_text(filename, method = "fread",
                                   share = share,
                                   id = id()),
                         error = function(e) {e})
      }
      else if(grepl("\\.[0-9]$", ignore.case = T, filename)) {
        rout <- tryCatch(read_0(filename, share = share, id = id()),
                         error = function(e) {e})
      }
      else {
        ex <- strsplit(basename(filename), split="\\.")[[1]]

        rout <- tryCatch(do.call(paste0("read_", tolower(ex[-1])),
                                 list(filename, share = share, id = id())),
                         error = function(e) {e})
      }

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
      } else {
        rout
      }
    })
  })

  # Corrects spectral intensity units using the user specified correction
  data <- reactive({
    req(preprocessed_data())
    adj_intens(data.table(wavenumber = seq(round_up(min(preprocessed_data()$wavenumber), 5), round_down(max(preprocessed_data()$wavenumber), 5), by = 5), intensity = clean_spec(preprocessed_data()$wavenumber, preprocessed_data()$intensity)), type = input$intensity_corr)  # j is not limited to just aggregations also expansions
    })

  #Preprocess Spectra ----
  # All cleaning of the data happens here. Smoothing and Baseline removing
  baseline_data <- reactive({
    if(!length(data()) | !input$active_preprocessing) {
      data.table(intensity = numeric(), wavenumber = numeric())
    }
    else{
      
    testdata <- data() %>% dplyr::filter(wavenumber > input$MinRange &
                                           wavenumber < input$MaxRange)
    test <-  nrow(testdata) < 3
    if (test) {
      data() %>%
        mutate(intensity = if(input$smooth_decision) {
          smooth_intens(.$wavenumber, .$intensity, p = input$smoother)$intensity
        } else .$intensity) %>%
        mutate(intensity = if(input$baseline_decision) {
          subtr_bg(.$wavenumber, .$intensity, degree = input$baseline)$intensity
          } else .$intensity)
    } else {
      data() %>%
        dplyr::filter(
          if(input$range_decision) {wavenumber > input$MinRange &
              wavenumber < input$MaxRange} else {
                wavenumber == wavenumber}) %>%
        mutate(intensity = if(input$smooth_decision) {
          smooth_intens(.$wavenumber, .$intensity, p = input$smoother)$intensity
        } else .$intensity) %>%
        mutate(intensity = if(input$baseline_decision & input$baseline_selection == "Polynomial") {
          subtr_bg(.$wavenumber, .$intensity, degree = input$baseline)$intensity
          }
          else if(input$baseline_decision & input$baseline_selection == "Manual" & !is.null(trace$data)){
            make_rel(.$intensity - approx(trace$data$wavenumber, trace$data$intensity, xout = .$wavenumber, rule = 2, method = "linear", ties = mean)$y)
          } else .$intensity)
    }
    }

  })


trace <- reactiveValues(data = NULL)

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
  #js$resetClick()
  #runjs("Shiny.setInputValue('plotly_selected-B', null);")
  trace$data <- NULL
})

#  output$text <- renderPrint({
#   trace$data#
#    })

  # Choose which spectrum to use
  DataR <- reactive({
      if(input$Data == "uploaded") {
      data() %>% 
        right_join(data.table(wavenumber = seq(405, 3995, by = 5))) %>%
        pull(intensity)
    }
    else if(input$Data == "processed" & input$active_preprocessing) {
      baseline_data() %>% 
        right_join(data.table(wavenumber = seq(405, 3995, by = 5))) %>%
        pull(intensity)
    }
    else if(input$Data == "derivative" & input$active_preprocessing) {
      baseline_data() %>% 
        mutate(intensity = process_cor_os(intensity)) %>%
        right_join(data.table(wavenumber = seq(405, 3995, by = 5))) %>%
        pull(intensity)
    }
    else if(input$Data == "derivative" & !input$active_preprocessing) {
      data() %>% 
        mutate(intensity = process_cor_os(intensity)) %>%
        right_join(data.table(wavenumber = seq(405, 3995, by = 5))) %>%
        pull(intensity)
    }
    else{ #Should change this to just forcing that this option isn't selectable. 
      show_alert(
        title = "Impossible task!",
        text = paste0("We can't search for preprocessed spectra if preprocessing isn't activated","'. ",
                      "Try again."),
        type = "warning"
      )
    }
    
  })
  
  DataR_plot <- reactive({
    if(!input$active_identification) {
      data.table(wavenumber = numeric(), 
                 intensity = numeric(), 
                 SpectrumIdentity = factor())    }
    else{
      data.table(wavenumber = seq(405, 3995, by = 5),
                 intensity = make_rel(DataR(), na.rm = T),
                 SpectrumIdentity = factor()) %>%
        dplyr::filter(!is.na(intensity))
    }
    
  })
  
  libraryR <- reactive({
    req(input$file1)
    req(input$active_identification)
    if(input$Library == "full") {
      load("data/library.RData") #Nest these in here so that they don't load automatically unless needed. 
    }
    else if (input$Library == "derivative"){
      load("data/library_deriv.RData") #Nest these in here so that they don't load automatically unless needed. 
    }
    else if (input$Library == "peaks"){
      load("data/library_peaks.RData") #Nest these in here so that they don't load automatically unless needed. 
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
    if(!length(input$event_rows_selected) | !input$active_identification) {
    data.table(wavenumber = numeric(), 
               intensity = numeric(), 
               SpectrumIdentity = factor())
      }
      else{
        id_select <- ifelse(is.null(input$event_rows_selected),
                      1,
                      MatchSpectra()[[input$event_rows_selected,
                                      "sample_name"]])
    # Get data from find_spec
    current_spectrum <- data.table(wavenumber = seq(405, 3995, by = 5), 
                                   intensity = libraryR()[[id_select]], 
                                   sample_name = id_select)
  
    current_spectrum %>%
      inner_join(meta, by = "sample_name") %>%
      select(wavenumber, intensity, SpectrumIdentity) %>%
      mutate(intensity = make_rel(intensity, na.rm = T)) %>%
      dplyr::filter(!is.na(intensity))
      }
    
  
  })
  
  # Identify Spectra function ----
  # Joins their spectrum to the internal database and computes correlation.
  MatchSpectra <- reactive ({
    req(input$file1)
    req(input$active_identification)
    input
    withProgress(message = 'Analyzing Spectrum', value = 1/3, {

      incProgress(1/3, detail = "Finding Match")
      

      correlations <- cor(DataR(),libraryR(), use = "pairwise.complete.obs")
      
      Lib <- left_join(meta, data.table(sample_name = colnames(correlations), rsq = round(correlations[1,], 2))) %>%
        arrange(desc(rsq))
      

      incProgress(1/3, detail = "Making Plot")

    })
    return(Lib)
  })

  # Create the data tables
  output$event <- DT::renderDataTable({
    datatable(MatchSpectra() %>%
                dplyr::rename("Material" = SpectrumIdentity) %>%
                dplyr::rename("Pearson's r" = rsq) %>%
                dplyr::select(if(input$id_level == "deep"){"Material"} else if(input$id_level == "pp_optimal"){"polymer"}, `Pearson's r`),
              options = list(searchHighlight = TRUE,
                             scrollX = TRUE,
                             sDom  = '<"top">lrt<"bottom">ip',
                             lengthChange = FALSE, pageLength = 5),
              rownames = FALSE,
              filter = "top", caption = "Selectable Matches",
              style = "bootstrap",
              
              selection = list(mode = "single", selected = c(1)))
  })


  output$eventmetadata <- DT::renderDataTable({
    # Default to first row if not yet clicked
    id_select <- ifelse(is.null(input$event_rows_selected),
                        1,
                        input$event_rows_selected)
    # Get data from find_spec
    current_meta <- MatchSpectra()[input$event_rows_selected,] %>%
      select(where(~!any(is_empty(.))))
    #names(current_meta) <- namekey[names(current_meta)]

    datatable(current_meta,
              escape = FALSE,
              options = list(dom = 't', bSort = F, 
                             scrollX = TRUE,
                             lengthChange = FALSE,
                             info = FALSE),
              rownames = FALSE,
              style = 'bootstrap', caption = "Selection Metadata",
              selection = list(mode = 'none'))
  })

  # Display matches based on table selection ----
  output$MyPlotC <- renderPlotly({
      plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
        add_trace(data = DataR_plot(), x = ~wavenumber, y = ~intensity,
                  name = 'Matched Spectrum',
                  line = list(color = 'rgb(125,249,255)')) %>%
        add_trace(data = match_selected(), x = ~wavenumber, y = ~intensity,
                  name = 'Selected Match',
                  line = list(color = 'rgb(255,255,255)')) %>%
        add_trace(data = baseline_data(), x = ~wavenumber, y = ~intensity,
                  name = 'Processed Spectrum',
                  line = list(color = 'rgb(240,19,207)')) %>%
        add_trace(data = data(), x = ~wavenumber, y = ~intensity,
                  name = 'Uploaded Spectrum',
                  line = list(color = 'rgba(240,236,19,0.8)')) %>%
        # Dark blue rgb(63,96,130)
        # https://www.rapidtables.com/web/color/RGB_Color.html https://www.color-hex.com/color-names.html
        layout(yaxis = list(title = "absorbance intensity [-]"),
               xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                            autorange = "reversed"),
               plot_bgcolor = 'rgb(17,0,73)',
               paper_bgcolor = 'rgba(0,0,0,0.5)',
               font = list(color = '#FFFFFF')) %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape" ))
   
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
  ## Sharing data ----
  # Hide functions which shouldn't exist when there is no internet or
  # when the API token doesn't exist
  observe({
    if((conf$share == "dropbox" & droptoken) | curl::has_internet()) {
      show("share_decision")
      show("share_meta")
    }
    else {
      hide("share_decision")
      hide("share_meta")
    }
  })

  observe({
    if (input$share_decision) {
      show("share_meta")
      } else {
        hide("share_meta")
        sapply(names(namekey)[c(1:24,32)], function(x) hide(x))
        hide("submit")
      }
  })


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
    if (is.null(preprocessed_data())) {
      show("placeholder1")
      show("placeholder2")
      show("placeholder3")
    } else {
      hide("placeholder1")
      hide("placeholder2")
      hide("placeholder3")
    }
  })

  #This toggles the hidden metadata input layers.
  observeEvent(input$share_meta, {
    sapply(names(namekey)[c(1:24,32)], function(x) toggle(x))
    toggle("submit")
  })

  output$translate <- renderUI({
    if(translate & curl::has_internet()) {
      includeHTML("www/googletranslate.html")
    }
  })

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

  output$tweet1 <- renderUI({
    render_tweet(tweets[1])
  })

  output$tweet2 <- renderUI({
    render_tweet(tweets[2])
  })


  # Log events ----

  observeEvent(input$go, {
    if(conf$log) {
      if(db) {
        database$insert(data.frame(user_name = input$fingerprint,
                                   session_name = session_id,
                                   wavenumber = trace$data$wavenumber,
                                   intensity = trace$data$intensity,
                                   data_id = digest::digest(preprocessed_data(),
                                                            algo = "md5"),
                                   ipid = input$ipid,
                                   time = human_ts()))
        }
    }
  })

  observe({
    req(input$file1)
    req(input$share_decision)
    if(conf$log) {
      if(db) {
        database$insert(data.frame(user_name = input$fingerprint,
                                   session_name = session_id,
                                   intensity_adj = input$intensity_corr,
                                   smoother = input$smoother,
                                   smooth_decision = input$smooth_decision,
                                   baseline = input$baseline,
                                   baseline_decision = input$baseline_decision,
                                   max_range = input$MinRange,
                                   min_range = input$MaxRange,
                                   range_decision = input$range_decision,
                                   data_id = digest::digest(preprocessed_data(),
                                                            algo = "md5"),
                                   spectra_type = input$Spectra,
                                   analyze_type = input$Data,
                                   region_type = input$Library,
                                   ipid = input$ipid,
                                   time = human_ts()))
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
               data_id = digest::digest(preprocessed_data(), algo = "md5"),
               spectra_type = input$Spectra,
               analyze_type = input$Data,
               region_type = input$Library,
               ipid = input$ipid,
               time = human_ts())
      }
    }

  })

})

