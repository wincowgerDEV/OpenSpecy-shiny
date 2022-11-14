
# This is the actual server functions, all functions before this point are not
# reactive
function(input, output, session) {

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
  # Sharing USER ID
 # id <- reactive({
#    if (!is.null(input$fingerprint)) {
#      paste(input$fingerprint, session_id, sep = "/")
#    } else {
#      paste(digest(Sys.info()), digest(sessionInfo()), sep = "/")
#    }
#  })

  #Reactive Values ----
  preprocessed <- reactiveValues(data = NULL)
  trace <- reactiveValues(data = NULL)
  data_click <- reactiveValues(data = NULL)


observeEvent(input$file1, {
  # Read in data when uploaded based on the file type
  req(input$file1)
  file <- input$file1
  data_click$data <- 1
  #filename$data <- as.character(file$datapath)

  if (!grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.RData$)|(\\.zip$)|(\\.[0-9]$)",
             ignore.case = T, as.character(file$datapath))) {
    show_alert(
      title = "Data type not supported!",
      text = paste0("Uploaded data type is not currently supported; please
                      check tooltips and 'About' tab for details."),
      type = "warning")
    return(NULL)
  }

  if (input$share_decision & curl::has_internet()) {
    #share <- conf$share
    progm <- "Sharing Spectrum to Community Library"
  } else {
    #share <- NULL
    progm <- "Reading Spectrum"
  }

  withProgress(message = progm, value = 3/3, {

      rout <- read_any(
          filename = as.character(file$datapath), share = NULL, id = "test", std_wavenumbers = std_wavenumbers
      )

      if(droptoken & input$share_decision & input$file1$size < 10^7 & !inherits(tryCatch(curl(url = "www.google.com", open = "rb"), error = function(e) {e}), what = "simpleError")){
          put_object(
              file = file.path(as.character(input$file1$datapath)),
              object = paste0("users/", "/", session_id, "/", digest(rout), "/", gsub(".*/", "", as.character(file$name))),
              bucket = "openspecy"
          )
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
    }
    else {
      preprocessed$data <- rout
    }
})
})

  # Corrects spectral intensity units using the user specified correction
  data <- reactive({
    req(input$file1)
      # TODO: replace with conform_intens()
      conform_spectra(df = preprocessed$data$spectra,
                      wavenumber = preprocessed$data$wavenumber,
                      correction = input$intensity_corr)
    })

  #Preprocess Spectra ----
  observeEvent(input$MinSNR | signal_noise(), {
      req(input$file1)
      updateProgressBar(session = session, id = "signal_progress", value = sum(signal_noise() > input$MinSNR)/length(signal_noise()) * 100)
  })
  
  # All cleaning of the data happens here. Range selection, Smoothing, and Baseline removing
  baseline_data <- reactive({
     req(input$file1)
     req(input$active_preprocessing)
    #if(!length(data()) | !input$active_preprocessing) {
    #  data.table(wavenumber = numeric(), intensity = numeric(), SpectrumIdentity = factor())
    #}
    #else{
    process_spectra(df = data(),
                    wavenumber = conform_res(preprocessed$data$wavenumber),
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
                    carbon_dioxide_decision = input$co2_decision,
                    trace = trace)


    #}
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

# Identify Spectra function ----

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
    if(!input$active_identification | is.null(preprocessed$data)) {
      data.table(wavenumber = numeric(),
                 intensity = numeric(),
                 SpectrumIdentity = factor())    }
    else{
      data.table(wavenumber = conform_res(preprocessed$data$wavenumber),
                 intensity = make_rel(DataR()[[data_click$data]], na.rm = T),
                 SpectrumIdentity = factor()) %>%
        dplyr::filter(!is.na(intensity))
    }
  })

  libraryR <- reactive({
    #req(input$file1)
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



  #Correlation ----
  
  observeEvent(input$MinCor | max_cor(), {
      req(input$file1)
      updateProgressBar(session = session, id = "correlation_progress", value = sum(max_cor() > input$MinCor)/length(max_cor()) * 100)
  })
  
  correlation <- reactive({
      req(input$file1)
      req(input$active_identification)
      correlate_spectra(data = DataR(), search_wavenumbers = conform_res(preprocessed$data$wavenumber), std_wavenumbers = std_wavenumbers, library = libraryR())

  })

  signal_noise <- reactive({
          req(input$file1)
          unlist(lapply(DataR(), snr))
  })

  max_cor <- reactive({
      req(input$file1)
      req(correlation())
      round(apply(correlation(), 2, function(x) max(x, na.rm = T)), 2)
  })

  max_cor_id <- reactive({
      req(input$file1)
      req(correlation())
      colnames(libraryR())[apply(correlation(),2 , function(x) which.max(x))]
  })

  # Joins their spectrum to the internal database.
  MatchSpectra <- reactive({
    #req(input$file1)
    req(input$active_identification)
      if(is.null(preprocessed$data)) {
      Lib <-  meta %>% filter(sample_name %in% names(libraryR())) %>% mutate(rsq = NA)
      }
      else{
          #input
          withProgress(message = 'Analyzing Spectrum', value = 1/3, {

              incProgress(1/3, detail = "Finding Match")

              Lib <- get_all_metadata(sample_name = names(libraryR()), rsq = correlation()[[data_click$data]], metadata = meta)


              incProgress(1/3, detail = "Making Plot")
      })
    }
    return(Lib)
  })

  match_selected <- reactive({# Default to first row if not yet clicked
      #req(input$file1)
      #req(input$active_identification)
      if(!input$active_identification) {
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

  top_matches <- reactive({
      req(input$active_identification)
      MatchSpectra() %>%
          dplyr::rename("Material" = SpectrumIdentity) %>%
          dplyr::rename("Pearson's r" = rsq) %>%
          dplyr::select(if(input$id_level == "deep"){"Material"}
                        else if(input$id_level == "pp_optimal"){"polymer"}
                        else if(input$id_level == "pp_groups"){"polymer_class"}
                        else{"plastic_or_not"}, if(!is.null(preprocessed$data)){"Pearson's r"}, sample_name)
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
    req(input$active_identification)
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


 observeEvent(event_data("plotly_click", source = "heat_plot"), {
     if(is.null(event_data("plotly_click", source = "heat_plot"))){
        data_click$data <- 1
     }
     else{
        data_click$data <- event_data("plotly_click", source = "heat_plot")[["pointNumber"]] + 1

     }
 })

  # Display matches based on table selection ----
  output$MyPlotC <- renderPlotly({
    #req(input$file1)
    #if(grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.[0-9]$)",
     #         ignore.case = T, filename$data)){
        #req(single_data$data)
      plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
        add_trace(x = if(!is.null(preprocessed$data)) {conform_res(preprocessed$data$wavenumber)} else{NULL}, y = if(!is.null(preprocessed$data)){make_rel(data()[[data_click$data]], na.rm = T)} else{NULL},
                  name = 'Uploaded',
                  line = list(color = 'rgba(240,236,19,0.8)')) %>%
          add_trace(x = if(input$active_preprocessing & !is.null(preprocessed$data)){conform_res(preprocessed$data$wavenumber)} else{NULL}, y = if(input$active_preprocessing & !is.null(preprocessed$data)){make_rel(baseline_data()[[data_click$data]], na.rm = T)} else{NULL},
                    name = 'Processed',
                    line = list(color = 'rgb(240,19,207)')) %>%
          add_trace(data = match_selected(), x = ~wavenumber, y = ~intensity,
                    name = 'Selected',
                    line = list(color = 'rgb(255,255,255)')) %>%
          add_trace(data = DataR_plot(), x = ~wavenumber, y = ~intensity,
                    name = 'Matched',
                    line = list(color = 'rgb(125,249,255)')) %>%
        # Dark blue rgb(63,96,130)
        # https://www.rapidtables.com/web/color/RGB_Color.html https://www.color-hex.com/color-names.html
        layout(yaxis = list(title = "absorbance intensity [-]"),
               xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                            autorange = "reversed"),
               plot_bgcolor = 'rgb(17,0,73)',
               paper_bgcolor = 'rgba(0,0,0,0.5)',
               title = list(
                   text = if(!is.null(preprocessed$data)) paste0(paste0("Signal to Noise = ", round(signal_noise()[[data_click$data]], 2)), if(input$active_identification) paste0("; ", "Max Correlation = ", max_cor())) else "",
                   x = 0
               ),
               font = list(color = '#FFFFFF')) %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    #}
    })

  output$heatmap <- renderPlotly({
      req(input$file1)
      #req(ncol(data()) > 2)
        plot_ly(source = "heat_plot") %>%
            add_trace(
                x = preprocessed$data$coords$x, #Need to update this with the new rout format.
                y = preprocessed$data$coords$y,
                z = if(input$active_identification){ifelse(signal_noise() < input$MinSNR | max_cor() < input$MinCor, NA, max_cor())} else {ifelse(signal_noise() > input$MinSNR, signal_noise(), NA)
},
                type = "heatmap",
                hoverinfo = 'text',
                colors = if(input$active_identification){} else {heat.colors(n = sum(signal_noise() > input$MinSNR))
                },
                text = ~paste(
                    "x: ", preprocessed$data$coords$x,
                    "<br>y: ", preprocessed$data$coords$y,
                    "<br>z: ", if(input$active_identification){round(max_cor(), 2)} else{round(signal_noise(), 2)
                    },
                    "<br>Filename: ", preprocessed$data$coords$filename)) %>%
            layout(
              xaxis = list(title = 'x',
                           zeroline = F,
                           showgrid = F
              ),
              yaxis = list(title = 'y',
                           zeroline = F,
                           showgrid = F),
                   plot_bgcolor = 'rgba(17,0,73, 0)',
                   paper_bgcolor = 'rgba(0,0,0,0.5)',
                   font = list(color = '#FFFFFF'),
                   #legend= list(title=list(text= '<b> Correlation </b>')),
                   title = if(input$active_identification) paste0(round((1 - sum(signal_noise() < input$MinSNR | max_cor() < input$MinCor)/length(signal_noise())), 2) * 100, "% Good ID")  else paste0(round((1 - sum(signal_noise() < input$MinSNR)/length(signal_noise())), 2) * 100, "% Good Signal")) %>%
            event_register("plotly_click")
  })

  # Data Download options ----
  output$download_data <- downloadHandler(
       filename = function() {if(input$download_selection == "Test Map") {paste0(input$download_selection, human_ts(), ".zip")} else{paste0(input$download_selection, human_ts(), ".csv")}},
        content = function(file) {
            if(input$download_selection == "Test Data") {fwrite(testdata, file)}
            if(input$download_selection == "Test Map") {zip(file, unzip("data/CA_tiny_map.zip"))}
            if(input$download_selection == "Spectra Conformed") {fwrite(data()%>% mutate(wavenumber = conform_res(preprocessed$data$wavenumber)), file)}
            if(input$download_selection == "Spectra Processed") {fwrite(baseline_data() %>% mutate(wavenumber = conform_res(preprocessed$data$wavenumber)), file)}
            if(input$download_selection == "Spectra SNR") {fwrite(data.table(x = preprocessed$data$coords$x, y = preprocessed$data$coords$y, filename = preprocessed$data$coords$filename, signal_to_noise = signal_noise(), good_signal = signal_noise() > input$MinSNR), file)}
            if(input$download_selection == "Spectra Selected") {fwrite(match_selected() %>% select(-SpectrumIdentity), file)}
            if(input$download_selection == "Match Selected") {fwrite(DataR_plot() %>% select(-SpectrumIdentity), file)}
            if(input$download_selection == "Match Metadata") {fwrite(user_metadata(), file)}
            if(input$download_selection == "All Correlation Data") {fwrite(correlation %>% mutate(library_names = names(libraryR())), file)}
            if(input$download_selection == "Top Correlation Data") {fwrite(data.table(x = preprocessed$data$coords$x, y = preprocessed$data$coords$y, filename = preprocessed$data$coords$filename, signal_to_noise = signal_noise(), good_signal = signal_noise() > input$MinSNR, max_cor = max_cor(), good_cor = max_cor() > input$MinCor, max_cor_id = max_cor_id()) %>% left_join(meta, by = c("max_cor_id" = "sample_name")), file)}
            if(input$download_selection == "Validation Data") {fwrite(validation$data, file)}
            if(input$download_selection == "FTIR Library") {fwrite(spec_lib[["ftir"]][["library"]], file)}
            if(input$download_selection == "Raman Library") {fwrite(spec_lib[["raman"]][["library"]], file)}
            if(input$download_selection == "FTIR Library Metadata") {fwrite(spec_lib[["ftir"]][["metadata"]], file)}
            if(input$download_selection == "Raman Library Metadata") {fwrite(spec_lib[["raman"]][["metadata"]], file)}
            })

  ## Sharing data ----
  # Hide functions which shouldn't exist when there is no internet or
  # when the API token doesn't exist

  observe({
    toggle(id = "baseline", condition = input$baseline_selection == "Polynomial")
    toggle(id = "go", condition = input$baseline_selection == "Manual")
    toggle(id = "reset", condition = input$baseline_selection == "Manual")
    })

  #hide(id = "heatmap")

  observe({
      #req(input$file1)
      toggle(id = "download_conformed", condition = !is.null(preprocessed$data))
      toggle(id = "download_matched", condition = !is.null(preprocessed$data))
      toggle(id = "downloadData", condition = !is.null(preprocessed$data))
      toggle(id = "heatmap", condition = !is.null(preprocessed$data))
      if(!is.null(preprocessed$data)){
          toggle(id = "heatmap", condition = ncol(preprocessed$data$spectra) > 1)
      }
      #if(ncol(data()) > 1)
  })

  observe({
      toggle(id = "placeholder1", condition = is.null(preprocessed$data))
      toggle(id = "placeholder2", condition = !is.null(preprocessed$data) | input$active_identification)
      toggle(id = "placeholder3", condition = is.null(preprocessed$data))
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

  #Can use this to update the library by increasing the count to the total library size.
  observeEvent(input$validate, {
    load("data/library.RData")
    cols <- sample(1:ncol(library), 100, replace = F) # add in to reduce sample
    preprocessed$data$wavenumber <- std_wavenumbers
    preprocessed$data$spectra <- library[,..cols] #Bring this back if wanting less
    preprocessed$data$coords <- gen_grid(x = ncol(preprocessed$data$spectra))[,filename := "test"]

 })

  # Log events ----

  observeEvent(input$go, {
    if(conf$log) {
      if(db) {
        database$insert(data.frame(#user_name = input$fingerprint,
                                   session_name = session_id,
                                   wavenumber = trace$data$wavenumber,
                                   intensity = trace$data$intensity,
                                   data_id = digest::digest(preprocessed$data,
                                                            algo = "md5"),
                                   #ipid = input$ipid,
                                   time = human_ts()))
        }
    }
  })

  user_metadata <- reactive({
    data.frame(
             #user_name = input$fingerprint,
             time = human_ts(),
             session_name = session_id,
             data_id = digest::digest(preprocessed$data, algo = "md5"),
             #ipid = input$ipid,
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
               #user_name = input$fingerprint,
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
               #ipid = input$ipid,
               time = human_ts())
      }
    }

  })

  #Test ----
  output$event_test <- renderPrint({
      #print(!is.null(preprocessed$data))
      #print(input$file1)
      #print(paste0("users/", input$fingerprint,"/", session_id, "/", gsub(".*/", "", as.character(input$file1$name))))
      #print(max_cor())
      #print(max_cor_id())
      #print(preprocessed$data$coords$x)
      #print(preprocessed$data$coords$y)
      #print(preprocessed$data$coords$filename)

      #print(dim(data()))
      #print(input$active_preprocessing)
      #print(input$range_decision)
      #print(input$MinRange)
      #print(input$MaxRange)
      #print(input$smooth_decision)
      #print(input$smoother)
      #print(input$baseline_decision)
      #print(input$baseline_selection)
      #print(input$baseline)
      #print(preprocessed$data$coords$snr)
      #print(baseline_data())
  })

}

