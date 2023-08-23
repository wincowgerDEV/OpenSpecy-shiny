function(input, output, session) {
    
  #Set upload size
  if(conf$share != "system"){options(shiny.maxRequestSize = 1000*1024^2)} else{options(shiny.maxRequestSize = 10000*1024^2)}
    
  #create a random session id
  session_id <- digest(runif(10))

  # Loading overlay
  load_data()
  hide(id = "loading_overlay", anim = TRUE, animType = "fade")
  show("app_content")

  #Reactive Values ----
  preprocessed <- reactiveValues(data = NULL)
  trace <- reactiveValues(data = NULL)
  data_click <- reactiveValues(data = NULL)


  #Sending data to a remote repo. 
observeEvent(input$file, {
  # Read in data when uploaded based on the file type
  req(input$file)
  data_click$data <- 1

  if (!grepl("(\\.json$)|(\\.rds$)|(\\.yml$)|(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.RData$)|(\\.zip$)|(\\.[0-9]$)",
             ignore.case = T, as.character(input$file$datapath))) {
    show_alert(
      title = "Data type not supported!",
      text = paste0("Uploaded data type is not currently supported; please
                      check tooltips and 'About' tab for details."),
      type = "warning")
    return(NULL)
  }

  if (input$share_decision & curl::has_internet()) {
    progm <- "Sharing Spectrum to Community Library"
  } else {
    progm <- "Reading Spectrum"
  }

  withProgress(message = progm, value = 3/3, {

      rout <- tryCatch({
                      read_any(file = as.character(input$file$datapath))
                  }, error = function(e) {
                      paste("Error:", e$message)
                   })

      if(droptoken & input$share_decision & input$file$size < 10^7 & curl::has_internet()){
          put_object(
              file = file.path(as.character(input$file$datapath)),
              object = paste0("users/", "/", session_id, "/", digest(rout), "/", gsub(".*/", "", as.character(input$file$name))),
              bucket = "openspecy"
          )
      }

    if (inherits(rout, "simpleError")) {
      reset("file")
      show_alert(
        title = "Something went wrong :-(",
        text = paste0("R says: '", rout$message, "'. ",
                      "If you uploaded a text/csv file, make sure that the ",
                      "columns are numeric and named 'wavenumber' and ",
                      "'intensity'."),
        type = "error"
      )
      preprocessed$data <- NULL
    }
    else {
      preprocessed$data <- rout
    }
})
})

  # Corrects spectral intensity units using the user specified correction

 # Redirecting preprocessed data to be a reactive variable. Not totally sure why this is happening in addition to the other. 
 data <- reactive({
    req(input$file)
      preprocessed$data
    })

  #Preprocess Spectra ----
  
  # All cleaning of the data happens here. Range selection, Smoothing, and Baseline removing
  baseline_data <- reactive({
     req(input$file)
     req(input$active_preprocessing)
    process_spec(x = data(),
                    active = input$active_preprocessing,
                    adj_intens = input$intensity_decision, 
                    adj_intens_args = list(type = input$intensity_corr),
                    conform_spec = T, 
                    conform_args = list(range = seq(100, 4000, by = 5), res = 5),
                    restric_range = input$range_decision,
                    restric_range_args = list(min = input$MinRange, max = input$MaxRange),
                    flatten_range = input$co2_decision,
                    flatten_range_args = list(min = input$MinFlat, max = input$MaxFlat),
                    subtr_baseline = input$baseline_decision, 
                    subtr_baseline_args = list(type = "polynomial", degree = input$baseline, raw = FALSE, baseline = NULL),
                    smooth_intens = input$smooth_decision, 
                    smooth_intens_args = list(polynomial = input$smoother, window = input$smoother_window, derivative = input$derivative_order, abs = input$derivative_abs),
                    make_rel = T)
  })

#Updating the trace value when the user says go. 
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

#Resetting the trace if the user specifies. 
observeEvent(input$reset, {
  trace$data <- NULL
})

# Identify Spectra function ----

  # Choose which spectra to use for matching and plotting. 
  DataR <- reactive({
      if(input$active_preprocessing) {
        baseline_data()
    }
    else {
        conform_spec(data(), 
                     range = seq(100, 4000, by = 5), 
                     res = 5)
    }
  })

  #The data to use in the plot. 
  DataR_plot <- reactive({
      if(is.null(preprocessed$data)){
          list(wavenumber = numeric(), spectra = data.table(empty = numeric()))
      }
      else{
          filter_spec(DataR(), logic = 1:ncol(DataR()$spectra) == data_click$data) 
      }
  })

  #The matching library to use. 
  libraryR <- reactive({
    req(input$active_identification)
    if(input$id_strategy == "mediod"){
        library <- test_lib 
        library$metadata$SpectrumIdentity <- library$metadata$polymer_class 
        return(library)
    }
    else if(grepl("nobaseline$", input$id_strategy)) {
        library <- nobaseline_library
    }
    else if(grepl("deriv$", input$id_strategy)){
        library <- derivative_library
    }
    if(grepl("^both", input$id_strategy)) {
      library
    }
    else if (grepl("^ftir", input$id_strategy)){
      filter_spec(library, logic = library$metadata$SpectrumType == "FTIR")
    }
    else if (grepl("^raman", input$id_strategy)){
      filter_spec(library, logic = library$metadata$SpectrumType == "Raman")
    }
  })
  
  #Correlation ----
  output$correlation_head <- renderUI({
      boxLabel(text = if(input$active_identification) {"Cor"} else{"SNR"}, 
               status = if(input$active_identification) {
                   if(max_cor()[[data_click$data]] > MinCor() & signal_to_noise()[[data_click$data]] > MinSNR()){
                       "success"
                       } 
                   else{
                       "error"
                       }
                   } else{
                       if(signal_to_noise()[[data_click$data]] > MinSNR()){
                           "success"
                           } 
                       else{
                           "error"
                           }
                       }, 
               tooltip = "This tells you whether the signal to noise ratio or the match observed is above or below the thresholds.")
  })
  
  #Progress bar for how many of the spectra have good signal. 
  observeEvent(MinSNR() | signal_to_noise(), {
      req(input$file)
      updateProgressBar(session = session, 
                        id = "signal_progress", 
                        value = sum(signal_to_noise() > MinSNR())/length(signal_to_noise()) * 100)
  })
  #Bars stating how many of the uploaded spectra have good correlations or signals. 
  observeEvent(MinCor() | max_cor(), {
      req(input$file)
      updateProgressBar(session = session, 
                        id = "correlation_progress", 
                        value = sum(max_cor() > MinCor())/length(max_cor()) * 100)
      updateProgressBar(session = session, 
                        id = "match_progress", 
                        value = (sum(signal_to_noise() > MinSNR() & max_cor() > MinCor())/length(signal_to_noise())) * 100)
  })
  
  #The correlation matrix between the unknowns and the library. 
  correlation <- reactive({
      req(input$file)
      req(input$active_identification)
      req(!grepl("^ai$", input$id_strategy))
      withProgress(message = 'Analyzing Spectrum', value = 1/3, {
      cor_spec(x = DataR(), 
                        library = libraryR())
      })
  })

  #The signal to noise ratio
  signal_to_noise <- reactive({
          req(DataR)
      signal_option <- switch(input$signal_selection,
             "Signal Over Noise" = "run_sig_over_noise", 
             "Signal Times Noise" = "sig_times_noise", 
             "Total Signal" = "log_tot_sig")
      sig_noise(x = DataR(), return = signal_option)
  })
  
  MinSNR <- reactive({
      if(!input$threshold_decision){
          -Inf
      }
      else{
          input$MinSNR
      }
  })
  
  
  output$snr_plot <- renderPlot({
        ggplot() +
          geom_histogram(aes(x = signal_to_noise())) +
          scale_x_continuous(trans =  scales::modulus_trans(p = 0, offset = 1)) +
          geom_vline(xintercept = MinSNR(), color = "red") +
          theme_minimal()
  })

  #The output from the AI classification algorithm. 
  ai_output <- reactive({ #tested working. 
      req(input$file)
      req(input$active_identification)
      req(input$id_strategy == "ai")
      ai_classify(data = DataR()$spectra, 
                  wavenumbers = DataR()$wavenumber, 
                  model = model,
                  means = means)
  })
  
  #The maximum correlation or AI value. 
  max_cor <- reactive({
      req(input$file)
      #req(input$active_identification)
      if(isTruthy(input$active_identification)){
          if(!grepl("^ai$", input$id_strategy)){
          max_cor_named(correlation())
      }
      else if(input$id_strategy == "ai"){
          ai <- signif(ai_output()[["value"]], 1)
          names(ai) <- ai_output()[["name"]]
          ai
        }
      }
      else{
          NULL
      }
  })
  
  MinCor <- reactive({
      if(!input$cor_threshold_decision){
          -Inf
      }
      else{
          input$MinCor
      }
  })
  
  output$cor_plot <- renderPlot({
      ggplot() +
          geom_histogram(aes(x = max_cor())) +
          scale_x_continuous(trans =  scales::modulus_trans(p = 0, offset = 1)) +
          geom_vline(xintercept = MinCor(), color = "red") +
          theme_minimal()
  })
  
  #Metadata for all the top correlations.
  top_correlation <- reactive({
      req(input$file)
      req(input$active_identification)
      data.table(object_id = names(DataR()$spectra), 
                 library_id = names(max_cor()),
                 match_val = max_cor(), 
                 match_threshold = MinCor(),
                 good_correlations = max_cor() > MinCor(),
                 signal_to_noise = signal_to_noise(), 
                 signal_threshold = MinSNR(),
                 good_signal = signal_to_noise() > MinSNR(), 
                 good_matches = max_cor() > MinCor() & signal_to_noise() > MinSNR()) %>%
          {if(!grepl("^ai$", input$id_strategy)){bind_cols(., DataR()$metadata)} else{.}} %>%
          {if(!grepl("^ai$", input$id_strategy)){left_join(., libraryR()$metadata, by = c("library_id" = "sample_name"))} else{.}}
          
  })
  
  #Metadata for all the matches for a single unknown spectrum
  matches_to_single <- reactive({
      req(input$active_identification)
      req(!grepl("^ai$", input$id_strategy))
      if(is.null(preprocessed$data)){
          libraryR()$metadata %>%
              rename("library_id" = "sample_name")
      }
      else{
          data.table(object_id = names(DataR()$spectra)[data_click$data], 
                     library_id = names(libraryR()$spectra),
                     match_val = c(correlation()[,data_click$data]))[order(-match_val),] %>%
              left_join(libraryR()$metadata, by = c("library_id" = "sample_name"))
      }
  })

  #Spectral data for the selected match. 
  match_selected <- reactive({# Default to first row if not yet clicked
      #req(input$file)
      #req(input$active_identification)
      req(!grepl("^ai$", input$id_strategy))
      if(!input$active_identification) {
          as_OpenSpecy(x = numeric(), spectra = data.table(empty = numeric()))
      }
      else{
         #need to make reactive
          id_select <-  ifelse(is.null(input$event_rows_selected),
                              matches_to_single()[[1,"library_id"]],
                              matches_to_single()[[input$event_rows_selected,"library_id"]])#"00087f78d45c571524fce483ef10752e"	#matches_to_single[[1,column_name]]
              
          # Get data from filter_spec
          filter_spec(libraryR(), logic = id_select)
      }
  })

  #All matches table for the current selection
  top_matches <- reactive({
      #req(input$file)
      req(input$active_identification)
      req(!grepl("^ai$", input$id_strategy))
      if(is.null(preprocessed$data)){
          matches_to_single() %>%
              dplyr::rename("Material" = 'SpectrumIdentity',
                            "Plastic Pollution Category" = "polymer_class") %>%
              dplyr::select("Material",
                            "Plastic Pollution Category", 
                            "library_id")
      }
      else{
          matches_to_single() %>%
              mutate(match_val = signif(match_val, 2))  %>%
              dplyr::rename("Material" = SpectrumIdentity,
                            "Pearson's r" = match_val,
                            "Plastic Pollution Category" = "polymer_class") %>%
              dplyr::select("Pearson's r",
                            "Material",
                            "Plastic Pollution Category", 
                            "library_id")
      }
      
  })

  # Create the data tables for all matches
  output$event <- DT::renderDataTable({
    req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
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

  #Create the data table that goes below the plot which provides extra metadata. 
match_metadata <- reactive({
    req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
    if(is.null(preprocessed$data)){
        matches_to_single()[input$event_rows_selected,] %>%
            dplyr::rename("Material" = SpectrumIdentity,
                          "Plastic Pollution Category" = "polymer_class") %>%
            .[, !sapply(., OpenSpecy::is_empty_vector), with = F]
    }
    else{
        matches_to_single()[input$event_rows_selected,] %>%
            dplyr::rename("Material" = SpectrumIdentity,
                          "Pearson's r" = match_val,
                          "Plastic Pollution Category" = "polymer_class") %>%
            .[, !sapply(., OpenSpecy::is_empty_vector), with = F]
    }
})

#Table of metadata for the selected library value
 output$eventmetadata <- DT::renderDataTable({
    req(input$active_identification)
    req(!grepl("^ai$", input$id_strategy))
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


  # Display paired spectral matches based on table selection ----
  output$MyPlotC <- renderPlotly({
      #req(input$id_strategy == "correlation")
      #req(preprocessed$data)
      
      plotly_spec(x = if(!is.null(preprocessed$data)){DataR_plot()} else{match_selected()},x2 = if(!is.null(preprocessed$data)) {match_selected()} else{NULL}, source = "B") %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })

 #Display the map or batch data in a selectable heatmap. 
  output$heatmap <- renderPlotly({
      req(input$file)
      heatmap_spec(x = DataR(), 
                        sn = signif(signal_to_noise(), 2), 
                        cor = if(is.null(max_cor())){max_cor()} else{signif(max_cor(), 2)}, 
                        min_sn = MinSNR(),
                        min_cor = MinCor(),
                        selected_spectrum = data_click$data,
                        source = "heat_plot") %>%
          event_register("plotly_click")
  })

  thresholded_particles <- reactive({
      if(input$active_identification){
          particles_logi <- signal_to_noise() > MinSNR() & max_cor() > MinCor()
      }
      else{
          particles_logi <- signal_to_noise() > MinSNR()
      }
      collapse_spec(
          def_features(DataR(), features = particles_logi)
      ) %>%
          filter_spec(., logic = .$metadata$particle_ids != "-88")
  })
  
  # Data Download options ----
  output$download_data <- downloadHandler(
       filename = function() {if(input$download_selection == "Test Map") {paste0(input$download_selection, human_ts(), ".zip")} else if(input$download_selection == "Thresholded Particles"){paste0(input$download_selection, human_ts(), ".rds")} else{paste0(input$download_selection, human_ts(), ".csv")}},
        content = function(file) {
            if(input$download_selection == "Test Data") {fwrite(testdata, file)}
            if(input$download_selection == "Test Map") {zip(file, unzip("data/CA_tiny_map.zip"))}
            if(input$download_selection == "Your Spectra") {fwrite(cbind(wavenumber = DataR()$wavenumber, DataR()$spectra), file)}
            if(input$download_selection == "Library Spectra") {fwrite(cbind(wavenumber = libraryR()$wavenumber, libraryR()$spectra), file)}
            if(input$download_selection == "Top Matches") {fwrite(top_correlation(), file)}
            if(input$download_selection == "Thresholded Particles") {write_spec(thresholded_particles(), file = file)}
            })

  # Hide functions or objects when the shouldn't exist. 
  observe({
    toggle(id = "signal_progress", condition = !is.null(preprocessed$data) & input$threshold_decision)
    toggle(id = "correlation_progress", condition = !is.null(preprocessed$data))
    toggle(id = "match_progress", condition = !is.null(preprocessed$data))
    toggle(id = "baseline", condition = input$baseline_selection == "Polynomial")
    toggle(id = "go", condition = input$baseline_selection == "Manual")
    toggle(id = "reset", condition = input$baseline_selection == "Manual")
    toggle(id = "heatmap", condition = !is.null(preprocessed$data))
    toggle(id = "heatmap_stats", condition = !is.null(preprocessed$data))
    toggle(id = "placeholder1", condition = is.null(preprocessed$data))
    if(!is.null(preprocessed$data)){
        toggle(id = "heatmap", condition = ncol(preprocessed$data$spectra) > 1)
        toggle(id = "heatmap_stats", condition = ncol(preprocessed$data$spectra) > 1)
    }
    if(is.null(event_data("plotly_click", source = "heat_plot"))){
        data_click$data <- 1
    }
    else{
        data_click$data <- event_data("plotly_click", source = "heat_plot")[["pointNumber"]] + 1
    }
    })

  #Google translate. 
  output$translate <- renderUI({
    if(translate & curl::has_internet()) {
      includeHTML("www/googletranslate.html")
    }
  })

  # Log events ----
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
             #spectra_type = input$Spectra,
             #analyze_type = input$Data,
             #region_type = input$Library
             )
  })

  observe({
    req(input$file)
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
               #spectra_type = input$Spectra,
               #analyze_type = input$Data,
               #region_type = input$Library,
               #ipid = input$ipid,
               time = human_ts())
      }
    }

  })
  
  #output$event_test <- renderPrint({
  #    list(
  #        preprocessed = tryCatch({
  #            libraryR()
  #        }, error = function(e) {
  #            paste("Error:", e$message)
 #         })
 #         
 #     )
 # })
  

  #Storage ----
  #stores setup - insert at the bottom  !!!IMPORTANT
  #appid = "application_OpenSpecy"
  #setupStorage(appId = appid,inputs = TRUE)

}

