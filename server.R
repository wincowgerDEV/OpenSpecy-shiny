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


observeEvent(input$file, {
  # Read in data when uploaded based on the file type
  req(input$file)
  data_click$data <- 1

  if (!grepl("(\\.csv$)|(\\.asp$)|(\\.spa$)|(\\.spc$)|(\\.jdx$)|(\\.RData$)|(\\.zip$)|(\\.[0-9]$)",
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

      rout <- read_any(file = as.character(input$file$datapath))

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
      return(NULL)
    }
    else {
      preprocessed$data <- rout
    }
})
})

  # Corrects spectral intensity units using the user specified correction
  data <- reactive({
    req(input$file)
      preprocessed$data
    })

  #Preprocess Spectra ----
  observeEvent(input$MinSNR | signal_to_noise(), {
      req(input$file)
      updateProgressBar(session = session, 
                        id = "signal_progress", 
                        value = sum(signal_to_noise() > input$MinSNR)/length(signal_to_noise()) * 100)
  })
  
  # All cleaning of the data happens here. Range selection, Smoothing, and Baseline removing
  baseline_data <- reactive({
     req(input$file)
     req(input$active_preprocessing)
    process_spectra(object = data(),
                    active_processing = input$active_preprocessing,
                    adj_intensity_decision = input$intensity_decision, 
                    type = input$intensity_corr,
                    conform_decision = T, 
                    new_wavenumbers = seq(100, 4000, by = 5), 
                    res = 5,
                    range_decision = input$range_decision,
                    min_range = input$MinRange,
                    max_range = input$MaxRange,
                    flatten_decision = input$co2_decision,
                    flatten_min = 2200, #update
                    flatten_max = 2420, #update
                    smooth_decision = input$smooth_decision,
                    smooth_polynomial = input$smoother,
                    baseline_decision = input$baseline_decision,
                    baseline_selection = input$baseline_selection,
                    baseline_polynomial = input$baseline,
                    raw_baseline = F,
                    wavenumber_fit = trace$data$intensity,
                    intensity_fit = trace$data$intensity,
                    derivative_decision = input$derivative_decision)
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
      if(is.null(preprocessed$data)){
          list(wavenumber = numeric(), spectra = data.table(empty = numeric()))
      }
      else{
          filter_spec(DataR(), logic = 1:ncol(DataR()$spectra) == data_click$data)
      }
  })

  libraryR <- reactive({
    req(input$active_identification)
    if(!input$derivative_decision) {
        library <- readRDS("data/both_nobaseline.rds")
    }
    else {
        library <- readRDS("data/both_derivative.rds")#Nest these in here so that they don't load automatically unless needed.
    }
    if(input$Spectra == "both") {
      library
    }
    else if (input$Spectra == "ftir"){
      filter_spec(library, logic = library$metadata$SpectrumType == "FTIR")
    }
    else if (input$Spectra == "raman"){
      filter_spec(library, logic = library$metadata$SpectrumType == "Raman")
    }
  })

  #Correlation ----
  output$correlation_head <- renderUI({
      boxLabel(text = if(input$active_identification) {"Cor"} else{"SNR"}, 
               status = if(input$active_identification) {
                   if(round(max_cor()[[data_click$data]], 2) > input$MinCor & round(signal_to_noise()[[data_click$data]], 2) > input$MinSNR){
                       "success"
                       } 
                   else{
                       "error"
                       }
                   } else{
                       if(round(signal_to_noise()[[data_click$data]], 2) > input$MinSNR){
                           "success"
                           } 
                       else{
                           "error"
                           }
                       }, 
               tooltip = "This tells you whether the signal to noise ratio or the match observed is above or below the thresholds.")
  })
  
  observeEvent(input$MinCor | max_cor(), {
      req(input$file)
      updateProgressBar(session = session, 
                        id = "correlation_progress", 
                        value = sum(max_cor() > input$MinCor)/length(max_cor()) * 100)
      updateProgressBar(session = session, 
                        id = "match_progress", 
                        value = (sum(signal_to_noise() > input$MinSNR & max_cor() > input$MinCor)/length(signal_to_noise())) * 100)
  })
  
  
  correlation <- reactive({
      req(input$file)
      req(input$active_identification)
      req(input$id_strategy == "correlation")
      withProgress(message = 'Analyzing Spectrum', value = 1/3, {
      correlate_spectra(object = DataR(), 
                        library = libraryR(),
                        add_library_metadata = "sample_name")
      })
  })

  signal_to_noise <- reactive({
          req(DataR)
          signal_noise(DataR())
  })

  ai_output <- reactive({ #tested working. 
      req(input$file)
      req(input$active_identification)
      req(input$id_strategy == "ai")
      ai_classify(data = DataR()$spectra, 
                  wavenumbers = DataR()$wavenumber, 
                  model = model)
  })
  
  top_correlation <- reactive({
      req(input$file)
      req(input$active_identification)
      correlation[order(-match_val), head(.SD, 1), by = object_id][data.table(object_id = as.character(names(DataR()$spectra))), on = "object_id"]
  })
  
  max_cor <- reactive({
      req(input$file)
      req(input$active_identification)
      if(input$id_strategy == "correlation"){
          top_correlation$match_value
      }
      else if(input$id_strategy == "ai"){
          round(ai_output()[["value"]], 1)
      }
  })

  max_cor_id <- reactive({
      req(input$file)
      #req(input$id_strategy == "correlation")
      req(input$active_identification)
      if(input$id_strategy == "correlation"){
          max_cor$library_id
      }
      else if(input$id_strategy == "ai"){
          ai_output()[["name"]]
      }
  })
  
  matches_to_single <- reactive({
      req(input$active_identification)
      req(input$id_strategy == "correlation")
      if(is.null(preprocessed$data)){
          libraryR()$metadata
      }
      else{
          correlation()[object_id == names(DataR_plot()$spectra)[data_click$data],]
      }
  })

  match_selected <- reactive({# Default to first row if not yet clicked
      #req(input$file)
      #req(input$active_identification)
      req(input$id_strategy == "correlation")
      if(!input$active_identification) {
          data.table(intensity = numeric(), wavenumber = numeric())
      }
      else{
         column_name <-  ifelse(is.null(preprocessed$data),
                                "sample_name",
                                "library_id")
          
         #need to make reactive
          id_select <-  ifelse(is.null(input$event_rows_selected),
                              matches_to_single()[[1,column_name]],
                              matches_to_single()[[input$event_rows_selected,column_name]])#"00087f78d45c571524fce483ef10752e"	#matches_to_single[[1,column_name]]
              
              
          
          # Get data from find_spec
         data.table(wavenumber = libraryR()$wavenumber,
                    intensity = make_rel(libraryR()[["spectra"]][[id_select]], na.rm = T)) %>%
             filter(!is.na(intensity))

      }
  })

  #All matches table for the current selection
  top_matches <- reactive({
      req(input$active_identification)
      req(input$id_strategy == "correlation")
      matches_to_single() #%>%
          #dplyr::rename("Material" = SpectrumIdentity) %>%
          #dplyr::rename("Pearson's r" = match_val) %>%
          #dplyr::select("Material",
          #              "polymer",
          #              "polymer_class", 
          #              "plastic_or_not", 
          #              if(!is.null(preprocessed$data)){"Pearson's r"}, 
          #              sample_name)
  })

  # Create the data tables for all matches
  output$event <- DT::renderDataTable({
    req(input$active_identification)
    req(input$id_strategy == "correlation")
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
    req(input$id_strategy == "correlation")
    matches_to_single()[input$event_rows_selected,] #%>%
        #select(#"SpectrumIdentity",
               #"polymer",
               #"polymer_class", 
               #"plastic_or_not",
         #      everything()) %>%
        #select(where(~!any(is_empty(.))))  #Causing errors, need to debug. 
})

#Metadata for the selected value
 output$eventmetadata <- DT::renderDataTable({
    req(input$active_identification)
    req(input$id_strategy == "correlation")
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
      req(input$id_strategy == "correlation")
     
      plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
          add_trace(data = match_selected(), x = ~wavenumber, y = ~intensity,
                    name = 'Library Spectra',
                    line = list(color = 'rgb(255,255,255)')) %>%
          add_trace(x = DataR_plot()$wavenumber, y = DataR_plot()[["spectra"]][[1]],
                    name = 'Your Spectra',
                    line = list(color = 'rgb(125,249,255)')) %>%
        # Dark blue rgb(63,96,130)
        # https://www.rapidtables.com/web/color/RGB_Color.html https://www.color-hex.com/color-names.html
        layout(yaxis = list(title = "absorbance intensity [-]"),
               xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                            autorange = "reversed"),
               plot_bgcolor = 'rgb(17,0,73)',
               paper_bgcolor = 'rgba(0,0,0,0.5)',
               legend = list(orientation = 'h', y = 1.1),
               font = list(color = '#FFFFFF')) %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
    })

  output$heatmap <- renderPlotly({
      #req(input$id_strategy == "correlation")
      req(input$file)
      #req(ncol(data()) > 2)
        plot_ly(source = "heat_plot") %>%
            add_trace(
                x = preprocessed$data$metadata$x, #Need to update this with the new rout format.
                y = preprocessed$data$metadata$y,
                z = if(input$active_identification){
                        ifelse(signal_to_noise() > input$MinSNR & max_cor() > input$MinCor, max_cor(), NA)
                    }
                    else{
                        ifelse(signal_to_noise() > input$MinSNR, signal_to_noise(), NA)
                    },
                type = "heatmap",
                hoverinfo = 'text',
                showscale = F,
                colors = if(input$active_identification){hcl.colors(n = sum(signal_to_noise() > input$MinSNR & max_cor() > input$MinCor), palette = "viridis")} else {heat.colors(n = sum(signal_to_noise() > input$MinSNR))
                },
                text = ~paste(
                    "x: ", preprocessed$data$metadata$x,
                    "<br>y: ", preprocessed$data$metadata$y,
                    "<br>snr: ", round(signal_to_noise(), 0),
                    "<br>cor: ", if(input$active_identification){round(max_cor(), 1)} else{NA},
                    "<br>identity: ", if(input$active_identification){max_cor_id()} else{NA},
                    "<br>filename: ", preprocessed$data$metadata$filename)) %>%
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
                   showlegend = FALSE,
                   #legend = list(showlegend = F),
                   font = list(color = '#FFFFFF')) %>%
            event_register("plotly_click")
  })

  # Data Download options ----
  output$download_data <- downloadHandler(
       filename = function() {if(input$download_selection == "Test Map") {paste0(input$download_selection, human_ts(), ".zip")} else{paste0(input$download_selection, human_ts(), ".csv")}},
        content = function(file) {
            if(input$download_selection == "Test Data") {fwrite(testdata, file)}
            if(input$download_selection == "Test Map") {zip(file, unzip("data/CA_tiny_map.zip"))}
            if(input$download_selection == "Your Spectra") {fwrite(data() %>% mutate(wavenumber = conform_res(preprocessed$data$wavenumber)), file)}
            if(input$download_selection == "Library Spectra") {fwrite(libraryR() %>% mutate(wavenumber = conform_res(preprocessed$data$wavenumber)), file)}
            if(input$download_selection == "Top Matches") {fwrite(data.table(x = preprocessed$data$metadata$x, y = preprocessed$data$metadata$y, filename = preprocessed$data$metadata$filename, signal_to_noise = signal_to_noise(), good_signal = signal_to_noise() > input$MinSNR), file)}
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
      #req(input$file)
      toggle(id = "download_conformed", condition = !is.null(preprocessed$data))
      toggle(id = "download_matched", condition = !is.null(preprocessed$data))
      toggle(id = "downloadData", condition = !is.null(preprocessed$data))
      toggle(id = "heatmap", condition = !is.null(preprocessed$data))
      if(!is.null(preprocessed$data)){
          toggle(id = "heatmap", condition = ncol(preprocessed$data$spectra) > 1)
      }
  })
  
  observe({
          if(isTruthy(preprocessed$data$spectra) && ncol(preprocessed$data$spectra) > 1){
              updateBox(
                  id = "placeholder2",
                  action = "restore",
                  options = NULL,
                  session = shiny::getDefaultReactiveDomain()
              ) 
          }
          if(!isTruthy(ncol(preprocessed$data$spectra) > 1)){
              updateBox(
                  id = "placeholder2",
                  action = "remove",
                  options = NULL,
                  session = shiny::getDefaultReactiveDomain()
              )
          }    
  })
  

  observe({
      toggle(id = "placeholder1", condition = is.null(preprocessed$data))
      toggle(id = "placeholder2", condition = ncol(preprocessed$data$spectra) > 1)
      toggle(id = "placeholder3", condition = is.null(preprocessed$data))
  })

  output$translate <- renderUI({
    if(translate & curl::has_internet()) {
      includeHTML("www/googletranslate.html")
    }
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
               spectra_type = input$Spectra,
               #analyze_type = input$Data,
               #region_type = input$Library,
               #ipid = input$ipid,
               time = human_ts())
      }
    }

  })

  #Storage ----
  #stores setup - insert at the bottom  !!!IMPORTANT
  appid = "application_OpenSpecy"
  setupStorage(appId = appid,inputs = TRUE)

}

