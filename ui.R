
# UI ----
dashboardPage(dark = T, 
                     fullscreen = T,
        #Header ----
        dashboardHeader( 
            title = tags$a(href="https://www.openanalysis.org", 
                           target="_blank",
                        tags$img(src = "logo.png", 
                                 style = 'width: 15vw; padding:1rem;'))),
        #Sidebar ----
        dashboardSidebar(
            sidebarUserPanel(
                #image = "https://drive.google.com/file/d/13iCjC10dV3giFhCCoir_8mnbwtHM1rMA/view?usp=sharing",
                name = "Welcome!"
            ),
            sidebarMenu(
                id = "sidebarmenu",
                menuItem(
                    "Analyze Spectra",
                    tabName = "analyze",
                    icon = icon("bar-chart")
                ),
                #sidebarHeader("Header 1"),
                menuItem(
                    "About",
                    tabName = "about",
                    icon = icon("sliders-h")
                ),
                menuItem(
                    "Partner With Us",
                    tabName = "partner",
                    icon = icon("hands-helping")
                )
            )
        ),
        #Body ----
        dashboardBody(
            #Script for all pages ----
                # Required for any of the shinyjs functions.
            use_prompt(),
            shinyjs::useShinyjs(),
            #initialize stores
            initStore(),

            tags$head(
                      tags$script(async = T, src = "https://buttons.github.io/buttons.js"),
                      tags$style(HTML("
                    .shiny-output-error-validation {
                    color: green; font-size: 300%;
                    }
                    ")),
                     HTML('<script async src="https://media.ethicalads.io/media/client/ethicalads.min.js"></script>'),
                      tags$link(rel = "icon", type = "image/png", href = "favicon.png")
                      #This is for the error messages.
            ),
            tabItems(
                # About Tab ----
                tabItem(
                   tabName = "about",
                   accordion(
                       id = "accordion_welcome",
                       accordionItem(
                           title = "Welcome",
                           status = "info",
                           collapsed = F,
                         h2("Welcome"),
                         fluidRow(
                           column(9,
                                  p(class = "lead", "Join the hundreds of
                               researchers from around the world who are part of
                               the Open Specy community by
                               analyzing, sharing, processing, and identifying
                               their Raman and IR spectra."),
                               p(class = "lead",
                                 HTML("<span style='position: relative; top:.6ex;'><a
                                      href='https://twitter.com/OpenSpecy?ref_src=twsrc%5Etfw'
                                      class='twitter-follow-button' data-size='large' data-dnt='true'
                                      data-show-count='false'>
                                      Follow @OpenSpecy</a></span>
                                      on Twitter")
                               ),
                               p(class = "lead",
                                 HTML("<span style='position: relative; top:.8ex;'><a
                                    class='github-button' href='https://github.com/wincowgerDEV/OpenSpecy/subscription'
                                    data-color-scheme='no-preference: dark; light: dark; dark: dark;'
                                    data-size='large' aria-label='Watch wincowgerDEV/OpenSpecy'>Watch</a></span>
                                    us develop Open Specy on GitHub, file an
                                    <span style='position: relative; top:.8ex;'><a
                                    class='github-button'
                                    href='https://github.com/wincowgerDEV/OpenSpecy/issues'
                                    data-color-scheme='no-preference: dark; light: dark; dark: dark;'
                                    data-icon='octicon-issue-opened' data-size='large'
                                    aria-label='Issue wincowgerDEV/OpenSpecy on GitHub'>Issue</a></span>,
                                    or request a feature")
                               ),
                               p(class = "lead",
                                 HTML("Or just e-mail <a href='mailto:wincowger@gmail.com?subject=Open Specy mailing list'>
                                          wincowger@gmail.com</a>
                                          to be added to the Open Specy mailing list")
                               ),
                               br(),
                               p(class = "lead", "Open Specy is free and open
                               source thanks to our partners.")),
                           column(3, img(src = "dancing.jpg", width = "100%")
                                )
                            )
                         )
                       ),
                         
                         accordion(
                             id = "accordion_video",
                             accordionItem(
                                 title = "Quick Video Tutorial",
                                 status = "info",
                                 collapsed = F,
                                 HTML("<iframe width='100%' height='100%' src='https://www.youtube-nocookie.com/embed/w55WGtV2Dz4' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>")
                             )
                         ),
                   accordion(
                       id = "accordion_instructions",
                       accordionItem(
                           title = "Instructions",
                           status = "info",
                           collapsed = TRUE,
                         fluidRow(
                           column(6,
                                  HTML("<iframe width='100%' height='100%' src='https://www.youtube-nocookie.com/embed/JjhCdhjdcRY' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>")
                           ),
                           column(6,
                                  p(class = "lead", "In Brief: To use the tool upload a csv, asp, jdx, spc, or spa file to the upload file tab.
                                  If csv, one column should be named 'wavenumber' (in units of 1/cm) and another named 'intensity'.
                                  You can smooth your data using an SG filter, baseline correct your data using the polynomial order of iModPolyFit, and restrict the wavelength range for the match.
                                  The result will be compared to an internal Raman or FTIR spectra library. The strongest 1000 matches along with your
                                  uploaded or processed data will be presented in an interactive plot and table. For more details click the button below
                                  or watch the detailed instructional video."),
                                  a("SOP",
                                    onclick = "window.open('https://cran.r-project.org/web/packages/OpenSpecy/vignettes/sop.html', '_blank')",
                                    class="btn btn-primary btn-lg", 
                                    style = "width: 100%;")
                           )
                         )
                        )
                       ),
                   accordion(
                       id = "accordion_validation",
                       accordionItem(
                           title = "Tool Validation",
                           status = "info",
                           collapsed = TRUE,
                         p(class = "lead", "All parameters in this tool are tested to validate that
                                the tool is functioning as best as possible and determine the best default
                                parameters to use. Our current validation proceedure includes correcting
                                duplicated entries in the reference libraries, checking for spectra in
                                metadata that isn't in the spectral library, and ensuring the the default
                                parameters provide over 80% accuracy in the first match."
                         ),
                         div(
                           a("Detailed Validation Procedure",
                             onclick = "window.open('https://docs.google.com/document/d/1Zd2GY4bWIwegGeE4JpX8O0S5l_IYju0sLDl1ddTTMxU/edit?usp=sharing', '_blank')",
                             class="btn btn-primary btn-lg",
                             style = "width: 100%;")
                         )
                        )
                       ),
                       
                       accordion(
                           id = "accordion_links",
                           accordionItem(
                               title = "Useful Links",
                               status = "info",
                               collapsed = TRUE,
                               a(href = "https://simple-plastics.eu/", "Free FTIR Software: siMPle microplastic IR spectral identification software", class = "lead"),
                               br(),
                               a(href = "https://www.thermofisher.com/us/en/home/industrial/spectroscopy-elemental-isotope-analysis/spectroscopy-elemental-isotope-analysis-learning-center/molecular-spectroscopy-information.html", "Free Spectroscopy Learning Academy from ThermoFisher", class = "lead"),
                               br(),
                               a(href = "https://micro.magnet.fsu.edu/primer/", "Free Optical Microscopy Learning Resource from Florida State University", class = "lead"),
                               br(),
                               a(href = "https://www.effemm2.de/spectragryph/index.html", "Free desktop application for spectral analysis and links to reference databases.", class = "lead")   
                           )
                       )
              ),
              #Analyze Spectra Tab ----
              tabItem("analyze", 
                       br(),
                       fluidRow(
                           column(3,
                                  ##Upload/download ----
                                  tags$label("Choose .csv (preferred), .zip, .asp, .jdx, .spc, .spa, or .0 File"),
                                  
                                  fluidRow(
                                      column(12, 
                                             fileInput("file", NULL,
                                                       placeholder = ".csv, .zip, .asp, .jdx, .spc, .spa, .0",
                                                       accept=c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv", ".asp", ".spc", ".jdx", ".spa", ".0", ".zip")) %>%
                                                 add_prompt(
                                                     message = "Upload Raman or FTIR spectrum files as a csv, zip, asp, jdx, spc, 0, or spa. A csv file is preferred. If a csv, the file must contain one column labeled wavenumber in units of (1/cm) and another column labeled intensity in absorbance units. If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with wavenumber in (1/cm). If zip, batch upload using a zip file with multiple spectral files that all have the same wavenumbers or a map file formatted as .hdr and .dat. Hit the Sample button to download a sample Raman spectrum.",
                                                     type = "info", 
                                                     size = "medium", rounded = TRUE
                                                 ),
                                             prettySwitch("share_decision",
                                                          label = "Share Your Data?",
                                                          inline = T,
                                                          value = T,
                                                          status = "success",
                                                          fill = T) %>%
                                                 add_prompt(
                                                     message = "If you like, we share your uploaded spectra and settings with the spectroscopy community. By default, all data will be licensed under Creative Commons Attribution 4.0 International (CC BY 4.0). Uploaded spectra will appear here: https://osf.io/rjg3c. If you have spectra of known identities you can share, please upload a JDX file titled with the name of the material it is.",
                                                     type = "info", 
                                                     size = "medium", rounded = TRUE
                                                 ),
                                                       selectInput(inputId = "download_selection",
                                                                   label = downloadButton("download_data",
                                                                                  "",
                                                                                  style = "background-color: rgb(0,0,0); color: rgb(255,255,255);") %>%
                                                                       add_prompt(
                                                                           message = "This is a sample spectrum that can be uploaded to the tool for testing it out and understanding how the csv files should be formatted.",
                                                                           type = "info", 
                                                                           size = "large", rounded = TRUE
                                                                       ), 
                                                                   choices = c("Test Data",
                                                                               "Test Map",
                                                                               "Your Spectra",
                                                                               "Library Spectra",
                                                                               "Top Matches")) %>%
                                                           add_prompt(
                                                               message = "Options for downloading spectra and metadata from the analysis.",
                                                               type = "info", 
                                                               size = "medium", rounded = TRUE
                                                           ),
                                             br(),
                                             box(title = "Map Selection",
                                                 id = "placeholder2", 
                                                 width = 12,
                                                 maximizable = T,
                                                 plotlyOutput("heatmap",inline = T, height = "40%"),
                                                 shinyWidgets::progressBar(id = "signal_progress", value = 0, status = "success", title = "Good Signal", display_pct = TRUE),
                                                 conditionalPanel("input.active_identification == true",
                                                                  shinyWidgets::progressBar(id = "correlation_progress", value = 0, status = "success", title = "Good Correlations", display_pct = TRUE),
                                                                  shinyWidgets::progressBar(id = "match_progress", value = 0, status = "success", title = "Good Identifications", display_pct = TRUE)
                                                                 )
                                                    )
                                  )
                            )
                           ),
                           column(9,
                                  fluidRow(
                                             box(
                                                 title = "Analysis Parameters",
                                                 maximizable = T,
                                                 width = 12,
                                                 fluidRow( 
                                                     column(6, 
                                                            ## Preprocessing ----
                                                            fluidRow(
                                                                box(width = 12,
                                                                    collapsed = T,
                                                                    title = prettySwitch(inputId = "active_preprocessing",
                                                                                       label = "Preprocessing",
                                                                                       inline = T,
                                                                                       value = F,
                                                                                       status = "success",
                                                                                       fill = T),
                                                                fluidRow(
                                                                    box(
                                                                    width = 12,
                                                                    collapsed = T,
                                                                    title =  prettySwitch(inputId = "intensity_decision",
                                                                                        label = "Intensity Adjustment",
                                                                                        value = F,
                                                                                        inline = T,
                                                                                        status = "success",
                                                                                        fill = T),
                                                                                                radioButtons("intensity_corr", "Intensity Units",
                                                                                                             c("Absorbance" = "none", "Transmittance" = "transmittance", "Reflectance" = "reflectance")) %>%
                                                                                                    add_prompt(
                                                                                                        message = "If the uploaded spectrum is not in absorbance units, use this input to specify the units to convert from.The transmittance adjustment uses the log10(1/T) calculation which does not correct for system and particle characteristics. The reflectance adjustment uses the Kubelka-Munk equation (1-R)2/(2*R). We assume that the reflectance is formatted as a percent from 1-100 and first correct the intensity by dividing by 100 so that it fits the form expected by the equation. If none is selected, Open Specy assumes that the uploaded data is an absorbance spectrum.",
                                                                                                        type = "info", 
                                                                                                        size = "medium", rounded = TRUE
                                                                                                    )
                                                                                        ) %>%
                                                                                        add_prompt(
                                                                                            message = "Open Specy assumes spectra are in Absorbance units, if they are not, you can select the appropriate transformation.",
                                                                                            type = "info", position = "left",
                                                                                            size = "medium", rounded = TRUE
                                                                                        )),
                                                                             fluidRow(
                                                                                 box(width = 12,
                                                                                     collapsed = T,
                                                                                     title =  prettySwitch(inputId = "smooth_decision",
                                                                                                     label = "Smoothing",
                                                                                                     inline = T,
                                                                                                     value = F,
                                                                                                     status = "success",
                                                                                                     fill = T),
                                                                                     sliderInput("smoother", "Smoothing Polynomial", min = 0, max = 7, value = 3) %>%
                                                                                                               add_prompt(
                                                                                                                   message = "Smoothing uses the SG filter on an 11 data point window with the polynomial order specified.",
                                                                                                                   type = "info", 
                                                                                                                   size = "medium", rounded = TRUE
                                                                                                               )
                                                                                        ) %>%
                                                                                 add_prompt(
                                                                                     message = "This smoother can enhance the signal to noise ratio of the data and uses a Savitzky-Golay filter with 12 running data points and the polynomial specified.",
                                                                                     position = "left", type = "info", 
                                                                                     size = "medium", rounded = TRUE
                                                                                 )),
                                                                             fluidRow(
                                                                                 box(width = 12,
                                                                                     collapsed = T,
                                                                                     title = prettySwitch("baseline_decision",
                                                                                                     label = "Baseline Correction",
                                                                                                     inline = T,
                                                                                                     value = F,
                                                                                                     status = "success",
                                                                                                     fill = T),
                                                                                                          selectInput(inputId = "baseline_selection", label = "Baseline Correction Technique", choices = c("Polynomial", "Manual")) %>%
                                                                                                              add_prompt(
                                                                                                                  message = "Baseline correction techniques can be manually drawn on the spectra or automated using one of the other options.",
                                                                                                                  type = "info", 
                                                                                                                  size = "medium", rounded = TRUE
                                                                                                              ),
                                                                                     sliderInput("baseline", "Baseline Correction Polynomial", min = 1, max = 20, value = 8) %>%
                                                                                         add_prompt(
                                                                                             message = "This algorithm automatically fits to the baseline by fitting polynomials of the provided order to the whole spectrum.",
                                                                                             type = "info", 
                                                                                             size = "medium", rounded = TRUE
                                                                                         ),
                                                                                     fluidRow(
                                                                                         column(6,
                                                                                                actionButton("go", "Correct With Trace") %>%
                                                                                                    add_prompt(
                                                                                                        message = "After tracing the baseline spectrum on the plot, select this to correct the spectra.",
                                                                                                        type = "info", 
                                                                                                        size = "medium", rounded = TRUE
                                                                                                    )
                                                                                         ),
                                                                                         column(6,
                                                                                                actionButton("reset", "Reset") %>%
                                                                                                    add_prompt(
                                                                                                        message = "Reset the manual baseline to zero baseline.",
                                                                                                        type = "info", 
                                                                                                        size = "medium", rounded = TRUE
                                                                                                    )
                                                                                         )
                                                                                     )
                                                                                        
                                                                                 ) %>%
                                                                                     add_prompt(
                                                                                         message = "This baseline correction routine has two options for baseline correction, 1) the polynomial imodpolyfit procedure to itteratively find the baseline of the spectrum using a polynomial fit to the entire region of the spectra. 2) manual lines can be drawn using the line tool on the plot and the correct button will use the lines to subtract the baseline.",
                                                                                         position = "left", type = "info", 
                                                                                         size = "medium", rounded = TRUE
                                                                                     )
                                                                             ),
                                                                             fluidRow(
                                                                                 box(width = 12,
                                                                                     collapsed = T,
                                                                                     title =  prettySwitch("range_decision",
                                                                                                     label = "Range Selection",
                                                                                                     inline = T,
                                                                                                     value = F,
                                                                                                     status = "success",
                                                                                                     fill = T),
                                                                                         numericInput(
                                                                                             "MaxRange",
                                                                                             "Maximum Spectral Range",
                                                                                             value = 6000,
                                                                                             min = NA,
                                                                                             max = NA,
                                                                                             step = NA,
                                                                                             width = NULL
                                                                                         ),
                                                                                         numericInput(
                                                                                             "MinRange",
                                                                                             "Minimum Spectral Range",
                                                                                             value = 0,
                                                                                             min = NA,
                                                                                             max = NA,
                                                                                             step = NA,
                                                                                             width = NULL
                                                                                         ) %>%
                                                                                         add_prompt(
                                                                                             message = "Maximum and minimum wavenumbers to focus on.",
                                                                                             type = "info", 
                                                                                             size = "medium", rounded = TRUE
                                                                                         )
                                                                                     
                                                                                 )%>%
                                                                                     add_prompt(
                                                                                         message = "Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching.",
                                                                                         position = "left", type = "info", 
                                                                                         size = "medium", rounded = TRUE
                                                                                     )
                                                                                 ),
                                                                             fluidRow(
                                                                                 box(width = 12,
                                                                                     collapsed = T,
                                                                                        title = prettySwitch("derivative_decision",
                                                                                                     label = "Derivative",
                                                                                                     inline = T,
                                                                                                     value = T,
                                                                                                     status = "success",
                                                                                                     fill = T) 
                                                                                 )%>%
                                                                                     add_prompt(
                                                                                         message = "Derivative transformation decreases baseline and can amplify peak contrast.",
                                                                                         type = "info", position = "left",
                                                                                         size = "medium", rounded = TRUE
                                                                                     )),
                                                                             fluidRow(
                                                                                 box(width = 12,
                                                                                     collapsed = T,
                                                                                     title = prettySwitch("co2_decision",
                                                                                                     label = "Flatten FTIR CO2",
                                                                                                     inline = T,
                                                                                                     value = T,
                                                                                                     status = "success",
                                                                                                     fill = T)
                                                                                         
                                                                                 ) %>% add_prompt(
                                                                                         message = "Replace the wavenumbers from 2420 - 2200 with the mean of intensities at 2420 and 2200.",
                                                                                         type = "info", 
                                                                                         size = "medium", rounded = TRUE
                                                                                     )), 
                                                                             fluidRow(
                                                                                 box(width = 12,
                                                                                     title = "Threshold Signal to Noise",
                                                                                     collapsed = T,
                                                                                        numericInput(
                                                                                            "MinSNR",
                                                                                            "Minimum Signal to Noise",
                                                                                            value = 10,
                                                                                            min = 2,
                                                                                            max = 100,
                                                                                            step = 1#,
                                                                                            #width = '25%'
                                                                                        ) %>%
                                                                                            add_prompt(
                                                                                                message = "Specify the signal to noise threshold to use.",
                                                                                                type = "info", 
                                                                                                size = "medium", rounded = TRUE
                                                                                            )
                                                                                 )
                                                                             )
                                                            )
                                                        )
                                                     ),
                                                     ## Identification ----
                                                     column(6, 
                                                                            fluidRow(
                                                                                box(width = 12,
                                                                                    collapsed = T,
                                                                                    title = prettySwitch(inputId = "active_identification",
                                                                                                    label = "Identification",
                                                                                                    inline = T,
                                                                                                    value = F,
                                                                                                    status = "success",
                                                                                                    fill = T),
                                                                                                                                                            fluidRow(
                                                                                 column(4,
                                                                                        pickerInput(inputId = "Spectra", label =  "Library Type",
                                                                                                    choices =  c("Both" = "both",
                                                                                                                 "Raman" = "raman",
                                                                                                                 "FTIR" = "ftir")) %>%
                                                                                            add_prompt(
                                                                                                message = "This selection will determine whether both libraries, FTIR only, or Raman only matching library is used. Choose the spectrum type that was uploaded.",
                                                                                                position = "left", type = "info", 
                                                                                                size = "medium", rounded = TRUE
                                                                                            )
                                                                                 ),
                                                                                 column(4,
                                                                                        pickerInput(inputId = "id_strategy", label =  "ID Strategy",
                                                                                                    choices =  c("Correlation" = "correlation",
                                                                                                                 "AI (Multinomial)" = "ai")) %>%
                                                                                            add_prompt(
                                                                                                message = "This selection will choose the strategy for identification.",
                                                                                                position = "left", type = "info", 
                                                                                                size = "medium", rounded = TRUE
                                                                                            )
                                                                                 ),
                                                                                 column(4, 
                                                                                        numericInput(
                                                                                            "MinCor",
                                                                                            "Minimum Value",
                                                                                            value = 0.7,
                                                                                            min = 0.6,
                                                                                            max = 1,
                                                                                            step = 0.1#,
                                                                                            #width = '25%'
                                                                                        ) %>%
                                                                                            add_prompt(
                                                                                                message = "Specify the Correlation or AI Value Threshold to Use",
                                                                                                type = "info", 
                                                                                                size = "medium", rounded = TRUE
                                                                                            )
                                                                                        
                                                                                        )
                                                                                 
                                                                                 
                                                                             )  
                                                                             
                                                            )
                                                     ))))),
                                  ## Plot ----
                                  fluidRow(
                                             box(title = HTML(paste0("Spectral Comparisons")), 
                                                 maximizable = T,
                                                 width = 12,
                                                 label = uiOutput("correlation_head"),
                                                 h4(id = "placeholder1", "Upload some data to get started..."),
                                            
                                                 
                                                 fluidRow(
                                                            plotlyOutput("MyPlotC", inline = T, height = "40%"),
                                                                div(style = "overflow-x: scroll",
                                                                    DT::dataTableOutput("eventmetadata")   
                                                                )),
                                                 sidebar = boxSidebar(
                                                     id = "mycardsidebar",
                                                     fluidRow(style = "padding:1rem; overflow-x: scroll",
                                                              DT::dataTableOutput("event"))
                                                    )
                                                 )
                                             
                                  ),
                                  verbatimTextOutput("event_test")
                                  ))),
              tabItem("partner", 
                      #Partner With Us tab ----
                               titlePanel(h4("Help us reach our goal to revolutionize spectroscopy.")),
                               br(),
                      accordion(
                          id = "accordion_partners",
                          accordionItem(
                              title = "Partners",
                              status = "info",
                              collapsed = T,
                              fluidRow(
                                  column(6,
                                         h3("Monetary Partners"),
                                         panel(style = "align: centre",
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(205, 127, 50, 0.5)",
                                                   h3("Thriving (10,000–100,000$)"),
                                                   img(src = "https://mooreplasticresearch.org/wp-content/uploads/2021/06/HorizontalLogo-FullName-1.png", style = "width:20vw"),
                                                   img(src = "https://www.helmholtz-hida.de/typo3conf/ext/hida_site_package/Resources/Public/dest/images/logos/hida-logo.svg", style = "width:20vw"),
                                                   img(src = "https://infrastructure.der-lab.net/wp-content/uploads/2017/05/logo_nrel_c.jpg", style = "width:20vw"),
                                                   img(src = "https://mcpzfoundation.org/wp-content/uploads/2021/07/McPZ-Logo-Horizontal-RGB.png", style = "width:20vw")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(3, 252, 15, 0.5)",
                                                   h3("Maintaining (1,000–10,000$)"),
                                                   img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/UC_Riverside_logo.svg/1024px-UC_Riverside_logo.svg.png", style = "width:10vw"),
                                                   img(src = "https://upload.wikimedia.org/wikipedia/commons/7/7e/NSF_logo.png", style = "width:10vw"),
                                                   img(src = "https://www.awi.de/typo3conf/ext/sms_boilerplate/Resources/Public/Images/AWI/awi_logo.svg", style = "width:10vw"),
                                                   img(src = "https://www.hpu.edu/_global/images/header-logo.png", style = "width:10vw"),
                                                   img(src = "https://www.nist.gov/libraries/nist-component-library/dist/img/logo/nist_logo_sidestack_rev.svg", style = "width:10vw"),
                                                   img(src = "https://www.utoronto.ca/sites/all/themes/uoft_stark/img/U-of-T-logo.svg", style = "width:10vw"),
                                                   img(src = "https://www.uni-koblenz-landau.de/logo.png", style = "width:10vw"),
                                                   img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Thermo_Fisher_Scientific_logo.svg/2560px-Thermo_Fisher_Scientific_logo.svg.png", style = "width:10vw")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(0, 0, 255, 0.5)",
                                                   h3("Supporting (100–1,000$)"),
                                                   h5( "Jennifer Gadd")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(128, 0, 128, 0.5)",
                                                   h3("Saving (<100$)"),
                                                   h6("Susanne Brander (Oregon State University), Jeremy Conkle (TEXAS  A&M  UNIVERSITY  CORPUS  CHRISTI)")
                                               )
                                         )
                                  ),
                                  column(6,
                                         h3("In-Kind Partners"),
                                         panel(style = "align: centre",
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(205, 127, 50, 0.5)",
                                                   h3("Thriving (10,000–100,000$)"),
                                                   h4("Win Cowger, Zacharias Steinmetz")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(3, 252, 15, 0.5)",
                                                   h3("Maintaining (1,000–10,000$)"),
                                                   h5("Sebastian Primpke, Andrew Gray, Chelsea Rochman, Orestis Herodotu, Hannah De Frond, Keenan Munno, Hannah Hapich, Jennifer Lynch")
                                               ),
                                               div(class = "jumbotron",
                                                   style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(0, 0, 255, 0.5)",
                                                   h3("Supporting (100–1,000$)"),
                                                   h6( "Shreyas Patankar, Andrea Faltynkova, Alexandre Dehaut, Gabriel Erni Cassola, Aline Carvalho")
                                               )
                                         )
                                  )
                              )
                          ),
                          
                          accordionItem(
                              title = "Donate Cash",
                              status = "info",
                              collapsed = TRUE,
                              #img(src = "https://p.turbosquid.com/ts-thumb/rX/Wm1eqB/t5/currencysymbolsgoldensetc4dmodel000/jpg/1613802168/300x300/sharp_fit_q85/a31625492ce9c8009ab3e4281ad752006e1163ec/currencysymbolsgoldensetc4dmodel000.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                              actionButton(inputId = "ab1", label = "Donate", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                           icon = icon("donate"),
                                           onclick = "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=wincowger@gmail.com&lc=US&item_name=Donation+to+Open+Specy&no_note=0&cn=&currency_code=USD&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted', '_blank')")
                              ),
                          
                          accordionItem(
                              title = "Buy Merch",
                              status = "info",
                              collapsed = TRUE,
                              img(src = "https://image.spreadshirtmedia.com/image-server/v1/products/T813A823PA3132PT17X42Y46D1038541132FS4033/views/1,width=650,height=650,appearanceId=823/updated-logo-for-open-specy-designed-by-alex-mcgoran.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                              actionButton(inputId = "ab2", label = "Shop", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                           icon = icon("shopping-cart"),
                                           onclick ="window.open('https://shop.spreadshirt.com/openspecy/all', '_blank')")
                          ),
                          accordionItem(
                              title = "Contribute Time",
                              status = "info",
                              collapsed = T,
                                  img(src = "https://health.sunnybrook.ca/wp-content/uploads/2020/02/healthy-hands-810x424.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                        actionButton(inputId = "ab3", label = "Guidelines", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                                     icon = icon("clock"),
                                                     onclick ="window.open('https://docs.google.com/document/d/1SaFgAYKsLbMSYdJClR5s42TyGmPRWihLQcf5zun_yfo/edit?usp=sharing', '_blank')")
                          ),
                          
                              accordionItem(
                                  title = "Contribute Spectra",
                                  status = "info",
                                  collapsed = TRUE,
                                  p(class = "lead", "To share spectra upload a file to the upload file tab.
                             If you selected Share a copy of your spectra will be sent to the Community
                             Data Warehouse on Open Science Framework. To add additional metadata,
                             fill in the avaliable metadata fields and click -Share Data-. The
                             spectra file that you uploaded along with your responses will be copied
                             to the a -With Metadata- subfolder at the link below. All shared data holds
                             a Creative Commons Attribution License 4.0."),
                                  div(
                                      a("Community Data Warehouse",
                                        onclick = "window.open('https://osf.io/rjg3c/', '_blank')",
                                        class="btn btn-primary btn-lg",
                                        style = "width: 100%;")
                                  )
                              )
                      )
                )
              )
            ),
    
    #Footer ----
    footer = dashboardFooter(
        left = p(citation),
        right = HTML(paste0(uiOutput("translate"), 
                       a(href = "TOS.txt", "Terms And Conditions", class = "lead"),
                       br(),
                       a(href = "privacy_policy.txt", "Privacy Policy", class = "lead")
                       ),
                     #Ethical Ads
                     if(conf$share != "system"){HTML('<div class = "dark raised" data-ea-publisher="openanalysisorg" data-ea-type="image" data-ea-style="stickybox"></div>')}
        )
    )
)
