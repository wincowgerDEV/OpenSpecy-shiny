# UI ----

 ui <- dashboardPage(
     # dark = T, help = T, fullscreen = T, 
    dashboardHeader(
        title = tags$a(href="https://www.openanalysis.org", 
                       target="_blank",
                       tags$img(src = "logo.png", 
                                style = 'width: 15vw; padding:1rem;'))),
    # 
    dashboardSidebar(
        sidebarUserPanel(
            name = "Welcome"
        ),
        sidebarMenu(
            id = "sidebarmenu",
            menuItem(
                "Analyze Spectra",
                tabName = "analyze",
                icon = icon("bar-chart")
            ),
            menuItem(
                "About",
                tabName = "about",
                icon = icon("sliders-h")
            ),
            menuItem(
                "Partner With Us",
                tabName = "partner",
                icon = icon("hands-helping")
            ),
            menuItem(
                "Contract Us",
                tabName = "contract",
                icon = icon("file-contract")
            )
        )
    ),
    #Body ----
    dashboardBody(  
        #Script for all pages ----
        # Required for any of the shinyjs functions.
        #Use javascript directly
        shinyjs::useShinyjs(),
        
        tags$head(
            #tags$script(async = T, src = "https://buttons.github.io/buttons.js"),
            tags$style(HTML("
                    .shiny-output-error-validation {
                    color: green; font-size: 300%;
                    }
                    ")),
            HTML('<script async src="https://media.ethicalads.io/media/client/ethicalads.min.js"></script>'),
            tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
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
                             fluidRow(
                                 column(width = 6, 
                                        p(class = "lead", "Join the hundreds of
                               researchers from around the world who are part of
                               the Open Specy community by
                               analyzing, sharing, processing, and identifying
                               their Raman and IR spectra."),
                                        p(class = "lead",
                                          shiny::HTML("<span style='position: relative; top:.6ex;'Cross-Origin-Embedder-Policy: require-corp><a
                                      href='https://twitter.com/OpenSpecy?ref_src=twsrc%5Etfw'
                                      class='twitter-follow-button' data-size='large' data-dnt='true'
                                      data-show-count='false'>
                                      Follow @OpenSpecy</a></span>
                                      on Twitter")
                                          ),
                                        p(class = "lead",
                                          HTML("<span style='position: relative; top:.8ex;'><a
                                    class='github-button' href='https://github.com/wincowgerDEV/OpenSpecy'
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
                               source thanks to our partners."),
                                        br(),
                                        p(class = "lead", "Looking for the classic version of OpenSpecy? Go to wincowger.shinyapps.io/openspecy-classic")),
                                 column(6, shiny::HTML("<iframe width='100%' height='100%' src='https://www.youtube-nocookie.com/embed/3RKufDxzriE' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen require-corp credentialless (Chrome > 96)></iframe>")
                                 )
                             )
                         )
                     ),
                accordion(
                    id = "accordion_instructions", accordionItem(
                        title = "Detailed Instructions",
                        status = "info",
                        collapsed = TRUE,
                        fluidRow(
                            column(
                                6,
                                HTML(
                                    "<iframe width='100%' height='50%' src='https://www.youtube-nocookie.com/embed/oWwRWwXf0sc' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen require-corp credentialless (Chrome > 96)></iframe>"
                                ),
                                HTML(
                                    "<iframe width='100%' height='50%' src='https://www.youtube-nocookie.com/embed/cZZ3hgvIcao' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen require-corp credentialless (Chrome > 96)></iframe>"
                                )
                                   
                            ),
                            
                            column(6,
                                  tags$ol(
                                  tags$li("Upload a .zip, .csv, .0, .asp, .jdx, .spc, or .spa file to the upload file tab."),
                                  tags$li("Process your data using smoothing, derivative, baseline correction, flattening, range selection, and intensity adjustment."),
                                  tags$li("Identify your spectra using onboard reference libraries and/or AI"),
                                  tags$li("Download your results"),
                                  tags$li("For more details click the button below for the SOP or watch the detailed instructional videos.")
                                  ),
                                  a("SOP",
                                    onclick = "window.open('http://wincowger.com/OpenSpecy-package/articles/app.html', '_blank')",
                                    class="btn btn-primary btn-lg", 
                                    style = "width: 100%;")
                           )
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
                        a(href = "https://gitlab.ipfdd.de/GEPARD/gepard", "Free Raman and FTIR Software: GEPARD (Gepard-Enabled PARticle Detection for Raman microscopes) Designed for particle-based microplastic analysis", class = "lead"),
                        br(),
                        a(href = "https://molview.org/", "Free chemical modeling tool with built in spectral query, MolView.", class = "lead"),
                        br(),
                        a(href = "https://webbook.nist.gov/", "Free spectroscopy and chemical database NIST Chemistry WebBook", class = "lead"),
                        br(),
                        a(href = "https://www.thermofisher.com/us/en/home/industrial/spectroscopy-elemental-isotope-analysis/spectroscopy-elemental-isotope-analysis-learning-center/molecular-spectroscopy-information.html", "Free Spectroscopy Learning Academy from ThermoFisher", class = "lead"),
                        br(),
                        a(href = "https://micro.magnet.fsu.edu/primer/", "Free Optical Microscopy Learning Resource from Florida State University", class = "lead"),
                        br(),
                        a(href = "https://www.effemm2.de/spectragryph/index.html", "Free desktop application for spectral analysis and links to reference databases.", class = "lead")   
                    )                )
                
            ),
            tabItem("analyze",
                    br(),
            fluidRow(
                column(2,
                       ##Upload/download ----
                       #tags$label("Upload File"),
                       fluidRow(style = "display: flex; align-items: flex-end;",
                                column(12, 
                                       fileInput("file", NULL, multiple = T,
                                                 placeholder = ".csv, .zip, .asp, .jdx, .spc, .spa, .0",
                                                 accept=c("text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv", ".asp", ".tsv", ".spc", ".jdx", 
                                                          ".dx", ".spa", ".0", ".zip", 
                                                          ".json", ".rds", ".yml", ".hdr", ".dat")) %>%
                                           bs4Dash::popover(
                                               title = "Upload Raman or FTIR spectrum files as a csv, tsv, dx, hdr, dat, rds, json, yml, zip, asp, jdx, spc, 0, or spa. A csv file is preferred. If a csv, the file must contain one column labeled wavenumber in units of (1/cm) and another column labeled intensity in absorbance units. If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with wavenumber in (1/cm). If zip, batch upload using a zip file with multiple spectral files that all have the same wavenumbers or a map file formatted as .hdr and .dat. Hit the Download button to download a sample Raman spectrum.",
                                               content = "File Upload", placement = "right"
                                           )
                                )
                       )
                ),
                       

                 column(8, 
                        fluidRow(
                            column(width = 6,
                                   fluidRow(
                                       box(
                                            width = 12,
                                            collapsible = TRUE,
                                            collapsed = TRUE,
                                            style = "height: 50vh; overflow-y: auto;",
                                            footer = tags$small("Options for processing the spectra."),
                                             title = prettySwitch(
                                                 inputId = "active_preprocessing",
                                                 label = "Preprocessing",
                                                 inline = TRUE,
                                                 value = TRUE,
                                                 status = "success",
                                                 fill = TRUE),
                                            fluidRow(
                                                box(width = 12,
                                                    footer = tags$small("Signal thresholding technique, value, and histogram threshold plot."),
                                                    title = prettySwitch(
                                                        inputId = "threshold_decision",
                                                                         label = "Threshold Signal-Noise",
                                                                         inline = TRUE,
                                                                         value = FALSE,
                                                                         status = "success",
                                                                         fill = TRUE)
                                                    
                                                    ))))))))))))