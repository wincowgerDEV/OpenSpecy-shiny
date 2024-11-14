# UI ----

ui <- dashboardPage(
    dark = TRUE,
    help = TRUE,
    fullscreen = TRUE,
    dashboardHeader(
        title = tags$a(
            href = "https://www.openanalysis.org",
            target = "_blank",
            tags$img(src = "logo.avif", style = 'width: 15vw; padding:1rem;'),
            tags$head(HTML('<div class = "dark raised" data-ea-publisher="openanalysisorg" data-ea-type="text" data-ea-style="fixedfooter"></div>'))
        )
    ),
    #
    dashboardSidebar(
        sidebarUserPanel(name = "Welcome!"),
        sidebarMenu(
            id = "sidebarmenu",
            menuItem(
                "Analyze Spectra",
                tabName = "analyze",
                icon = icon("bar-chart")
            ),
            menuItem("About", tabName = "about", icon = icon("sliders-h")),
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
            tags$style(
                HTML(
                    "
                    .shiny-output-error-validation {
                    color: green; font-size: 300%;
                    }
                    "
                )
            ),
            HTML(
                '<script async src="https://media.ethicalads.io/media/client/ethicalads.min.js"></script>'
            ),
            tags$link(rel = "icon", type = "image/png", href = "favicon.avif")
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
                        collapsed = FALSE,
                        fluidRow(
                            column(
                                width = 6,
                                p(
                                    class = "lead",
                                    "Join the hundreds of
                               researchers from around the world who are part of
                               the Open Specy community by
                               analyzing, sharing, processing, and identifying
                               their Raman and IR spectra."
                                ),
                                p(
                                    class = "lead",
                                    shiny::HTML(
                                        "<span style='position: relative; top:.6ex;'Cross-Origin-Embedder-Policy: require-corp><a
                                      href='https://twitter.com/OpenSpecy?ref_src=twsrc%5Etfw'
                                      class='twitter-follow-button' data-size='large' data-dnt='true'
                                      data-show-count='false'>
                                      Follow @OpenSpecy</a></span>
                                      on Twitter"
                                    )
                                ),
                                p(
                                    class = "lead",
                                    HTML(
                                        "<span style='position: relative; top:.8ex;'><a
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
                                    or request a feature"
                                    )
                                ),
                                p(
                                    class = "lead",
                                    HTML(
                                        "Or just e-mail <a href='mailto:wincowger@gmail.com?subject=Open Specy mailing list'>
                                          wincowger@gmail.com</a>
                                          to be added to the Open Specy mailing list"
                                    )
                                ),
                                br(),
                                p(class = "lead", "Open Specy is free and open
                               source thanks to our partners."),
                                br(),
                                p(
                                    class = "lead",
                                    "Looking for the classic version of OpenSpecy? Go to wincowger.shinyapps.io/openspecy-classic"
                                )
                            ),
                            column(
                                6,
                                shiny::HTML(
                                    "<iframe width='100%' height='100%' src='https://www.youtube-nocookie.com/embed/3RKufDxzriE' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen require-corp credentialless (Chrome > 96)></iframe>"
                                )
                            )
                        )
                    )
                ),
                accordion(
                    id = "accordion_instructions",
                    accordionItem(
                        title = "Detailed Instructions",
                        status = "info",
                        collapsed = TRUE,
                        fluidRow(column(
                            6,
                            HTML(
                                "<iframe width='100%' height='50%' src='https://www.youtube-nocookie.com/embed/oWwRWwXf0sc' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen require-corp credentialless (Chrome > 96)></iframe>"
                            ),
                            HTML(
                                "<iframe width='100%' height='50%' src='https://www.youtube-nocookie.com/embed/cZZ3hgvIcao' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen require-corp credentialless (Chrome > 96)></iframe>"
                            )
                            
                        ), column(
                            6,
                            tags$ol(
                                tags$li(
                                    "Upload a .zip, .csv, .0, .asp, .jdx, .spc, or .spa file to the upload file tab."
                                ),
                                tags$li(
                                    "Process your data using smoothing, derivative, baseline correction, flattening, range selection, and intensity adjustment."
                                ),
                                tags$li(
                                    "Identify your spectra using onboard reference libraries and/or AI"
                                ),
                                tags$li("Download your results"),
                                tags$li(
                                    "For more details click the button below for the SOP or watch the detailed instructional videos."
                                )
                            ),
                            a(
                                "SOP",
                                onclick = "window.open('http://wincowger.com/OpenSpecy-package/articles/app.html', '_blank')",
                                class = "btn btn-primary btn-lg",
                                style = "width: 100%;"
                            )
                        ))
                    )
                ),
                accordion(
                    id = "accordion_links",
                    accordionItem(
                        title = "Useful Links",
                        status = "info",
                        collapsed = TRUE,
                        a(
                            href = "https://simple-plastics.eu/",
                            "Free FTIR Software: siMPle microplastic IR spectral identification software",
                            class = "lead"
                        ),
                        br(),
                        a(
                            href = "https://gitlab.ipfdd.de/GEPARD/gepard",
                            "Free Raman and FTIR Software: GEPARD (Gepard-Enabled PARticle Detection for Raman microscopes) Designed for particle-based microplastic analysis",
                            class = "lead"
                        ),
                        br(),
                        a(
                            href = "https://molview.org/",
                            "Free chemical modeling tool with built in spectral query, MolView.",
                            class = "lead"
                        ),
                        br(),
                        a(
                            href = "https://webbook.nist.gov/",
                            "Free spectroscopy and chemical database NIST Chemistry WebBook",
                            class = "lead"
                        ),
                        br(),
                        a(
                            href = "https://www.thermofisher.com/us/en/home/industrial/spectroscopy-elemental-isotope-analysis/spectroscopy-elemental-isotope-analysis-learning-center/molecular-spectroscopy-information.html",
                            "Free Spectroscopy Learning Academy from ThermoFisher",
                            class = "lead"
                        ),
                        br(),
                        a(
                            href = "https://micro.magnet.fsu.edu/primer/",
                            "Free Optical Microscopy Learning Resource from Florida State University",
                            class = "lead"
                        ),
                        br(),
                        a(
                            href = "https://www.effemm2.de/spectragryph/index.html",
                            "Free desktop application for spectral analysis and links to reference databases.",
                            class = "lead"
                        )
                    )
                )
                
            ),
            tabItem(
                "analyze",
                br(),
                fluidRow(
                    column(
                        2,
                        ##Upload/download ----
                        #tags$label("Upload File"),
                        fluidRow(style = "display: flex; align-items: flex-end;", column(
                            12,
                            fileInput(
                                "file",
                                NULL,
                                multiple = TRUE,
                                placeholder = ".csv, .zip, .asp, .jdx, .spc, .spa, .0",
                                accept = c(
                                    "text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv",
                                    ".asp",
                                    ".tsv",
                                    ".spc",
                                    ".jdx",
                                    ".dx",
                                    ".spa",
                                    ".0",
                                    ".zip",
                                    ".json",
                                    ".rds",
                                    ".yml",
                                    ".hdr",
                                    ".dat"
                                )
                            ) #%>%
                            # bs4Dash::popover(
                            #     title = "Upload Raman or FTIR spectrum files as a csv, tsv, dx, hdr, daTRUE, rds, json, yml, zip, asp, jdx, spc, 0, or spa. A csv file is preferred. If a csv, the file must contain one column labeled wavenumber in units of (1/cm) and another column labeled intensity in absorbance units. If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with wavenumber in (1/cm). If zip, batch upload using a zip file with multiple spectral files that all have the same wavenumbers or a map file formatted as .hdr and .dat. Hit the Download button to download a sample Raman spectrum.",
                            #     content = "File Upload",
                            #     placement = "right"
                            # )
                        ))
                    ),
                    
                    
                    column(8, fluidRow(
                        column(width = 6, fluidRow(
                            box(
                                width = 12,
                                collapsed = TRUE,
                                style = "height: 50vh; overflow-y: auto;",
                                footer = tags$small("Options for processing the spectra."),
                                title = prettySwitch(
                                    inputId = "active_preprocessing",
                                    label = "Preprocessing",
                                    inline = TRUE,
                                    value = TRUE,
                                    status = "success",
                                    fill = TRUE
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        footer = tags$small(
                                            "Signal thresholding technique, value, and histogram threshold plot."
                                        ),
                                        title = prettySwitch(
                                            inputId = "threshold_decision",
                                            label = "Threshold Signal-Noise",
                                            inline = TRUE,
                                            value = FALSE,
                                            status = "success",
                                            fill = TRUE
                                        ),
                                        collapsed = TRUE,
                                        numericInput(
                                            "MinSNR",
                                            "Minimum Value",
                                            value = 4,
                                            min = -10000,
                                            max = 10000,
                                            step = 1
                                        ),
                                        
                                        br(),
                                        selectInput(
                                            inputId = "signal_selection",
                                            label = "Signal Thresholding Technique",
                                            choices = c(
                                                "Signal Over Noise" = "run_sig_over_noise",
                                                "Signal Times Noise" = "sig_times_noise",
                                                "Total Signal" = "log_tot_sig"
                                            )
                                        ),
                                        br(),
                                        plotOutput("snr_plot", height = "10vh")
                                    )
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        footer = tags$small( 
                                            includeMarkdown("text/min_max_normalize.md")
                                        ),
                                        title = prettySwitch(
                                            "make_rel_decision",
                                            label = "Min-Max Normalize",
                                            inline = TRUE,
                                            value = TRUE,
                                            status = "success",
                                            fill = TRUE
                                        ),
                                        collapsed = TRUE
                                    )
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        collapsed = TRUE,
                                        footer = tags$small(includeMarkdown("text/smoothing_decision.md")
                                        ),
                                        title =  prettySwitch(
                                            inputId = "smooth_decision",
                                            label = "Smoothing/Derivative",
                                            inline = TRUE,
                                            value = TRUE,
                                            status = "success",
                                            fill = TRUE
                                        ),
                                        sliderInput(
                                            "smoother",
                                            "Polynomial",
                                            min = 0,
                                            max = 5,
                                            value = 3
                                        ),
                                        sliderInput(
                                            "derivative_order",
                                            "Derivative Order",
                                            min = 0,
                                            max = 3,
                                            value = 1
                                        ),
                                        sliderInput(
                                            "smoother_window",
                                            "Wavenumber Window",
                                            min = 50,
                                            max = 200,
                                            value = 90,
                                            step = 5
                                        ),
                                        prettySwitch(
                                            "derivative_abs",
                                            label = "Absolute Value",
                                            inline = TRUE,
                                            value = TRUE,
                                            status = "success",
                                            fill = TRUE
                                        )
                                    )
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        footer = tags$small(
                                            includeMarkdown("text/conform_wavenumbers.md")
                                        ),
                                        title = prettySwitch(
                                            "conform_decision",
                                            label = "Conform Wavenumbers",
                                            inline = TRUE,
                                            value = TRUE,
                                            status = "success",
                                            fill = TRUE
                                        ),
                                        
                                        collapsed = TRUE,
                                        selectInput(
                                            inputId = "conform_selection",
                                            label = "Conformation Technique",
                                            choices = c("Linear Interpolation" = "interp", "Nearest" = "roll")
                                        ),
                                        br(),
                                        sliderInput(
                                            "conform_res",
                                            "Wavenumber Resolution",
                                            min = 4,
                                            max = 16,
                                            value = 6
                                        )
                                        
                                    )
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        collapsed = TRUE,
                                        footer = tags$small(
                                            includeMarkdown("text/intensity_adjustment.md")
                                        ),
                                        title =  prettySwitch(
                                            inputId = "intensity_decision",
                                            label = "Intensity Adjustment",
                                            value = FALSE,
                                            inline = TRUE,
                                            status = "success",
                                            fill = TRUE
                                        ),
                                        radioButtons(
                                            "intensity_corr",
                                            "Intensity Units",
                                            c(
                                                "Absorbance" = "none",
                                                "Transmittance" = "transmittance",
                                                "Reflectance" = "reflectance"
                                            )
                                        )
                                    )
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        collapsed = TRUE,
                                        footer = tags$small(
                                            "This algorithm automatically fits to the baseline by fitting
                                                                                     polynomials of the provided order to the whole spectrum using the iModPolyFit algorithm."
                                        ),
                                        title = prettySwitch(
                                            "baseline_decision",
                                            label = "Baseline Correction",
                                            inline = TRUE,
                                            value = FALSE,
                                            status = "success",
                                            fill = TRUE
                                        ),
                                        sliderInput(
                                            "baseline",
                                            "Baseline Correction Polynomial",
                                            min = 1,
                                            max = 20,
                                            value = 8
                                        )
                                    )
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        collapsed = TRUE,
                                        footer = tags$small(
                                            "Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching.
                                                                                     These options control the maximum and minimum wavenumbers in the range to crop the spectra."
                                        ),
                                        title =  prettySwitch(
                                            "range_decision",
                                            label = "Range Selection",
                                            inline = TRUE,
                                            value = FALSE,
                                            status = "success",
                                            fill = TRUE
                                        ),
                                        numericInput(
                                            "MinRange",
                                            "Minimum Wavenumber",
                                            value = 300,
                                            min = NA,
                                            max = NA,
                                            step = NA,
                                            width = NULL
                                        ),
                                        numericInput(
                                            "MaxRange",
                                            "Maximum Wavenumber",
                                            value = 2000,
                                            min = NA,
                                            max = NA,
                                            step = NA,
                                            width = NULL
                                        )
                                    )
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        collapsed = TRUE,
                                        footer = tags$small(
                                            includeMarkdown("text/flatten_region.md")
                                        ),
                                        title = prettySwitch(
                                            "co2_decision",
                                            label = "Flatten Region",
                                            inline = TRUE,
                                            value = FALSE,
                                            status = "success",
                                            fill = TRUE
                                        ),
                                        numericInput(
                                            "MinFlat",
                                            "Minimum Wavenumber",
                                            value = 2200,
                                            min = 1,
                                            max = 6000,
                                            step = 1
                                        ),
                                        numericInput(
                                            "MaxFlat",
                                            "Maximum Wavenumber",
                                            value = 2400,
                                            min = 1,
                                            max = 6000,
                                            step = 1
                                        )
                                    )
                                )
                                
                            )
                        )), ## Identification ----
                        column(6, fluidRow(
                            box(
                                width = 12,
                                collapsed = TRUE,
                                footer = tags$small(
                                    includeMarkdown("text/identification.md")
                                ),
                                title = prettySwitch(
                                    inputId = "active_identification",
                                    label = "Identification",
                                    inline = TRUE,
                                    value = FALSE,
                                    status = "success",
                                    fill = TRUE
                                ),
                                pickerInput(
                                    inputId = "id_strategy",
                                    label =  "ID Library",
                                    choices =  c(
                                        #"Cor: Both Deriv" = "both_deriv",
                                        #"Cor: Both No Baseline" = "both_nobaseline",
                                        #"Cor: FTIR Deriv" = "ftir_deriv",
                                        #"Cor: Raman Deriv" = "raman_deriv",
                                        #"Cor: FTIR No Baseline" = "ftir_nobaseline",
                                        #"Cor: Raman No Baseline" = "raman_nobaseline",
                                        #"AI: FTIR Deriv Multinomial" = "ai",
                                        "AI: Both Deriv Mediod" = "mediod"
                                    )
                                ),
                                fluidRow(
                                    box(
                                        width = 12,
                                        collapsed = TRUE,
                                        title = prettySwitch(
                                            "cor_threshold_decision",
                                            label = "Threshold Correlation",
                                            inline = TRUE,
                                            value = TRUE,
                                            status = "success",
                                            fill = TRUE
                                        ),
                                        numericInput(
                                            "MinCor",
                                            "Minimum Value",
                                            value = 0.7,
                                            min = 0,
                                            max = 1,
                                            step = 0.1#,
                                            #width = '25%'
                                        ),
                                        plotOutput("cor_plot", height = "10vh")
                                        
                                    )
                                )
                            )
                        ))
                    )),
                    column(
                        2,
                        selectInput(
                            inputId = "download_selection",
                            label = downloadButton("download_data", style = "background-color: rgb(0,0,0); color: rgb(255,255,255);"),
                            choices = c(
                                "Test Data",
                                "Test Map",
                                "Your Spectra",
                                "Library Spectra",
                                "Top Matches",
                                "Thresholded Particles"
                            )
                        )# %>%
                        # popover(
                        #     title = "Options for downloading spectra and metadata from the analysis.
                        #               Test Data is a Raman HDPE spectrum in csv format. Test Map is an FTIR ENVI file of a CA particle.
                        #               Your Spectra will download your data with whatever processing options are active. Library Spectra
                        #               will download the current library selected. Top Matches downloads the top identifications in the
                        #               active analysis. Thresholded Particles will download a version of your spectra using the active
                        #               thresholds selected to infer where particles are in spectral maps, particle spectra are collapsed
                        #               to their medians and locations to their centroids.",
                        #     content = "Download Options",
                        #     placement = "left"
                        # )
                    )
                ),
                
                ## Plot ----
                fluidRow(
                    #verbatimTextOutput("event_test"),
                    box(
                        title = HTML(paste0("Spectra")),
                        maximizable = TRUE,
                        width = 12,
                        #background = "black",
                        label = uiOutput("correlation_head"),
                        h4(id = "placeholder1", "Upload some data to get started..."),
                        uiOutput("progress_bars"),
                        fluidRow(
                            tags$script(src = "https://cdn.plot.ly/plotly-2.11.1.min.js"),
                            tags$script(src = "https://cdn.datatables.net/1.11.3/js/jquery.dataTables.min.js"),
                            plotlyOutput("heatmap", inline = T),
                            plotlyOutput("MyPlotC", inline = T),
                            div(style = "overflow-x: scroll", DT::dataTableOutput("eventmetadata"))
                        ),
                        
                        sidebar = boxSidebar(
                            id = "mycardsidebar",
                            fluidRow(style = "padding:1rem; overflow-x: scroll", DT::dataTableOutput("event"))
                        )
                    )
                )
            ),
            
            tabItem(
                "partner",
                #Partner With Us tab ----
                titlePanel(h4(
                    "Help us reach our goal to revolutionize spectroscopy."
                )),
                br(),
                accordion(
                    id = "accordion_partners",
                    accordionItem(
                        title = "Partners",
                        status = "info",
                        collapsed = TRUE,
                        fluidRow(column(
                            6,
                            h3("Monetary Partners"),
                            panel(
                                style = "align: centre",
                                div(
                                    class = "jumbotron",
                                    style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(205, 127, 50, 0.5)",
                                    h3("Thriving (10,000–100,000$)"),
                                    img(src = "https://mooreplasticresearch.org/wp-content/uploads/2021/06/HorizontalLogo-FullName-1.png", style = "width:20vw"),
                                    img(src = "https://www.helmholtz-hida.de/typo3conf/ext/hida_site_package/Resources/Public/dest/images/logos/hida-logo.svg", style = "width:20vw"),
                                    img(src = "https://infrastructure.der-lab.net/wp-content/uploads/2017/05/logo_nrel_c.jpg", style = "width:20vw"),
                                    img(src = "https://mcpzfoundation.org/wp-content/uploads/2021/07/McPZ-Logo-Horizontal-RGB.png", style = "width:20vw")
                                ),
                                div(
                                    class = "jumbotron",
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
                                div(
                                    class = "jumbotron",
                                    style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(0, 0, 255, 0.5)",
                                    h3("Supporting (100–1,000$)"),
                                    h5("Jennifer Gadd")
                                ),
                                div(
                                    class = "jumbotron",
                                    style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(128, 0, 128, 0.5)",
                                    h3("Saving (<100$)"),
                                    h6(
                                        "Anne Jefferson, Heather Szafranski, Gwendolyn Lattin, Collin Weber, Gregory Gearhart, Anika Ballent, Shelly Moore, Susanne Brander (Oregon State University), Jeremy Conkle (TEXAS  A&M  UNIVERSITY  CORPUS  CHRISTI)"
                                    )
                                )
                            )
                        ), column(
                            6,
                            h3("In-Kind Partners"),
                            panel(
                                style = "align: centre",
                                div(
                                    class = "jumbotron",
                                    style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(205, 127, 50, 0.5)",
                                    h3("Thriving (10,000–100,000$)"),
                                    h4("Win Cowger, Zacharias Steinmetz")
                                ),
                                div(
                                    class = "jumbotron",
                                    style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(3, 252, 15, 0.5)",
                                    h3("Maintaining (1,000–10,000$)"),
                                    h5(
                                        "Garth Covernton, Jamie Leonard, Shelly Moore, Rachel Kozloski, Katherine Lasdin, Aleksandra Karapetrova, Laura Markley, Walter Yu, Walter Waldman, Vesna Teofilovic, Monica Arienzo, Mary Fey Long Norris, Cristiane Vidal, Scott Coffin, Charles Moore, Aline Carvalho, Shreyas Patankar, Andrea Faltynkova, Sebastian Primpke, Andrew Gray, Chelsea Rochman, Orestis Herodotu, Hannah De Frond, Keenan Munno, Hannah Hapich, Jennifer Lynch"
                                    )
                                ),
                                div(
                                    class = "jumbotron",
                                    style = "padding:0rem 1rem 0rem;
                                    border:solid #f7f7f9;
                                    background-color:rgb(0, 0, 255, 0.5)",
                                    h3("Supporting (100–1,000$)"),
                                    h6("Alexandre Dehaut, Gabriel Erni Cassola")
                                )
                            )
                        ))
                    ),
                    
                    accordionItem(
                        title = "Donate Cash",
                        status = "info",
                        collapsed = TRUE,
                        #img(src = "https://p.turbosquid.com/ts-thumb/rX/Wm1eqB/t5/currencysymbolsgoldensetc4dmodel000/jpg/1613802168/300x300/sharp_fit_q85/a31625492ce9c8009ab3e4281ad752006e1163ec/currencysymbolsgoldensetc4dmodel000.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                        actionButton(
                            inputId = "ab1",
                            label = "Donate",
                            style = "padding:4px; background-color: #2a9fd6; font-size:200%",
                            width = "100%",
                            icon = icon("donate"),
                            onclick = "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=wincowger@gmail.com&lc=US&item_name=Donation+to+Open+Specy&no_note=0&cn=&currency_code=USD&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted', '_blank')"
                        )
                    ),
                    accordionItem(
                        title = "Buy Merch",
                        status = "info",
                        collapsed = TRUE,
                        img(src = "https://image.spreadshirtmedia.com/image-server/v1/products/T813A823PA3132PT17X42Y46D1038541132FS4033/views/1,width=650,height=650,appearanceId=823/updated-logo-for-open-specy-designed-by-alex-mcgoran.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                        actionButton(
                            inputId = "ab2",
                            label = "Shop",
                            style = "padding:4px; background-color: #2a9fd6; font-size:200%",
                            width = "100%",
                            icon = icon("shopping-cart"),
                            onclick = "window.open('https://shop.spreadshirt.com/openspecy/all', '_blank')"
                        )
                    ),
                    accordionItem(
                        title = "Contribute Time",
                        status = "info",
                        collapsed = TRUE,
                        img(src = "https://health.sunnybrook.ca/wp-content/uploads/2020/02/healthy-hands-810x424.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                        actionButton(
                            inputId = "ab3",
                            label = "Guidelines",
                            style = "padding:4px; background-color: #2a9fd6; font-size:200%",
                            width = "100%",
                            icon = icon("clock"),
                            onclick = "window.open('https://docs.google.com/document/d/1SaFgAYKsLbMSYdJClR5s42TyGmPRWihLQcf5zun_yfo/edit?usp=sharing', '_blank')"
                        )
                    ),
                    
                    accordionItem(
                        title = "Contribute Spectra",
                        status = "info",
                        collapsed = TRUE,
                        p(
                            class = "lead",
                            includeMarkdown("text/contribute_spectra.md")
                        ),
                        div(
                            a(
                                "Community Data Warehouse",
                                onclick = "window.open('https://osf.io/rjg3c/', '_blank')",
                                class = "btn btn-primary btn-lg",
                                style = "width: 100%;"
                            )
                        )
                    )
                )
            ),
            
            tabItem("contract", div(
                h2(
                    "We are a group of experienced spectroscopists and can provide a variety of services for hire, please contact wincowger@gmail.com to inquire about any of the services below.",
                    style = "color: lightblue;"
                ),
                h3(
                    tags$ul(
                        tags$li("Adding new features to OpenSpecy"),
                        tags$li("Creating spectroscopy software"),
                        tags$li("Microplastic sample analysis"),
                        tags$li("Spectral identification"),
                        tags$li("Study design"),
                        tags$li("So much more...")
                    ),
                    style = "color: lightyellow;"
                ),
                style = "padding: 50px"
            ))
    ),
    tags$footer(
        citation,
        style = "
            background-color: #363e45;
            color: white;
            padding: 10px;
            "
    )
    )
)