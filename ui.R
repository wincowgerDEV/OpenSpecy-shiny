#' Shiny app server 
#'
#' @importFrom graphics hist
#' @import shiny
#'
# Libraries ----
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(bs4Dash)
library(dplyr)
library(plotly)
library(DT)
library(prompter)

# Name keys for human readable column names ----
load("data/namekey.RData")
options(shiny.maxRequestSize = 100*1024^2)

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

css <- HTML(
  "body {
    color: #fff;
  }
  .nav-tabs > li[class=active] > a,
  .nav-tabs > li[class=active] > a:focus,
  .nav-tabs > li[class=active] > a:hover
  {
    background-color: #000;
  }"
)

containerfunction <- function(...) {
  div(
    style = "padding:5rem",
    div(class = "jumbotron jumbotron-fluid",
        style = "border:solid #f7f7f9;background-color:rgba(0, 0, 0, 0.5)",
        align = "justify", ... ))
}

plotcontainerfunction <- function(...) {
  div(
    #style = "padding:0.1rem",
    div(class = "jumbotron jumbotron-fluid",
        style = "border:solid #f7f7f9;background-color:rgba(0, 0, 0, 0.5);padding:1rem",
        align = "justify",
        ...)
  )
}

columnformat <- function() {
  # 'background-color:rgba(0, 0, 0, 0.5);
  # padding-bottom: 2rem'
}

bodyformat <- function() {
  # 'background-color:rgba(0, 0, 0, 0.5);
  # padding-bottom: 2rem'
}

#linefunction <- function(...){
#  hr(style = "color:#f7f7f9", ...)
#}

# UI ----
ui <-  dashboardPage(dark = T,
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
                #sidebarHeader("Header 1"),
                menuItem(
                    "About",
                    tabName = "about",
                    icon = icon("sliders-h")
                ),
                menuItem(
                    "Analyze Spectra",
                    tabName = "analyze",
                    icon = icon("bar-chart")
                )
            )
        ),
        #Body ----
        dashboardBody(
            #Script for all pages ----
                # Required for any of the shinyjs functions.
            use_prompt(),
            shinyjs::useShinyjs(),
            #extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.clientValue-plotly_click-A', 'null'); }", functions = "resetClick"),
            #inputIp("ipid"),
            #inputUserid("fingerprint"),
            # tags$head(uiOutput("name_get")),singleton(tags$head()),
            #
            
            tags$head(tags$style(css),
                      tags$script(async = NA, src = "https://platform.twitter.com/widgets.js"),
                      tags$script(async = T, src = "https://buttons.github.io/buttons.js"),
                      tags$style(HTML("
                    .shiny-output-error-validation {
                    color: green; font-size: 300%;
                    }
                    ")),
                      #Ethical Ads
                      HTML('<script async src="https://media.ethicalads.io/media/client/ethicalads.min.js"></script>
                   
                   <!-- Show a text ad -->
                   <div class = "dark raised" data-ea-publisher="openanalysisorg" data-ea-type="image" data-ea-style="stickybox"></div>'),
                      
                      # <!-- Show an image ad -->
                      # <div data-ea-publisher="openanalysisorg" data-ea-type="image"></div>'), 
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
                               img(src = "https://mooreplasticresearch.org/wp-content/uploads/2021/06/HorizontalLogo-FullName-1.png", style = "padding:1rem; width:20vw"),
                               img(src = "https://www.helmholtz-hida.de/typo3conf/ext/hida_site_package/Resources/Public/dest/images/logos/hida-logo.svg", style = "padding:1rem; width:20vw"),
                               h4("Mcpike Zima Charitable Foundation")
                                        ),
                               div(class = "jumbotron",
                                   style = "padding:0rem 1rem 0rem;
                               border:solid #f7f7f9;
                               background-color:rgb(3, 252, 15, 0.5)",
                               h3("Maintaining (1,000–10,000$)"),
                               img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/aa/UC_Riverside_logo.svg/1024px-UC_Riverside_logo.svg.png", style = "padding:1rem; width:10vw"),
                               img(src = "https://upload.wikimedia.org/wikipedia/commons/7/7e/NSF_logo.png", style = "padding:1rem; width:10vw"),
                               img(src = "https://www.awi.de/typo3conf/ext/sms_boilerplate/Resources/Public/Images/AWI/awi_logo.svg", style = "padding:1rem; width:10vw"),
                               img(src = "https://www.hpu.edu/_global/images/header-logo.png", style = "padding:1rem; width:10vw"),
                               img(src = "https://www.nist.gov/libraries/nist-component-library/dist/img/logo/nist_logo_sidestack_rev.svg", style = "padding:1rem; width:10vw"),
                               img(src = "https://www.utoronto.ca/sites/all/themes/uoft_stark/img/U-of-T-logo.svg", style = "padding:1rem; width:10vw"),
                               img(src = "https://www.uni-koblenz-landau.de/logo.png", style = "padding:1rem; width:10vw"),
                               img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/50/Thermo_Fisher_Scientific_logo.svg/2560px-Thermo_Fisher_Scientific_logo.svg.png", style = "padding:1rem; width:10vw")
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
                       id = "accordion_download",
                       accordionItem(
                           title = "Download Open Data",
                           status = "info",
                           collapsed = TRUE,
                         p(class = "lead", "Reference spectra was sourced from open access resources
                                online, peer reviewed publications, and corporate donations. In the future,
                                spectra that is uploaded to the tool will be incorporated to the reference
                                library to make it even better."),
                         div(
                           downloadButton("downloadData6", "Raman Library", style = "background-color: #2a9fd6; width: 100%;"),
                           downloadButton("downloadData5", "FTIR Library", style = "background-color: #2a9fd6; width: 100%;"),
                           downloadButton("downloadData4", "Raman Metadata", style = "background-color: #2a9fd6; width: 100%;"),
                           downloadButton("downloadData3", "FTIR Metadata", style = "background-color: #2a9fd6; width: 100%;")
                         )
                        )
                       ),
                       
                   accordion(
                       id = "accordion_contribute",
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
                           column(3, style = columnformat(),
                                  tags$label("Choose .csv (preferred), .zip, .asp, .jdx, .spc, .spa, or .0 File"),
                                  
                                  fluidRow(
                                      column(12, 
                                             fileInput("file1", NULL,
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
                                             fluidRow(
                                                 column(12, 
                                                        downloadButton("download_testdata",
                                                                       "Single Sample",
                                                                       style = "background-color: rgb(0,0,0); color: rgb(255,255,255); float: left;") %>%
                                                            add_prompt(
                                                                message = "This is a sample spectrum that can be uploaded to the tool for testing it out and understanding how the csv files should be formatted.",
                                                                type = "info", 
                                                                size = "medium", rounded = TRUE
                                                            ),
                                                        downloadButton("download_metadata",
                                                                       "Metadata",
                                                                       style = "background-color: rgb(75,0,130); color: rgb(255,255,255); float: left;") %>%
                                                            add_prompt(
                                                                message = "Download metadata for all settings currently used.",
                                                                type = "info", 
                                                                size = "medium", rounded = TRUE
                                                            ),
                                                        downloadButton("download_conformed", "Conformed",
                                                                       style = "background-color: rgb(240,236,19); color: rgb(0,0,0); float: left;") %>%
                                                            add_prompt(
                                                                message = "Download the current spectra conformed to Open Specy's internal format used in the analysis you see.",
                                                                type = "info", 
                                                                size = "medium", rounded = TRUE
                                                            )
                                                 )
                                             ), 
                                             
                                             br(),
                                             
                                             box(title = "Spectral Selection",
                                                 id = "placeholder2", 
                                                 width = 12,
                                                 maximizable = T,
                                                 background = "fuchsia",
                                                 solidHeader = T,
                                                                       fluidRow(style = "padding:1rem",
                                                                                plotlyOutput("heatmap"),
                                                                                conditionalPanel("input.active_identification == true",
                                                                                                 DT::dataTableOutput("event"))
                                                                                
                                                                       )
                                                                ),
                                             HTML(' <!-- Show an image ad -->
                                                         <div class = "dark raised" data-ea-publisher="openanalysisorg" data-ea-type="image" data-ea-style="stickybox"></div>')
                                             
                                             
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
                                                                box(width = 12,
                                                                    collapsed = T,
                                                                    title = prettySwitch(inputId = "active_preprocessing",
                                                                                       label = "Preprocessing",
                                                                                       inline = T,
                                                                                       value = F,
                                                                                       status = "success",
                                                                                       fill = T),
                                                                box(
                                                                  title =  prettySwitch(inputId = "intensity_decision",
                                                                     label = "Intensity Adjustment",
                                                                     inline = T,
                                                                                                                           value = F,
                                                                                                                           status = "success",
                                                                                                                           fill = T),
                                                                                                          width = 12,
                                                                                                          collapsed = T,
                                                                                                              
                                                                                                radioButtons("intensity_corr", "Intensity Units",
                                                                                                             c("Absorbance" = "none", "Transmittance" = "transmittance", "Reflectance" = "reflectance")) %>%
                                                                                                    add_prompt(
                                                                                                        message = "If the uploaded spectrum is not in absorbance units, use this input to specify the units to convert from.The transmittance adjustment uses the log10(1/T) calculation which does not correct for system and particle characteristics. The reflectance adjustment uses the Kubelka-Munk equation (1-R)2/(2*R). We assume that the reflectance is formatted as a percent from 1-100 and first correct the intensity by dividing by 100 so that it fits the form expected by the equation. If none is selected, Open Specy assumes that the uploaded data is an absorbance spectrum.",
                                                                                                        type = "info", 
                                                                                                        size = "medium", rounded = TRUE
                                                                                                    )
                                                                                        )%>%
                                                                                        add_prompt(
                                                                                            message = "Open Specy assumes spectra are in Absorbance units, if they are not, you can select the appropriate transformation.",
                                                                                            type = "info", position = "left",
                                                                                            size = "medium", rounded = TRUE
                                                                                        ),
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
                                                                                     plotcontainerfunction(
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
                                                                                         )
                                                                                         
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
                                                                                     title = prettySwitch("co2_decision",
                                                                                                     label = "Flatten FTIR CO2",
                                                                                                     inline = T,
                                                                                                     value = F,
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
                                                                                            value = 1,
                                                                                            min = 0.5,
                                                                                            max = 12,
                                                                                            step = 0.1#,
                                                                                            #width = '25%'
                                                                                        ) %>%
                                                                                            add_prompt(
                                                                                                message = "Specify the signal to noise threshold to use.",
                                                                                                type = "info", 
                                                                                                size = "medium", rounded = TRUE
                                                                                            )
                                                                                 )
                                                                             ),
                                                            fluidRow(
                                                                column(12, 
                                                                       conditionalPanel("input.active_preprocessing == true",
                                                                                        downloadButton("downloadData", "Processed",
                                                                                                       style = "background-color: rgb(240,19,207); color: rgb(0,0,0); float: left;") %>%
                                                                                            add_prompt(
                                                                                                message = "Some users may wish to save a copy of their processed spectrum. This button downloads the processed spectrum as a csv file.",
                                                                                                type = "info", 
                                                                                                size = "medium", rounded = TRUE
                                                                                            ),
                                                                                        downloadButton("downloadsnr", "SNR Data",
                                                                                                       style = "background-color: rgb(0,0,0); color: rgb(255,255,255); float: left;") %>%
                                                                                            add_prompt(
                                                                                                message = "Some users may wish to save a copy of their signal to noise information. This button downloads the signal to noise data as a csv file.",
                                                                                                type = "info", 
                                                                                                size = "medium", rounded = TRUE
                                                                                            )
                                                                       )
                                                                       
                                                                )
                                                            )
                                                            
                                                        )
                                                     ),
                                                     column(6, 
                                                                            fluidRow(
                                                                                box(width = 12,
                                                                                    title = prettySwitch(inputId = "active_identification",
                                                                                                    label = "Identification",
                                                                                                    inline = T,
                                                                                                    value = F,
                                                                                                    status = "success",
                                                                                                    fill = T),
                                                                                column(3, 
                                                                                       prettyToggle("view_identification",
                                                                                                    icon_on = icon("eye"),
                                                                                                    icon_off = icon("eye-slash"), 
                                                                                                    label_on = NULL, label_off = NULL,
                                                                                                    status_on = "success",
                                                                                                    status_off = "default",
                                                                                                    outline = TRUE,
                                                                                                    plain = TRUE,
                                                                                                    bigger = T) %>%
                                                                                           add_prompt(
                                                                                               message = "View advanced identification options.",
                                                                                               type = "info", 
                                                                                               size = "medium", rounded = TRUE
                                                                                           )
                                                                                ),
                                                            conditionalPanel("input.view_identification == true",
                                                                             fluidRow(
                                                                                 column(3,
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
                                                                                 
                                                                                 column(3,
                                                                                        pickerInput(inputId = "id_level", label =  "Identity Level",
                                                                                                    choices = c("Raw" = "deep", 
                                                                                                                "Plastic Detailed" = "pp_optimal",
                                                                                                                "Plastic Grouped" = "pp_groups",
                                                                                                                "Plastic or Not" = "plastic_not")) %>%
                                                                                            add_prompt(
                                                                                                message = "Select how detailed or general you want the match description",
                                                                                                position = "right", type = "info", 
                                                                                                size = "medium", rounded = TRUE
                                                                                            )
                                                                                        
                                                                                 ),
                                                                                 column(3, 
                                                                                        numericInput(
                                                                                            "MinCor",
                                                                                            "Minimum Correlation",
                                                                                            value = 0.6,
                                                                                            min = 0.1,
                                                                                            max = 1,
                                                                                            step = 0.1#,
                                                                                            #width = '25%'
                                                                                        ) %>%
                                                                                            add_prompt(
                                                                                                message = "Specify the Correlation Threshold to Use",
                                                                                                type = "info", 
                                                                                                size = "medium", rounded = TRUE
                                                                                            ))
                                                                                 
                                                                             )  
                                                                             
                                                            ),
                                                            
                                                            conditionalPanel("input.active_identification == true",
                                                                             downloadButton("correlation_download", "Correlations",
                                                                                            style = "background-color: rgb(0,0,0); color: rgb(255,255,255); float: left;") %>%
                                                                                 add_prompt(
                                                                                     message = "Download the correlation matrix for all matches assessed by Open Specy.",
                                                                                     type = "info", 
                                                                                     size = "medium", rounded = TRUE
                                                                                 )), 
                                                            conditionalPanel("input.active_identification == true",
                                                                             downloadButton("topmatch_metadata_download", "Top Matches",
                                                                                            style = "background-color: rgb(0,0,0); color: rgb(255,255,255); float: left;") %>%
                                                                                 add_prompt(
                                                                                     message = "Download the metadata for the top spectral match.",
                                                                                     type = "info", 
                                                                                     size = "medium", rounded = TRUE
                                                                                 ))
                                                            
                                                            
                                                     )))))),
                                  fluidRow(
                                      column(12, 
                                             box(title = "Spectral Comparisons", 
                                                 maximizable = T,
                                                 width = 12,
                                                 h4(id = "placeholder1", "Upload some data to get started..."), 
                                                                   plotlyOutput("MyPlotC"),
                                                                   DT::dataTableOutput("eventmetadata")), 
                                             conditionalPanel("input.active_identification == true",
                                                              
                                                              downloadButton("download_matched", "Matched",
                                                                             style = "background-color: rgb(125,249,255); color: rgb(0,0,0); float: left;") %>%
                                                                  add_prompt(
                                                                      message = "Download the spectra you are trying to identify.",
                                                                      type = "info", 
                                                                      size = "medium", rounded = TRUE
                                                                  )), #Make colors align with the plot, Make only appear if on plot.
                                             conditionalPanel("input.active_identification == true",
                                                              downloadButton("download_selected", "Selected",
                                                                             style = "background-color: rgb(255,255,255); color: rgb(0,0,0); float: left;") %>%
                                                                  add_prompt(
                                                                      message = "Download the spectra for the match you selected in Open Specy.",
                                                                      type = "info", 
                                                                      size = "medium", rounded = TRUE
                                                                  )),
                                             actionButton("validate", "Validate Settings", style = "float: right;") %>%
                                                 add_prompt(
                                                     message = "Run 100 spectra from internal library through the current settings to validate the routine.",
                                                     type = "info", 
                                                     size = "medium", rounded = TRUE
                                                 ), 
                                             verbatimTextOutput("event_test"),
                                             style = bodyformat()
                                      )
                                      
                                      #Partner With Us tab ----
                                      #tabPanel("Partner With Us",
                                      #         titlePanel(h4("Help us reach our goal to revolutionize spectroscopy.")),
                                      #         br(),
                                      #         fluidRow(
                                      #             column(1),
                                      #             column(3,
                                      #                    plotcontainerfunction(
                                      #                        tags$h3("Donate Cash"),
                                      #                        icon = icon("shopping-cart"),
                                      #                        img(src = "https://p.turbosquid.com/ts-thumb/rX/Wm1eqB/t5/currencysymbolsgoldensetc4dmodel000/jpg/1613802168/300x300/sharp_fit_q85/a31625492ce9c8009ab3e4281ad752006e1163ec/currencysymbolsgoldensetc4dmodel000.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                      #                        actionButton(inputId = "ab1", label = "Donate", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                      #                                     icon = icon("donate"),
                                      #                                     onclick = "window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=wincowger@gmail.com&lc=US&item_name=Donation+to+Open+Specy&no_note=0&cn=&currency_code=USD&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted', '_blank')")
                                      #                    )),
                                      #             column(3,
                                      #                    plotcontainerfunction(tags$h3("Buy From Swag Store"),
                                      #                                          img(src = "https://image.spreadshirtmedia.com/image-server/v1/products/T813A823PA3132PT17X42Y46D1038541132FS4033/views/1,width=650,height=650,appearanceId=823/updated-logo-for-open-specy-designed-by-alex-mcgoran.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                      #                                          actionButton(inputId = "ab2", label = "Shop", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                      ##                                                       icon = icon("shopping-cart"),
                                      #                                                      onclick ="window.open('https://shop.spreadshirt.com/openspecy/all', '_blank')")
                                      #                   )),
                                      #            column(3,
                                      #                   plotcontainerfunction(
                                      #                       h2("Contribute time"),
                                      #                       #p(class = "lead", "We are looking for coders, moderators, spectroscopy experts, microplastic researchers, industry, government, and others to join the Open Specy team. Please contact Win at wincowger@gmail.com"),
                                      #                      img(src = "https://health.sunnybrook.ca/wp-content/uploads/2020/02/healthy-hands-810x424.jpg", style = "padding:1rem; background-color:rgba(255,255,255, 0.9)", width = "100%"),
                                      #                      actionButton(inputId = "ab3", label = "Guidelines", style="padding:4px; background-color: #2a9fd6; font-size:200%", width = "100%",
                                      #                                   icon = icon("clock"),
                                      #                                   onclick ="window.open('https://docs.google.com/document/d/1SaFgAYKsLbMSYdJClR5s42TyGmPRWihLQcf5zun_yfo/edit?usp=sharing', '_blank')")
                                      #                  )
                                      #           ),
                                      #           column(2)
                                      #       ),
                                      #       fluidRow(
                                      #           column(1),
                                      #           column(9,
                                      #                  div(style = "font-size:150%",
                                      #                      DT::dataTableOutput("event_goals"),
                                      #                      br()
                                      #                  ),
                                      #           ),
                                  )))))),
    
    #Footer ----
    footer = dashboardFooter(
        left = p(citation),
        right = HTML(paste0(uiOutput("translate"), 
                       a(href = "TOS.txt", "Terms And Conditions", class = "lead"),
                       br(),
                       a(href = "privacy_policy.txt", "Privacy Policy", class = "lead")
                       )
        )
    )
)


#Ideas
# see https://stackoverflow.com/questions/36412407/shiny-add-link-to-another-tabpanel-in-another-tabpanel/36426258
