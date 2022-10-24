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

# CSS for star
appCSS <-
  ".mandatory_star { color: red; }
    #loading_overlay {
      position: absolute;
      margin-top: 10%;
      background: #000000;
      opacity: 0.9;
      z-index: 100;
      left: 0;
      right: 0;
      height: 100%;
      text-align: center;
      color: #f7f7f9;
    }"

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
        dashboardHeader(title = "Open Specy"),
        #fluidRow(
        #    column(10, align = "left", img(src = "logo.png", width = 300, height = 75)),
        #    column(2, align = "right",
        #           div(style = "width: 90%;
        #                    padding: 15px;
        #                    font-size: 14pt;
        #                    border-radius: 0;
        #                    outline: none;
        #                    border: none;
        #                    text-align:left !important;",
        #               uiOutput("translate")
        #               # Google Translate
        #           )
        #    )
        #), windowTitle = "Open Specy"
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
                    icon = icon("magnifying-glass-chart")
                )
            )
        ),
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
            # Change this for other themes
            setBackgroundImage("jumbotron.png"),
            
            shinyjs::inlineCSS(appCSS),
            
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
                tabItem(
                    #Title Panel ----
                   tabName = "about",
  # About Tab ----
                       containerfunction(
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
                                          to be added to the Open Specy mailing list"),
                               ),
                               br(),
                               h3("Citation"),
                               p(class = "lead", citation),
                               br(),br(),
                               p(class = "lead", "Open Specy is free and open
                               source thanks to our partners:")),
                           column(3, img(src = "dancing.jpg", width = "100%")
                           )
                         ),
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
                                  ))
                         )
                       ),
                       containerfunction(
                         h2("Quick Video Tutorial"),
                         HTML("<iframe width='100%' height='100%' src='https://www.youtube-nocookie.com/embed/w55WGtV2Dz4' title='YouTube video player' frameborder='0' allow='accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture' allowfullscreen></iframe>")
                       ),
                       containerfunction(
                         h2("Instructions"),
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
                       ),
                       
                       containerfunction(
                         h2("Download Open Data"),
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
                       ),
                       
                       containerfunction(
                         h2("Contribute Spectra"),
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
                       ),
                       
                       containerfunction(
                         h2("Tool Validation"),
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
                       ),
                       
                       containerfunction(
                         h2("Useful Links"),
                         a(href = "https://simple-plastics.eu/", "Free FTIR Software: siMPle microplastic IR spectral identification software", class = "lead"),
                         p(),
                         a(href = "https://www.thermofisher.com/us/en/home/industrial/spectroscopy-elemental-isotope-analysis/spectroscopy-elemental-isotope-analysis-learning-center/molecular-spectroscopy-information.html", "Free Spectroscopy Learning Academy from ThermoFisher", class = "lead"),
                         p(),
                         a(href = "https://micro.magnet.fsu.edu/primer/", "Free Optical Microscopy Learning Resource from Florida State University", class = "lead"),
                         p(),
                         a(href = "https://www.effemm2.de/spectragryph/index.html", "Free desktop application for spectral analysis and links to reference databases.", class = "lead")
                       ),
                       
                       containerfunction(
                         h2("Terms And Conditions"),
                         pre(includeText("www/TOS.txt"))
                       ),
                       
                       containerfunction(
                         h2("Privacy Policy"),
                         pre(includeText("www/privacy_policy.txt"))
                       )
              )
        )
    )
)


#Ideas
# see https://stackoverflow.com/questions/36412407/shiny-add-link-to-another-tabpanel-in-another-tabpanel/36426258
