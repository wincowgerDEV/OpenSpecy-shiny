server <- function(input, output, session){
  
  output$distPlot <- renderPlot({
    #x <- faithful$waiting
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    os_test <- OpenSpecyWebr::as_OpenSpecy(data.frame(wavenumber = 1:4000, intensity = runif(4000)))
    
    plot(raman_hdpe)
  })
  
  
}
