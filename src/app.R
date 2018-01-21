# library(shiny)

server <- (function(input, output, session) {
  # server codes
  # useShinyjs(html = TRUE)
  
  # data <- read_csv('../data/scorecard.csv')
  scorecard <- readRDS('../data/data.Rds')
  
  # coords <- tibble(lat = c(47.792927, 40.252928),
  #                  lon = c(-118.785114, -121.493451))
  coords <- scorecard[,c('INSTNM', 'lat', 'lon')]
  session$sendCustomMessage("getCities", coords)
  
  observeEvent(input$selectedState, {
    # message1 = "hello"
    # session$sendCustomMessage("handler1", message1)
    print(input$selectedState)
  })
  
  output$inputState <- renderPrint({
    input$state
  })
})

shinyApp(ui = htmlTemplate("www/index.html"), server)