library(shiny)

server <- (function(input, output, session) {
  # server codes
  # useShinyjs(html = TRUE)
  
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