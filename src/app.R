library(shiny)
library(tidyverse)
library(shinyjs)

server <- (function(input, output, session) {
  # server codes
  
  scorecard <- readRDS('./data.Rds')
  
  data_cleaned <- scorecard[,c('INSTNM', "STABBR", 'lat', 'lon', 'mn_earn_wne_p10',
                         'mn_earn_wne_p9', 'mn_earn_wne_p8',
                         'mn_earn_wne_p7', 'mn_earn_wne_p6', 'loan_ever',
                         'faminc',
                         'pct_white', 'pct_black', 'pct_asian',
                         'pct_hispanic')]
  
  data_cleaned <- data_cleaned %>%
    filter(!grepl("Inter American", INSTNM))
  
  state <- reactive(input$selectedState)
  college <- reactive(ifelse(is.null(input$selectedCollege),
                             ifelse(is.null(state()),
                                    "Yale University",
                                    data_cleaned$INSTNM[data_cleaned$STABBR == state()][1]),
                             input$selectedCollege))
  level <- reactive(ifelse(is.null(input$selectedState), '(national)', '(state)'))
  
  session$sendCustomMessage("getCities", data_cleaned)
  
  observeEvent(input$selectedState, {
    print(input$selectedState)
  })
  
  observeEvent(input$selectedCollege, {
    print(input$selectedCollege)
  })
  
  output$inputState <- renderPrint({
    input$state
  })
  
  get_year <- Vectorize(function(x) {
    if (x == 'mn_earn_wne_p6') {
      return(6)
    } else if (x == 'mn_earn_wne_p7') {
      return(7)
    } else if (x == 'mn_earn_wne_p8') {
      return(8)
    } else if (x == 'mn_earn_wne_p9') {
      return(9)
    } else if (x == 'mn_earn_wne_p10') {
      return(10)
    }
  })
  
  output$plot <- renderPlot({
    
    if (is.null(state())) {
      data_gathered <- data_cleaned
    } else {
      data_gathered <- data_cleaned %>%
        filter(STABBR == state())
    }
    
    data_gathered <- data_gathered %>%
      gather(`mn_earn_wne_p6`, `mn_earn_wne_p7`, `mn_earn_wne_p8`,
             `mn_earn_wne_p9`, `mn_earn_wne_p10`, key = "year", value = "earning") %>%
      mutate(year = get_year(year))
    
    # if (!is.null(selectedCollege())) {
    #   college2highlight <- selectedCollege()
    # } else {
    #   college2highlight <- "Yale University"
    # }
    college2highlight <- college()
    
    data_gathered %>%
      ggplot(aes(year, earning, color = INSTNM)) +
        geom_line(aes(group = INSTNM,
                      color = INSTNM == college2highlight,
                      alpha = INSTNM == college2highlight)) +
        scale_x_continuous("Year after entry") +
        scale_y_continuous(paste("Earnings", level()),
                           labels = scales::dollar_format()) +
        scale_color_manual("",
                           labels = c("Others",
                                      college2highlight),
                           values = c("grey", "red")) +
        scale_alpha_discrete("",
                             labels = c("Others", college2highlight),
                             range=c(0.2, 1)) +
        theme_bw() +
        theme(legend.position = 'none',
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(color = "#9e9e9e"))
  })
  
  output$plotCost <- renderPlot({
    # if (!is.null(input$selectedCollege)) {
    #   college2highlight <- input$selectedCollege
    # } else {
    #   college2highlight <- "Yale University"
    # }
    college2highlight <- college()
    
    if (is.null(state())) {
      data_gathered <- data_cleaned
    } else {
      data_gathered <- data_cleaned %>%
        filter(STABBR == state())
    }
    
    data_gathered %>%
      filter(loan_ever > 0.5) %>%
      ggplot() +
        geom_density(aes(loan_ever)) +
        geom_vline(xintercept = data_cleaned$loan_ever[data_cleaned$INSTNM == college2highlight],
                   color = 'red') +
      scale_y_continuous("") +
      scale_x_continuous("",
                         labels = scales::percent_format()) +
      scale_color_manual("",
                         labels = c("Others",
                                    college2highlight),
                         values = c("grey", "red")) +
      # scale_alpha_discrete("",
      #                      labels = c("Others", college2highlight),
      #                      range=c(0.2, 1)) +
      # scale_size_discrete("",
      #                     labels = c("Others", college2highlight),
      #                     range=c(1, 5)) +
      ggtitle(paste("Percentage of students who get loans", level())) +
      theme_bw() +
      theme(legend.position = 'none',
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.x = element_line(color = "black"))
  })
  
  output$plotIncome <- renderPlot({
    # state2highlight <- "Yale University"
    # if (!is.null(input$selectedCollege)) {
    #   state2highlight <- input$selectedCollege
    # }
    college2highlight <- college()
    if (is.null(state())) {
      data_gathered <- data_cleaned
    } else {
      data_gathered <- data_cleaned %>%
        filter(STABBR == state())
    }
    
    data_gathered %>%
      filter(loan_ever > 0.5) %>%
      ggplot() +
      geom_density(aes(faminc)) +
      geom_vline(xintercept = data_cleaned$faminc[data_cleaned$INSTNM == college2highlight],
                 color = 'red') +
      scale_y_continuous("") +
      scale_x_continuous("",
                         labels = scales::dollar_format()) +
      ggtitle(paste("Student's family income", level())) +
      theme_bw() +
      theme(legend.position = 'none',
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.x = element_line(color = "black"))
  })
  
  output$plotCulture <- renderPlot({
    college2highlight <- "Yale University"
    if (!is.null(college())) {
      college2highlight <- college()
    }
    
    idx <- data_cleaned$INSTNM == college2highlight
    y <- round(c(data_cleaned$pct_white[idx], data_cleaned$pct_black[idx],
                 data_cleaned$pct_asian[idx], data_cleaned$pct_hispanic[idx]))

    ggplot() +
      aes(x = reorder(c('White', 'Black', 'Asian', 'Hispanic'), y), y = y,
          fill = c('White', 'Black', 'Asian', 'Hispanic')) +
      geom_bar(width = 1, stat = 'identity') +
      scale_fill_brewer("", palette = 'Set2') +
      scale_y_continuous("") +
      scale_x_discrete("") +
      geom_text(aes(x = c('White', 'Black', 'Asian', 'Hispanic'),
                    y = y - y/2,
                    label = paste0(y, '% ', c('White', 'Black', 'Asian', 'Hispanic'))),
                hjust = -0.5,
                inherit.aes = TRUE) +
      coord_flip() +
      ggtitle("College diversity") +
      theme_bw() +
      theme(legend.position = 'none',
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.line.x = element_blank(),
            axis.line.y = element_blank())
  })
  
  output$title <- renderText({
    # selectedCollege <- "Yale University"
    # if (!is.null(input$selectedCollege)) {
    #   selectedCollege <- input$selectedCollege
    # }
    # selectedCollege
    paste('Stats for', college())
  })
  
})

shinyApp(ui = htmlTemplate("www/index.html",
                           title = textOutput("title"),
                           plot = plotOutput("plot", height = "250px"),
                           plotCost = plotOutput("plotCost", height = '100px'),
                           plotIncome = plotOutput("plotIncome", height = '100px'),
                           plotCulture = plotOutput("plotCulture", height = '100px')
                           ),
         server)