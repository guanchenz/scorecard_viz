library(shiny)
library(tidyverse)
library(shinyjs)

server <- (function(input, output, session) {
  # server codes
  
  # get data
  scorecard <- readRDS('./data.Rds')
  
  data_cleaned <- scorecard[,c('INSTNM', "STABBR", 'lat', 'lon', 'mn_earn_wne_p10',
                         'mn_earn_wne_p9', 'mn_earn_wne_p8',
                         'mn_earn_wne_p7', 'mn_earn_wne_p6', 'loan_ever',
                         'faminc',
                         'pct_white', 'pct_black', 'pct_asian',
                         'pct_hispanic')]
  
  data_cleaned <- data_cleaned %>%
    filter(!grepl("Inter American", INSTNM))
  
  # prep reactive vars
  
  # selected state and college from ui
  state <- reactive(input$selectedState)
  college <- reactive(ifelse(is.null(input$selectedCollege),
                             ifelse(is.null(state()),
                                    "Yale University",
                                    data_cleaned$INSTNM[data_cleaned$STABBR == state()][1]),
                             input$selectedCollege))
  
  # for tracking multiple college selects
  rv <- reactiveValues(college_arr = c())
  
  # scale of aggregate data for comparison
  level <- reactive(ifelse(is.null(input$selectedState), '(national)', '(state)'))
  
  # send location data to ui
  session$sendCustomMessage("getCities", data_cleaned)
  
  # receive state from ui and validate it, if valid,
  # send state abbr back to ui
  observeEvent(input$triggerInputValidation, {
    if (input$triggerInputValidation != 'CA') {
      session$sendCustomMessage("stateValidation", NULL)
    }
    session$sendCustomMessage("stateValidation", input$triggerInputValidation)
  })
  
  # show selected state from ui
  observeEvent(input$selectedState, {
    print(input$selectedState)
  })
  
  # add selected colleges to vector for comparison
  observeEvent(input$selectedCollege, {
    rv$college_arr <- c(rv$college_arr, input$selectedCollege)
  })
  
  # response to user click on REST SELECTION button,
  # clear rv$college_arr
  observeEvent(input$resetSelectedColleges, {
    print(input$resetSelectedColleges)
    # rv$college_arr <- c('Yale University')
    rv$college_arr <- c()
  })
  
  # selected state in a hidden input box in the ui,
  # the ui will use this navigate to the state
  output$inputState <- renderPrint({
    input$state
  })
  
  # get years from data (for salaries info)
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
  
  # salaries plot
  output$plot <- renderPlot({
    
    # filter data based on selected state and colleges
    if (is.null(state())) {
      data_gathered <- data_cleaned
    } else {
      data_gathered <- data_cleaned %>%
        filter(STABBR == state() |
               INSTNM %in% rv$college_arr)
    }
    
    # format data to get year column
    data_gathered <- data_gathered %>%
      gather(`mn_earn_wne_p6`, `mn_earn_wne_p7`, `mn_earn_wne_p8`,
             `mn_earn_wne_p9`, `mn_earn_wne_p10`, key = "year", value = "earning") %>%
      mutate(year = get_year(year))
    
    college2highlight <- college()
    
    # show multiple selected colleges for comparison
    if (!is.null(rv$college_arr)) {
      data_gathered %>%
        ggplot(aes(year, earning)) +
        geom_line(aes(group = INSTNM,
                      color = INSTNM %in% rv$college_arr,
                      alpha = INSTNM %in% rv$college_arr)) +
        geom_text(data=subset(data_gathered, INSTNM %in% rv$college_arr & year==8),
                   aes(label = INSTNM),
                   position = position_jitter()) +
        scale_x_continuous("Year after entry") +
        scale_y_continuous(paste("Salary", level()),
                           labels = scales::dollar_format()) +
        scale_color_manual("",
                           labels = c("Others",
                                      "Selected"),
                           values = c("grey", "#FF4081")) +
        # scale_alpha_discrete("",
        #                      labels = c("Others", college2highlight),
        #                      range=c(0.2, 1)) +
        # scale_color_manual("",
        #                    labels = c("Others",
        #                               college2highlight),
        #                    values = c("grey", "red")) +
        # scale_alpha_discrete("",
        #                      labels = c("Others", college2highlight),
        #                      range=c(0.2, 1)) +
        theme_bw() +
        theme(legend.position = 'none',
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(color = "#9e9e9e"))
    } else {
      # show just one college (default to avoid null errors)
      data_gathered %>%
        ggplot(aes(year, earning)) +
        geom_line(aes(group = INSTNM,
                      color = INSTNM == college(),
                      alpha = INSTNM == college())) +
        geom_text(data=subset(data_gathered, INSTNM == college() & year==8),
                  aes(label = INSTNM)) +
        scale_x_continuous("Year after entry") +
        scale_y_continuous(paste("Salary", level()),
                           labels = scales::dollar_format()) +
        scale_alpha_discrete("",
                             labels = c("Others", college2highlight),
                             range=c(0.2, 1)) +
        scale_color_manual("",
                           labels = c("Others",
                                      college2highlight),
                           values = c("grey", "#FF4081")) +
        scale_alpha_discrete("",
                             labels = c("Others", college2highlight),
                             range=c(0.2, 1)) +
        theme_bw() +
        theme(legend.position = 'none',
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(color = "#9e9e9e")) 
    }
  })
  
  # plots for cost (% student loan)
  output$plotCost <- renderPlot({
    college2highlight <- college()
    
    if (is.null(state())) {
      data_cleaned2 <- data_cleaned
    } else {
      data_cleaned2 <- data_cleaned %>%
        filter(STABBR == state() | INSTNM %in% rv$college_arr)
    }
    
    get_college_abbr <- Vectorize(function (name) {
      s <- strsplit(name, " ")
      print(s)
      abbr <- ''
      for (w in s[[1]]) {
        abbr <- paste0(abbr, substr(w,1,1))
      }
      abbr
    })
    
    if (!is.null(rv$college_arr)) {
      plt <- data_cleaned2 %>%
        filter(loan_ever > 0.5) %>%
        ggplot() +
          geom_density(aes(loan_ever)) +
          geom_vline(xintercept = data_cleaned2$loan_ever[data_cleaned2$INSTNM %in% rv$college_arr],
                     color = 'red') +
        scale_y_continuous("") +
        scale_x_continuous("",
                           # limits = c(min(data_cleaned2$loan_ever), max(data_cleaned2$loan_ever)),
                           labels = scales::percent_format()) +
        # scale_color_manual("",
        #                    labels = c("Others",
        #                               color_codes),
        #                    values = colors) +
        # scale_alpha_discrete("",
        #                      labels = c("Others", college2highlight),
        #                      range=c(0.2, 1)) +
        # scale_size_discrete("",
        #                     labels = c("Others", college2highlight),
        #                     range=c(1, 5)) +
        ggtitle(paste("% students granted loan", level())) +
        theme_bw() +
        theme(legend.position = 'none',
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.x = element_line(color = "black"))
      
      
      if (is.null(state()) & length(rv$college_arr) > 5) {
        return(plt)
      } else {
        # hide labels if more than 5 colleges are selected (reduce messy overlap)
        return(plt + geom_text(data=subset(data_cleaned2, INSTNM %in% rv$college_arr),
                               aes(x = loan_ever, y = 4,
                                   angle = 45,
                                   label = get_college_abbr(INSTNM))))
      }
      
    } else {
      # default to avoid null errors
      data_cleaned2 %>%
        filter(loan_ever > 0.5) %>%
        ggplot() +
        geom_density(aes(loan_ever)) +
        geom_vline(xintercept = data_cleaned$loan_ever[data_cleaned$INSTNM == college2highlight],
                   color = 'red') +
        geom_text(data=subset(data_cleaned, INSTNM == college2highlight),
                  aes(x = loan_ever, y = 4,
                      angle = 45,
                      label = get_college_abbr(INSTNM))) +
        scale_y_continuous("") +
        scale_x_continuous("",
                           labels = scales::percent_format()) +
        scale_color_manual("",
                           labels = c("Others",
                                      college2highlight),
                           values = c("grey", "#FF4081")) +
        # scale_alpha_discrete("",
        #                      labels = c("Others", college2highlight),
        #                      range=c(0.2, 1)) +
        # scale_size_discrete("",
        #                     labels = c("Others", college2highlight),
        #                     range=c(1, 5)) +
        ggtitle(paste("% students granted loan", level())) +
        theme_bw() +
        theme(legend.position = 'none',
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.line.x = element_line(color = "black"))
    }
  })
  
  # plot family income, similar to the prev plot for student loan
  output$plotIncome <- renderPlot({
    college2highlight <- college()
    if (is.null(state())) {
      data_cleaned2 <- data_cleaned
    } else {
      data_cleaned2 <- data_cleaned %>%
        filter(STABBR == state() | INSTNM %in% rv$college_arr)
    }
    
    get_college_abbr <- Vectorize(function (name) {
      s <- strsplit(name, " ")
      print(s)
      abbr <- ''
      for (w in s[[1]]) {
        abbr <- paste0(abbr, substr(w,1,1))
      }
      abbr
    })
    
    if (!is.null(rv$college_arr)) {
      plt <- data_cleaned2 %>%
        filter(loan_ever > 0.5) %>%
        ggplot() +
        geom_density(aes(faminc)) +
        geom_vline(xintercept = data_cleaned2$faminc[data_cleaned2$INSTNM %in% rv$college_arr],
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
      
      if (is.null(state()) & length(rv$college_arr) > 5) {
        return(plt)
      } else {
        return(plt + geom_text(data=subset(data_cleaned2, INSTNM %in% rv$college_arr),
                               aes(x = faminc, y = 0.00001,
                                   angle = 45,
                                   label = get_college_abbr(INSTNM))))
      }
    } else {
      data_cleaned2 %>%
        filter(loan_ever > 0.5) %>%
        ggplot() +
        geom_density(aes(faminc)) +
        geom_vline(xintercept = data_cleaned$faminc[data_cleaned$INSTNM == college2highlight],
                   color = 'red') +
        geom_text(data=subset(data_cleaned, INSTNM == college2highlight),
                  aes(x = faminc, y = 0.00001,
                      angle = 45,
                      label = get_college_abbr(INSTNM))) +
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
    }
  })
  
  # Dropped for simplicity and this info is not very useful either
  # output$plotCulture <- renderPlot({
  #   college2highlight <- "Yale University"
  #   if (!is.null(college())) {
  #     college2highlight <- college()
  #   }
  #   
  #   idx <- data_cleaned$INSTNM == college2highlight
  #   y <- round(c(data_cleaned$pct_white[idx], data_cleaned$pct_black[idx],
  #                data_cleaned$pct_asian[idx], data_cleaned$pct_hispanic[idx]))
  # 
  #   ggplot() +
  #     aes(x = reorder(c('White', 'Black', 'Asian', 'Hispanic'), y), y = y,
  #         fill = c('White', 'Black', 'Asian', 'Hispanic')) +
  #     geom_bar(width = 1, stat = 'identity') +
  #     scale_fill_brewer("", palette = 'Set2') +
  #     scale_y_continuous("") +
  #     scale_x_discrete("") +
  #     geom_text(aes(x = c('White', 'Black', 'Asian', 'Hispanic'),
  #                   y = y - y/2,
  #                   label = paste0(y, '% ', c('White', 'Black', 'Asian', 'Hispanic'))),
  #               hjust = -0.5,
  #               inherit.aes = TRUE) +
  #     coord_flip() +
  #     ggtitle("College diversity") +
  #     theme_bw() +
  #     theme(legend.position = 'none',
  #           panel.grid.major = element_blank(),
  #           panel.grid.minor = element_blank(),
  #           panel.border = element_blank(),
  #           axis.text.y = element_blank(),
  #           axis.ticks.y = element_blank(),
  #           axis.text.x = element_blank(),
  #           axis.ticks.x = element_blank(),
  #           axis.line.x = element_blank(),
  #           axis.line.y = element_blank())
  # })
  
  output$title <- renderText({
    paste('Stats for', college())
  })
  
})

# force rigid heights as it is not clear how to fit the plots
# according to container dims in html
shinyApp(ui = htmlTemplate("www/index.html",
                           title = textOutput("title"),
                           plot = plotOutput("plot", height = "250px"),
                           plotCost = plotOutput("plotCost", height = '125px'),
                           plotIncome = plotOutput("plotIncome", height = '125px')
                           ),
         server)