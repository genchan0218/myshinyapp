source("global.R")

server <-  function(input, output, session){
  
  # Reset function (input$refresh ) Start ==============
  observeEvent(input$refresh, {
    shinyalert::shinyalert(inputId = "reset_app","Reset your information ...", showCancelButton = TRUE, type = "info", timer = 5000, confirmButtonCol = "#DD6B55")
  })
  
  observe({
    req(input$reset_app)
    refresh()
    shinyalert::shinyalert("Done!", "", type = "success", timer = 10000)
  })
  # Reset function (input$refresh ) End =============

 
  #================================
  # Get data of last trade
  
values <- reactiveValues(data = NULL, stickers = NULL, dt_frome = NULL, dt_to = NULL)
  

observeEvent(input$submit,{
  #symbols
  symbols <-  input$stickers 
  # Subtitle
  subtitle <- sticker %>% filter(symbol %in% input$stickers)
  date_from <- input$dt_frome
  date_to <- input$dt_to
  Stocks <- lapply(symbols, function(i) get_stock(i, date_from, date_to, periodicity = "daily"))
  Stocks <- setNames(Stocks, symbols)
  list2env(Stocks, envir = .GlobalEnv)

  # stocks data
  stocks <- lapply(Stocks, function(i) i[[1]])


  # return data
  returns <- lapply(Stocks, function(i) i[[2]])
  returns <- do.call("rbind", returns)

  # last trade data
  last_trade <- lapply(Stocks, function(i) i[[3]])


  lit <- list(stocks,symbols,subtitle, returns,last_trade)
  # values$data contained stocks data, Symbols, Subtitle, return data and last trade data
  values$data <- lit


#=================================
    name <- values$data[[2]]
    dt <- values$data[[1]]
    subtitle  <- values$data[[3]]
    subtitle <- gsub("\\(.*","",subtitle$name)
    last_trade <- values$data[[5]]
    returns  <- values$data[[4]]

    # Use return data to plot list of stocks
    output$plot_out_line <- renderPlotly({
      p <- plot_ly(returns, x = ~Date, y = ~Return, color = ~Name) %>% add_lines()
      p <- p %>% layout(legend = list(orientation = 'h', x = 0.5, y = 1.2,
                        xanchor = 'center', yref = 'paper',
                        font = list(size = 10)))

    })

    # Use return data to plot list of stocks
    output$plot_out_line <- renderPlotly({
      #=================================
      name <- values$data[[2]]
      dt <- values$data[[1]]
      subtitle  <- values$data[[3]]
      subtitle <- gsub("\\(.*","",subtitle$name)
      last_trade <- values$data[[5]]
      returns  <- values$data[[4]]

      p <- plot_ly(returns, x = ~Date, y = ~Return, color = ~Name) %>% add_lines()
      p <- p %>% layout(legend = list(orientation = 'h', x = 0.5, y = 1.2,
                                      xanchor = 'center', yref = 'paper',
                                      font = list(size = 10)))

    })

    # Title of the box
    output$title_line  <- renderText(
      HTML(paste0("<span style='font-size:20px;'>",paste0(name, collapse = " vs "),"</span><span style='font-size:15px;'>","&nbsp;&nbsp;&nbsp;","","</span>"))
    )

    output$footer_line  <- renderText(
      HTML(paste0("<span style='font-size:20px;'> How to Develop a Stock Analysis Application using R Shiny?</span><br>"))
    )

    output$footer_line_1  <- renderText(
      HTML(paste0("<span style='font-size:12px;'><a href='https://www.loankimrobinson.com'>Author: Loan Kim Robinson </a></span><br>
                    <span style='font-size:12px;'>Email: loankimrobinson@gmail.com</span><br>"))
    )

    output$dl_all <- renderUI({
      actionButton("dl_all","Download Data",icon = icon("save"), style = "color: #fff; background-color: #32907c; border-color: #32907c; width:100%")
    })

    # render everything in a box
    output$plot_line <- renderUI({
      box(
        width = 12,
        title = tagList(htmlOutput("title_line"),
                        span(style = "position:absolute;right:1em;top:4px;",uiOutput("dl_all"))),
        plotlyOutput("plot_out_line", height = "500"),
        footer = fluidRow(htmlOutput("footer_line",style = "text-align:center;"),
                          htmlOutput("footer_line_1",style = "text-align:right;padding-right:10px;"))
      )
    })
    #==========================

    output$boxes <- renderUI({
      lapply(1:length(name), function(a, name, dt, subtitle,last_trade) {

        output[[name[[a]]]] <- renderText(
          HTML(paste0("<span style='font-size:40px;'>",format(last_trade[[a]]$Last,nsmall=2,big.mark=","),"</span><span style='font-size:18px;'> USD</span>"))
          )

        output[[paste0("vol_",name[[a]])]] <- renderText(
          HTML(paste0("<span style='font-size:12px;'>Volume: ",format(dt[[a]]$Volume[1], nsmall=0,big.mark=","),"</span><span style='font-size:12px;'> </span>"))
        )

        output[[paste0("low_",name[[a]])]] <- renderText(
          HTML(paste0("<span style='font-size:12px;'>Low: ",sprintf("%.2f",dt[[a]]$Low[1]),"</span><span style='font-size:12px;'> </span>"))
        )

        output[[paste0("name_",name[[a]])]] <- renderText(
          HTML(paste0("<span style='font-size:20px;'>",name[[a]],"</span><span style='font-size:15px;'>","&nbsp;&nbsp;&nbsp;",subtitle[[a]],"</span>"))
        )

        output[[paste0("ch_",name[[a]])]] <- renderText(
          HTML(last_trade[[a]]$change_out)
        )

        output[[paste0("time_",name[[a]])]] <- renderText(
          HTML(last_trade[[a]]$Time)
        )

       box(
           width = 3,
            title = htmlOutput(paste0("name_",name[[a]])),
            htmlOutput(name[[a]],style = "text-align:center;"),br(),
            splitLayout(cellWidths = c("70%", "30%"),
                       htmlOutput(paste0("ch_",name[[a]]),style = "text-align:left;padding-left:10px;font-size:22px;color:#32907c"),
                       div(style ="text-align:center;",
            actionButton(name[[a]], label = "Explore", class = "css-selector",
                         onclick = "Shiny.setInputValue('btnLabel', this.id);",  #onclick = "Shiny.setInputValue('btnLabel', this.this.innerText);", to capture label
                         style = "background-color:#353c42;border-color:transparent;color:white;"))
            ),
           htmlOutput(paste0("time_",name[[a]]),style = "text-align:left;padding-left:10px;font-size:10px;color:white"),
           footer = fluidRow(htmlOutput(paste0("vol_",name[[a]]),style = "text-align:right;padding-right:10px;"))
        )
      }, name = name, dt = dt, subtitle = subtitle,last_trade = last_trade)
    })

}) # end of obserevent observeEvent(input$submit,{

observeEvent(input$btnLabel,{
  req(input$submit)
  stock <- values$data[[1]]
  stock <- stock[[input$btnLabel]]
  
  
  last_trade <- values$data[[5]]
  last_trade <- last_trade[[input$btnLabel]]
 
  names <- input$btnLabel
  subtitle <- sticker %>% filter(symbol %in% input$btnLabel)
  subtitle <- gsub("\\(.*","",subtitle$name)
  
  
  
  # ggplot2
  output$ggplot_area <- renderPlot({
    
    min <- min(stock$Open)-(min(stock$Open)/10)
    max <- max(stock$Open)
    grad_df <- data.frame(yintercept = seq(0, max(stock$Open), length.out = 1000),
                          alpha = seq(1,0.0, length.out = 1000))
    
    p <- ggplot2::ggplot(stock, aes(x= Date, y = Open)) +
      geom_area(fill = "#32907c", alpha = 0.9) + 
      geom_hline(data = grad_df, aes(yintercept = yintercept, alpha = alpha),
                 size = 1, colour = "white") +
      geom_line(colour = "#32907c", size = 1) +
      coord_cartesian(ylim = c(min, max))
    p <- p +labs(x ="Date", y = "Price ($)")
    p <- p + theme(legend.position = "none")
    print(p)

  })
 
  # plotly
  output$plot_out <- renderPlotly({
    plot_function(stock, names)
   })
  
  output$title  <-  output$title_1 <- renderText(
    HTML(paste0("<span style='font-size:20px;'>",names,"</span><span style='font-size:15px;'>","&nbsp;&nbsp;&nbsp;",subtitle[1],"</span>"))
  )
  
  output$dl <- renderUI({
    actionButton("dl","Download Data",icon = icon("save"), style = "color: #fff; background-color: #32907c; border-color: #32907c; width:100%")
  })

  # Plotly
  output$plot <- renderUI({
    box(
      width = 12,
      title = htmlOutput("title"),
      plotlyOutput("plot_out", height = "500")
    )
  })
  
  #ggplot2
  output$plot_area <- renderUI({
    box(
      width = 12,
      title = tagList(htmlOutput("title_1"),
                       span(style = "position:absolute;right:1em;top:4px;",uiOutput("dl"))),
      plotOutput("ggplot_area",height = "500")
    )
  })
  
})




}