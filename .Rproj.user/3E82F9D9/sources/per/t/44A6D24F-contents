source("global.R")

#refresh Button ============


ui = dashboardPage(
  skin = "midnight",
  header = source("header.R", local = TRUE)$value,
  sidebar = source("sidebar.R", local = TRUE)$value,
  body = dashboardBody(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    br(),
    fluidRow( uiOutput("boxes")),br(),
    fluidRow(column(width = 6,uiOutput("plot")),
             column(width = 6,uiOutput("plot_area"))),
    fluidRow(column(width = 10, offset = 1,uiOutput("plot_line")))

  ),
  controlbar = dashboardControlbar(
    tagList(
      fluidRow(column(width = 8, offset = 2,br(),
      actionButton("refresh","Refresh",icon("refresh"), width = "100%",style = "color: #fff; background-color: #DD6B55; border-color: #DD6B55"))),
      helpText("This is refresh button, it will help you to reset your application back to default.", style = "padding-left : 10px; padding-right : 10px;")
      )
  ),
  footer = dashboardFooter(left = HTML("<span style='font-size:12px;'><a href='https://www.loankimrobinson.com'>Author: Loan Kim Robinson </a></span><br>
                                        <span style='font-size:12px;'>Email: loankimrobinson@gmail.com</span><br>"),
                           right = "San Diego, Feb 7th 2019"),
)      
            
            
            
            
            
            
            
            