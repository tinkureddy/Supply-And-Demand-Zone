library("shinybusy")
library("shinycssloaders")
library("shinycustomloader")
library(shinyWidgets)
library(shinyjs)
library(shinythemes)
library(shinyjs)
library(rintrojs)
library(shinyalert)
library(shinydashboard)
library(highcharter)
library(quantmod)
library("RJSONIO")
library(magrittr)
library(dplyr)
library(lubridate)

library("RQuantLib")

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    
    title = tags$a( tags$head(tags$style(HTML(".name { color: white }"))),
                    'Supply and Demand Zone Dashboard',class="name")
    
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("home")),
      menuItem("Supply and Demand Zone Identifier", tabName = "zone_identifier", icon = icon("info"))
    )
  ),
  dashboardBody(
    
    tabItems( 
      tabItem(
        tabName = "introduction",
        
        tags$h2("Information"),
        
        fluidRow(column(width = 6,
                        tags$div(class = "box box-solid",
                                 tags$div(class = "box-header with-border collapse",
                                          tags$i(class = "fas fa-book-open"),
                                          tags$h3(class = "box-title custom-box-header","Guide & Usage")
                                 ),
                                 tags$div(class = "box-body",
                                          
                                          tags$h4('Quick Manual'),
                                          tags$li('Demand Zone : The demand zone is where all the big buyers are located.'),
                                          tags$li(' Supply Zone :  The supply zone is where all the big sellers are located.'),
                                          br(),
                                          tags$p(HTML(paste0('Here is the ',a(href = 'https://docs.google.com/document/d/1NIcVwSS7HQt3my2zlnm5fW69U2u3qzMaq1GAh8iFlxg/edit', 'Document'),' about Supply and Demand Zone !!')))

                                 )
                        )
                        
        )
        # ,
        # column(width = 6,
        #        tags$div(class = "box box-solid custom-body",
        #                 tags$div(class = "box-header with-border custom-box-header",
        #                          tags$h2(" One line definition : ")
        #                 ),
        #                 tags$p('“A computer program is said to learn from experience ‘E’, with respect to some class of tasks ‘T’ and performance measure ‘P’ if its performance at tasks in ‘T’ as measured by ‘P’ improves with experience ‘E’."')
        #        )
        # )
        )
        
      ),
      tabItem(
        tabName = "zone_identifier",
        box(width=12,status="primary",solidHeader=TRUE,
            fluidRow(
              column(4,
                selectizeInput("bot_timeframe", label = "Range :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","1 Day"="1d","1 Week"="1wk","1 Month"="1mo"), selected = "1d", multiple = FALSE,
                               options = list(maxOptions = 10))
              ),
              column(6,
                     tags$p("*If you select the interval as 1 min,2 min then we analyse for last 2 days."),
                     tags$p("*If you select the interval as 5 min then we analyse for last 5 days."),
                     tags$p("*If you select the interval as 15 min, 1 hour then we analyse for last 60 days."),
                     tags$p("*If you select the interval as 1 Day then we analyse for last 2 months."),
                     tags$p("*If you select the interval as 1 Week, 1 Month then we analyse for last 36 months.")
                     # ,
                     # tags$a(href="https://docs.google.com/document/d/1NIcVwSS7HQt3my2zlnm5fW69U2u3qzMaq1GAh8iFlxg/edit", "Document", target="_blank") 
                     ),
              column(1,
                     # submitButton("Run Trade",width = "100px")
                     actionButton(inputId = "bot_submit",label = "Run Trade",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              )
            ),
            fluidRow(
              box(width=12,title= "Daily Zones :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
                  div(style = 'overflow-y:scroll;',
                      withSpinner( DT::dataTableOutput("demand_daily_screener")))
              ),
              box(width=12,title= "Stocks In Zone :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
                  div(style = 'overflow-y:scroll;',
                      withSpinner( DT::dataTableOutput("Stocks_Screener")))
              ),
              box(width=12,title= "Historical Times :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
                  div(style = 'overflow-y:scroll;',
                      withSpinner( DT::dataTableOutput("Execution_Screener")))
              )
            )
            
        )
      )
    )
    
    
  )
)


# # Define UI for application that draws a histogram
# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Demand and Supply Zone Identifier"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       # radioButtons("nifty_selection", "Select the Bucket:",
#       #              c("Nifty 50" = "nifty_50",
#       #                "Nifty 500" = "nifty_500"),
#       #              selected = "nifty_500"),
#       selectizeInput("bot_timeframe", label = "Range :", choices=c("1 min"="1m","2 min"="2m","5 min"="5m","15 min"="15m","1 hour"="1h","1 Day"="1d","1 Week"="1wk","1 Month"="1mo"), selected = "1d", multiple = FALSE,
#                      options = list(maxOptions = 10)),
#       # dateRangeInput("date_range", "Date range:", start = "2021-01-01", end = Sys.Date(),format = "yyyy-mm-dd"),
#       # tags$p("*Note: If you select the date as 2021-01-01 then we start the analysis one day after the selected working date. If you select 2021-03-31 as end date then we stop the analysis one day before the selected working date."),
#       tags$p("*If you select the interval as 1 min,2 min then we analyse for last 2 days."),
#       tags$p("*If you select the interval as 5 min then we analyse for last 5 days."),
#       tags$p("*If you select the interval as 15 min, 1 hour then we analyse for last 60 days."),
#       tags$p("*If you select the interval as 1 Day then we analyse for last 2 months."),
#       tags$p("*If you select the interval as 1 Week, 1 Month then we analyse for last 36 months."),
#       tags$a(href="https://docs.google.com/document/d/1NIcVwSS7HQt3my2zlnm5fW69U2u3qzMaq1GAh8iFlxg/edit", "Document", target="_blank")
# 
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       box(width=12,title= "Daily Zones :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
#       div(style = 'overflow-y:scroll;',
#           withSpinner( DT::dataTableOutput("demand_daily_screener"))),
#       ),
#       box(width=12,title= "Stocks In Zone :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = T ,
#           div(style = 'overflow-y:scroll;',
#               withSpinner( DT::dataTableOutput("Stocks_Screener"))),
#       )
#       # ,
#       # box(width=12,title= "Weekly Zones :",status="primary",solidHeader=TRUE , collapsible = T, collapsed = F ,
#       #     div(style = 'overflow-y:scroll;',
#       #         withSpinner( DT::dataTableOutput("demand_weekly_screener")))
#       # )
#     )
#   )
# )