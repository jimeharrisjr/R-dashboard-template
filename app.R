## app.R ##
library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinyWidgets)
# Comon packages I use
library(dplyr)
library(data.table)
library(jsonlite)
library(ggplot2)

ui <- dashboardPage(title='Template',skin='blue',
                    dashboardHeader(title=tags$a(href='https://www.google.com',tags$script(src = "message-handler.js"),
                                                   tags$img(src='Batman-PNG-Transparent.png', height='45px'))),
                    dashboardSidebar(sidebarMenu(
                      menuItem('Dashboard', tabName = "dashboard", icon = icon("dashboard")),
                      menuItem('Second', tabName = "second", icon = icon("tablet")),
                      menuItem('Third', tabName = "third", icon = icon("bar-chart-o")),
                      menuItem('Fourth', tabName = "fourth", icon = icon("bar-chart-o")),
                      menuItem('Fifth', tabName = "fifth", icon = icon("cog"))
                    ),width = '150px'),
                    dashboardBody(tags$head(tags$style(HTML(".btn-styled {
                                                            border: 0;
                                                            line-height: 2.5;
                                                            padding: 0 20px;
                                                            font-size: 1rem;
                                                            text-align: center;
                                                            color: #fff;
                                                            text-shadow: 1px 1px 1px #000;
                                                            border-radius: 10px;
                                                            background-color: #337ab7;
                                                            background-image: linear-gradient(to top left,
                                                            rgba(0, 0, 0, .2),
                                                            rgba(0, 0, 0, .2) 30%,
                                                            rgba(0, 0, 0, 0));
                                                            box-shadow: inset 2px 2px 3px rgba(255, 255, 255, .6),
                                                            inset -2px -2px 3px rgba(0, 0, 0, .6);
                                                            }
                                                            
                                                            .btn-styled:hover {
                                                            background-color: #337aff;
                                                            }
                                                            
                                                            .btn-styled:active {
                                                            box-shadow: inset -2px -2px 3px rgba(255, 255, 255, .6),
                                                            inset 2px 2px 3px rgba(0, 0, 0, .6);
                                                            }
                                                            ")),tags$script(src = "message-handler.js")),
                                  # Boxes need to be put in a row (or column)
                                  tabItems(
                                    tabItem(tabName = 'dashboard',uiOutput('dtable'),width = 12),#end tab 1
                                    tabItem(tabName = 'second', uiOutput('second'), width=12),#end tab 2
                                    tabItem(tabName = 'third', uiOutput('third')),#end tab 3
                                    tabItem(tabName = 'fourth',uiOutput('fourth')),#end tab 4
                                    tabItem(tabName = 'fifth', uiOutput('fifth')) # end tab 5
                                  )#endtabs
                    ) # end Dashboard body
                    ) # end UI

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  v <- reactiveValues(murderMean = NULL, assaultMean=NULL, rapeMean=NULL) # Reactive Values can be read and written to by any function
  
  
  output$dtable<-renderUI({ # make a data table dynamically
    #use the USA arrests data for the data table and grab some means
    data("USArrests")
    dt<-USArrests
    means<-apply(dt[2:5,],2,mean)
    dt<-cbind(row.names(dt),dt)
    colnames(dt)[1]<-'State'

    # set some reactive values for other functions
    v$murderMean<-means[1]
    v$assaultMean<-means[2]
    v$rapeMean<-means[4]
    
    # output the data table to a reactive variable called "data"
    output$data<-renderDataTable(dt)
    
    # now create the UI - with "data" output
    box(dataTableOutput('data') # add box details
        ,title = NULL, footer = NULL, status = NULL,
        solidHeader = FALSE, background = NULL, width = 6, height = NULL,
        collapsible = FALSE, collapsed = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

