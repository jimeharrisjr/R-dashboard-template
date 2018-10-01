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

ui <- dashboardPage(title='Template',skin='blue', # This title sets the tab title, skin default is blue, but there are also black, purple, green, red, and yellow. 
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
                                    tabItem(tabName = 'second', uiOutput('continuousPlot'), width=12),#end tab 2
                                    tabItem(tabName = 'third', uiOutput('third')),#end tab 3
                                    tabItem(tabName = 'fourth',uiOutput('fourth')),#end tab 4
                                    tabItem(tabName = 'fifth', uiOutput('fifth')) # end tab 5
                                  )#endtabs
                    ) # end Dashboard body
                    ) # end UI




# SERVER STUFF IS NEXT

server <- function(input, output, session) { # need session for interactive stuff
  
  v <- reactiveValues(murderMean = NULL, assaultMean=NULL, rapeMean=NULL) # Reactive Values can be read and written to by any function
  
  
  #----------------------------------------------STUFF FOR A DATA TABLE----------
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
        ,title = NULL, footer = NULL, 
        status = NULL,  # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
        solidHeader = FALSE, background = NULL, width = 6, height = NULL,
        collapsible = FALSE, collapsed = FALSE)
  }) # end data table UI
  #----------------------------END DATA TABLE STUFF---------------------------
  
  
  
  # ---------------------------STUFF FOR THE CONTINUOUSLY UPDATING PLOT
  rv <- reactiveValues(x=rnorm(1),run=FALSE) # create reactive variables
  
  autoInvalidate <- reactiveTimer(intervalMs=500,session) # set an autoinvalidate timer
  
  observe({
    autoInvalidate()
    isolate({ if (rv$run) { rv$x <- c(rv$x,rnorm(1)) } })
  })
  observeEvent(input$gogobutt, { isolate({ rv$run=TRUE      }) })
  observeEvent(input$stopbutt, { isolate({ rv$run=FALSE      }) })
  observeEvent(input$resetbutt,{ isolate({ rv$x=rnorm(1) }) })
  #render the UI with the plot and call the UI comntinuousPlot
  output$continuousPlot<-renderUI({
    output$histplot <- renderPlot({
      htit <- sprintf("Hist of %d rnorms",length(rv$x))
      hist(rv$x,col = "steelblue",main=htit,breaks=12)
    })
    output$valuePlot <-renderPlot({
      plot(x=1:length(rv$x), y=rv$x,col = 'steelblue',main='Chaotic Neutral', type = 'b', pch=19)
    })
    fluidRow( # create two plots in a fluid row
      box(actionButton("gogobutt","Go"), # Put a histogram in one
        actionButton("stopbutt","Stop"),
        actionButton("resetbutt","Reset"),
        plotOutput("histplot"), # Set Box Details
        title = NULL, footer = NULL, status = NULL,
        solidHeader = FALSE, background = NULL, width = 6, height = NULL,
        collapsible = FALSE, collapsed = FALSE),
      
    box(plotOutput('valuePlot'), # put another plot in the other
        title = "Some Random Plot", footer = NULL, 
        status = 'info', # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
        solidHeader = TRUE, background = NULL, width = 6, height = NULL,
        collapsible = FALSE, collapsed = FALSE)
    )
  }) # END RENDERUI
  
  #----------------------------------------END CONTINUOUS PLOT
  
  
} # END SERVER

# Run the application 
shinyApp(ui = ui, server = server)

