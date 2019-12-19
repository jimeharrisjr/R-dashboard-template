## app.R ##
library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinyWidgets)
# Common packages I use
library(dplyr)
library(data.table)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

ui <- dashboardPage(title='Template',skin='blue', # This title sets the tab title, skin default is blue, but there are also black, purple, green, red, and yellow. 
                    dashboardHeader(title=tags$a(href='https://www.google.com',tags$script(src = "message-handler.js"),
                                                   tags$img(src='Batman-PNG-Transparent.png', height='45px'))),
                    # The Side Bar buts the menu on the left, and creates the spaces and links for the tabsNames
                    dashboardSidebar(sidebarMenu(
                      menuItem('Dashboard', tabName = "dashboard", icon = icon("dashboard")),
                      menuItem('Second', tabName = "second", icon = icon("tablet")),
                      menuItem('Third', tabName = "third", icon = icon("bar-chart-o")),
                      menuItem('Fourth', tabName = "fourth", icon = icon("bar-chart-o")),
                      menuItem('Fifth', tabName = "fifth", icon = icon("cog"))
                    ),width = '150px'),
                    # add some HTML tags, etc next, if desired
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
                                  # Define your tab items - in this case, each tab calls out a custom UI rendered in the Server
                                  tabItems(
                                    tabItem(tabName = 'dashboard',uiOutput('dtable'),width = 12),#end tab 1
                                    tabItem(tabName = 'second', uiOutput('continuousPlot'), width=12),#end tab 2
                                    tabItem(tabName = 'third', uiOutput('infoBoxes')),#end tab 3
                                    tabItem(tabName = 'fourth',uiOutput('selections')),#end tab 4
                                    tabItem(tabName = 'fifth' ,uiOutput('filter') ) # end tab 5
                                  )#endtabs
                    ) # end Dashboard body
                    ) # end UI




# SERVER STUFF IS NEXT

server <- function(input, output, session) { # need session for interactive stuff
  
  v <- reactiveValues(murderMean = NULL, assaultMean=NULL, rapeMean=NULL, mapData=NULL) # Reactive Values can be read and written to by any function
  
  
  #----------------------------------------------STUFF FOR A DATA TABLE----------
  output$dtable<-renderUI({ # make a data table dynamically
    #use the USA arrests data for the data table and calculate some means
    data("USArrests")
    dt<-USArrests
    means<-apply(dt[2:5,],2,mean)
    dt<-cbind(row.names(dt),dt)
    colnames(dt)[1]<-'region'
    v$df<-dt
    #row.names(v$df)<-NULL
    # set some reactive values for other functions
    v$murderMean<-means[1]
    v$assaultMean<-means[2]
    v$rapeMean<-means[4]
    
    # output the data table to a reactive variable called "data"
    output$data<-DT::renderDataTable(dt)
    
    # now create the UI - with "data" output
    fluidPage(
      box(DT::dataTableOutput('data') # add box details
                  ,title = NULL, footer = NULL, 
                  status = NULL,  # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
                  solidHeader = FALSE, background = NULL, width = 10, height = NULL,
                  collapsible = FALSE, collapsed = FALSE)# end box
      )# end page
    
  }) # end data table UI
  #----------------------------END DATA TABLE STUFF---------------------------
  
  
  
  # ---------------------------STUFF FOR THE CONTINUOUSLY UPDATING PLOT 
  # (Borrowing from Mike Wise's response https://stackoverflow.com/questions/41438725/update-dynamically-created-plot-in-r-shiny?answertab=votes#tab-top )
  rv <- reactiveValues(x=rnorm(1),run=FALSE) # create reactive variables
  
  autoInvalidate <- reactiveTimer(intervalMs=500,session) # set an autoinvalidate timer
  # trigger the reactive function with the timer
  observe({
    autoInvalidate()
    isolate({ if (rv$run) { rv$x <- c(rv$x,rnorm(1)) } })
  })
  # observe the buttons and either start, stop, or reset the plots
  observeEvent(input$gogobutt, { isolate({ rv$run=TRUE      }) })
  observeEvent(input$stopbutt, { isolate({ rv$run=FALSE      }) })
  observeEvent(input$resetbutt,{ isolate({ rv$x=rnorm(1) }) })
  
  #render the UI with the plot and call the UI comntinuousPlot
  output$continuousPlot<-renderUI({# output the UI generated below into the reactive object "continuousPlot"
    output$histplot <- renderPlot({ # create reactive plot in output 
      htit <- sprintf("Hist of %d rnorms",length(rv$x))
      hist(rv$x,col = "steelblue",main=htit,breaks=12)
    })
    output$valuePlot <-renderPlot({ # create a second reactive plot
      plot(x=1:length(rv$x), y=rv$x,col = 'steelblue',main='Chaotic Neutral', type = 'b', pch=19)
    })
    # The below will create (render) the dynamic UI
    fluidRow( # create two plots in a fluid row
      box(actionButton("gogobutt","Go"), # Put a histogram in one
        actionButton("stopbutt","Stop"),
        actionButton("resetbutt","Reset"),
        plotOutput("histplot"), # Set Box Details
        title = NULL, footer = NULL, status = NULL,
        solidHeader = FALSE, background = NULL, width = 6, height = NULL,
        collapsible = FALSE, collapsed = FALSE),
    # create a second box with a different plot  
    box(plotOutput('valuePlot'), # put another plot in the other
        title = "Some Random Plot", footer = NULL, 
        status = 'info', # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
        solidHeader = TRUE, background = NULL, width = 6, height = NULL,
        collapsible = FALSE, collapsed = FALSE)
    )
  }) # END RENDERUI
  #----------------------------------------END CONTINUOUS PLOT
  
  
  
  #----------------------------------------MAKE SOME INFO BOXES----------------
  output$infoBoxes<-renderUI({ # output a UI called infoBoxes
    output$murderBox <- renderInfoBox({# make this an infobox with a refular icon
      infoBox(
        "Murder Mean", v$murderMean, icon = icon("list"),
        color = "purple"
      )
    })
    output$assaultBox <- renderInfoBox({# an infobox with a glyficon
      infoBox(
        "Assault Mean", v$assaultMean, icon = icon("wrench", lib = "glyphicon"), # Glyphicons are only those listed here: https://getbootstrap.com/docs/3.3/components/
        color = "yellow"
      )
    })
    output$rapeBox <- renderValueBox({ #a valueBox instead of an infobox
      valueBox(
        v$rapeMean, "Rape Mean", icon = icon("alert", lib = "glyphicon"), # Glyphicons are only those listed here: https://getbootstrap.com/docs/3.3/components/
        color = "red"
      )
    })
    # output a box with the info/value boxes inside
    box(infoBoxOutput('murderBox', width = 3), infoBoxOutput('assaultBox', width = 3),valueBoxOutput('rapeBox', width = 3),
        title = "Some Average Values", footer = "Two InfoBoxes and a ValueBox" , 
        status = 'info',  # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
        solidHeader = FALSE, background = NULL, width = 12, height = NULL,
        collapsible = FALSE, collapsed = FALSE)
    
  })
  #----------------------------------------END INFO BOXES-------
  
  #----------------------------------------BEGIN MAP--------------
  # Create a section list and output it as a control
  output$selections<-renderUI({
    choices<-colnames(v$df)[c(2,3,5)] # dynamically assign the selections
    fluidPage(
      box(selectInput("select", "Select crime to display",choices = choices),uiOutput('heatMap'),
          title = NULL, footer = NULL, 
          status = 'info',  # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
          solidHeader = FALSE, background = NULL, width = 12, height = NULL,
          collapsible = FALSE, collapsed = FALSE)
      
      
    )
    
  })
  # render a heatmap based on a selection in the control panel
  output$heatMap<-renderUI({
    
    dt<-as.data.table(v$df)
    dt$region<-tolower(dt$region)
    states <- as.data.table(map_data("state")) # get the map data for states
    
    map.df <-merge(states,dt,key='region')
    map.df <- map.df[order(map.df$order),]
    map.df[,Count:=group] # set an initial value for Count to avoid a warning
    if (!is.null(input$select)){ #avoid warnings by making sure the input has been recorded before setting the values of the column
      map.df$Count<-map.df[,input$select, with=FALSE]
    }
    v$mapData<-as.data.table(dplyr::select(map.df,-Count))
    
    output$map<-renderPlot({ # map a heatmap 
      ggplot(map.df, aes(x=long,y=lat,group=group))+
        ggtitle(paste(input$select,'by State')) +
        geom_polygon(aes(fill=Count))+
        geom_path()+ 
        scale_fill_gradientn(colours=rev(heat.colors(100)),na.value="grey90")+
        coord_map() +
        theme(plot.title = element_text(color="red", size=32,hjust=.5,face="bold.italic"))
      
    })
    fluidRow(
    box(plotOutput('map', width = '100%', height='800px'), width = 12, height='900px')
    )
   
  })
  
  #----------------------------------------END MAP----------------
  
  #----------------------------------------BEGIN FILTERS--------------
  output$filter<-renderUI({
    if(is.null(v$mapData)){ # if the data hasn't been placed in a reactive variable yet, pop a message and tell them
      session$sendCustomMessage(type='testmessage',message='You must visit tab four before coming here')
      
    }
    else({ # otherwise, render the page
      # calculate the max and min for each of the variables to create the input sliders dynamically
      long<-round(range(v$mapData$long))
      lat<-round(range(v$mapData$lat))
      murderRange<-range(v$mapData$Murder)
      assaultRange<-range(v$mapData$Assault)
      rapeRange<-range(v$mapData$Rape)
      popRange<-range(v$mapData$UrbanPop)
      cn<-c('region', 'avgLong',  'avgLat', 'Murder', 'Assault', 'UrbanPop', 'Rape') # I'm setting these for the final, processed data table
      
      
     # create a fluid UI
      fluidPage(
        fluidRow(
          box(checkboxGroupInput('selectColumns',"Select Columns to Display", choices=cn, selected = cn ,inline=TRUE), # set up the checkbox for the final processed data table
                     title = NULL, footer = NULL, 
                     status = 'info',  # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
                     solidHeader = FALSE, background = NULL, width = 12, height = NULL,
                     collapsible = FALSE, collapsed = FALSE
                     ) # end Box
                 ), # end Row
        fluidRow(
          box( # box for all of the sliders
          sliderInput('longitude',"Longitude Range",min = long[1], max = long[2], value = (long)),
          sliderInput('lattitude',"Lattitude Range",min = lat[1], max = lat[2], value = (lat)),
          sliderInput('murderRate',"Murder Rate Range",min = murderRange[1], max = murderRange[2], value = (murderRange)),
          sliderInput('assaultRate',"Assault Rate Range",min = assaultRange[1], max = assaultRange[2], value = (assaultRange)),
          sliderInput('rapeRate',"Rape Rate Range",min = rapeRange[1], max = rapeRange[2], value = (rapeRange)),
          sliderInput('popRate',"Urban Population Range",min = popRange[1], max = popRange[2], value = (popRange)),
          title = NULL, footer = NULL, 
          status = 'info',  # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
          solidHeader = FALSE, background = NULL, width = 4, height = NULL,
          collapsible = FALSE, collapsed = FALSE
        )# end Box
        ,uiOutput('filterData') # output the dynamic UI for the data table (next)
        )# end Row
        
      ) # end Page
    })
    
  })
  output$filterData<-renderUI({ # generate the dynamic data table (this is removed from the previous section to keep from resetting the filters each time)
    # find the elements in the data frame between the two ends of the sliders
    mdf<-v$mapData[Murder>=input$murderRate[1] & Murder <=input$murderRate[2] & Assault>=input$assaultRate[1] & Assault <=input$assaultRate[2] & Rape>=input$rapeRate[1] & Rape <=input$rapeRate[2] & UrbanPop>=input$popRate[1] & UrbanPop <=input$popRate[2] ]
    mdf[,avgLong:=mean(long), by=region] # average the boundary latlongs for the may for each state
    mdf[,avgLat:=mean(lat), by=region]
    mdf<-mdf[!duplicated(region)] # remove the duplicated states, then filter on the average Lat/Long
    mdf<-mdf[ avgLong>=input$longitude[1] & avgLong <=input$longitude[2] & avgLat>=input$lattitude[1] & avgLat <=input$lattitude[2] ]
    mdf<-mdf[,list(region,Murder,Assault,UrbanPop,Rape,avgLong,avgLat)] # Select only the columns needed for the render
    omdf<-dplyr::select(mdf,input$selectColumns) # select the columns chosen by the user dynamically
    output$mdf<-DT::renderDataTable(omdf, options = list(scrollX = TRUE)) # output the filtered table
    
      box(DT::dataTableOutput('mdf'), width = 8) # render the UI
    
  })
  #----------------------------------------END FILTERS----------------
} # END SERVER

# Run the application 
shinyApp(ui = ui, server = server)

