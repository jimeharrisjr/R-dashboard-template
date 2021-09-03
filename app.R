## app.R ##

## 
library(shiny)
library(shinyFiles)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
# Common packages I use
library(dplyr)
library(data.table)
library(visNetwork)
library(iptools)
library(ipaddress)
library(jsonlite)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(mapproj)
library(dashboardthemes)
library(elastic)
library(lubridate)
library(digest)
library(chorddiag)
library(stringr)
library(igraph)
source('customTheme.R')
con<<-elastic::connect(port=9200)
source('functions.R')
ui <- dashboardPage(title='Template', # This title sets the tab title, skin default is blue, but there are also black, purple, green, red, and yellow. 
                    # dashboardHeader(title=tags$a(href='https://www.google.com',tags$script(src = "message-handler.js"),
                    #                                tags$img(src='Batman-PNG-Transparent.png', height='45px'))),
                    dashboardHeader(
                      
                      ### changing logo
                      title = shinyDashboardLogo(
                        theme = "purple_gradient",
                        boldText = "NS-Edge",
                        mainText = "Dashboard",
                        badgeText = "v0.1"
                      )),
                    # The Side Bar buts the menu on the left, and creates the spaces and links for the tabsNames
                    dashboardSidebar(sidebarMenu(
                      menuItem('Dashboard', tabName = "dashboard", icon = icon("dashboard")),
                      menuItem('Second', tabName = "second", icon = icon("tablet")),
                      menuItem('Third', tabName = "third", icon = icon("bar-chart-o")),
                      menuItem('Fourth', tabName = "fourth", icon = icon("bar-chart-o")),
                      menuItem('Fifth', tabName = "fifth", icon = icon("cog"))
                    ),width = '150px'),
                    # add some HTML tags, etc next, if desired
                    dashboardBody( #shinyDashboardThemes(
                     # theme = "blue_gradient"
                      customTheme
                    ,tags$head(tags$style(HTML(".btn-styled {
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
                                    tabItem(tabName = 'dashboard',uiOutput('continuousPlot'),width = 12),#end tab 1
                                    tabItem(tabName = 'second',uiOutput('dtable') , width=12),#end tab 2
                                    tabItem(tabName = 'third', uiOutput('infoBoxes')),#end tab 3
                                    tabItem(tabName = 'fourth',uiOutput('selections')),#end tab 4
                                    tabItem(tabName = 'fifth' ,uiOutput('filter') ) # end tab 5
                                  )#endtabs
                    ) # end Dashboard body
                    ) # end UI




# SERVER STUFF IS NEXT

server <- function(input, output, session) { # need session for interactive stuff
 # isa<-fread('./data/iana_special.csv')
  mc<-fread('./data/mcast.csv')
  macs<-fread('./data/macs.csv')
  network_ip<-function(x){x<-ip_to_numeric(x);ipdb[x>=num_min & x <=num_max, network]}
  country_ip<-function(x){x<-ip_to_numeric(x);ipdb[x>=num_min & x <=num_max, country_name]}
  network_ip_special<-function(x){x<-ip_to_numeric(x);isa[x>=num_min & x <=num_max, address_block]}
  name_ip_special<-function(x){x<-ip_to_numeric(x);isa[x>=num_min & x <=num_max, name]}
  description_ip<-function(x){x<-ip_to_numeric(x);mc[x>=num_min & x <=num_max, Description]}
  makeTitle<-function(x){if (length(x)==0){return(NA)} else {return(paste0('<p>',x,'</p>'))}}
  remTitle<-function(x){x %>% str_remove_all('</*p>')}
  
  # ---------------------------STUFF FOR THE CONTINUOUSLY UPDATING PLOT 
  # (Borrowing from Mike Wise's response https://stackoverflow.com/questions/41438725/update-dynamically-created-plot-in-r-shiny?answertab=votes#tab-top )
  sensors<-get_mainindex(con)
  sensors[,earliest:=ymd(earliest)]
  youngest<-max(sensors$earliest)
  #sensors[,table:=paste(table,str_extract(earliest,'^[0-9]+'), sep = '-'), by=table]
  flowtable<-sensors[grepl('pcapflows',type),table]
  t<-today()-days(0:5)
  t<-t[t>=youngest]
  flowtable<-paste(flowtable,t, sep='-')
  dnstable<-sensors[grepl('DNS',type),table]
  dnstable<-paste(dnstable,t, sep='-')
  authtable<-sensors[grepl('auth',type),table]
  authtable<-paste(authtable,t, sep='-')
  rv <- reactiveValues(DNSdata=get_dns_all(con,dnstable),pcapinput=get_all_flows(con,flowtable),nodeData=data.table(),run=FALSE, selectedPcap=data.table()) # create reactive variables
  

  observeEvent(input$resetbutt,{ isolate({ rv$pcapinput<-get_all_flows(con,flowtable) }) })
  
  
  #render the UI with the plot and call the UI comntinuousPlot
  output$continuousPlot<-renderUI({# output the UI generated below into the reactive object "continuousPlot"
    output$networkplot <- renderVisNetwork({
      pcapinput<-rv$pcapinput# create reactive plot in output 
      lastdt<-max(pcapinput$lastseen)
      sellayout<-input$layout
      if (nrow(pcapinput)>0){
        layers<-input$layerselect
        #print(sellayout)
        if ('')
        # remote source hostname to local mac
        edges<-pcapinput[localsrc==FALSE & localdst==TRUE,.(from=hostname_src,to=macdst, N, label=description)]
        edges<-edges[,log(sum(N))+1, by=.(from,to,label)]
        colnames(edges)[4]<-'weight'
        nodes<-data.table(id=unique(edges$from), type='hostname')
        nodes<-rbind(nodes,data.table(id=unique(edges$to), type='mac'))
        # localhostsrc to local ipsrc
        tmp<-pcapinput[localsrc==TRUE,.(from=hostname_src, to=ipsrc,N, label=description)]
        tmp<-tmp[,log(sum(N)+1), by=.(from,to,label)]
        colnames(tmp)[4]<-'weight'
        nodes<-rbind(nodes,data.table(id=unique(tmp$from), type='hostname')) %>% unique()
        nodes<-rbind(nodes,data.table(id=unique(tmp$to), type='ip')) %>% unique()
        edges<-rbind(edges,tmp)
        #local ipsrc to local macsrc
        tmp<-pcapinput[localsrc==TRUE,.(from=ipsrc, to=macsrc,N, label=protocol)]
        tmp<-tmp[,log(sum(N)+1), by=.(from,to,label)]
        colnames(tmp)[4]<-'weight'
        nodes<-rbind(nodes,data.table(id=unique(tmp$from), type='ip')) %>% unique
        nodes<-rbind(nodes,data.table(id=unique(tmp$to), type='mac')) %>% unique
        edges<-rbind(edges,tmp)
        # local macsrc to local macdst
        tmp<-pcapinput[localsrc==TRUE & localdst==TRUE,.(from=macsrc, to=macdst,N, label='Ethernet')]
        tmp<-tmp[,log(sum(N)+1), by=.(from,to,label)]
        colnames(tmp)[4]<-'weight'
        nodes<-rbind(nodes,data.table(id=unique(tmp$from), type='mac')) %>% unique
        nodes<-rbind(nodes,data.table(id=unique(tmp$to), type='mac')) %>% unique
        edges<-rbind(edges,tmp)
        # local macsrc to remote hostname
        tmp<-pcapinput[localsrc==TRUE & localdst==FALSE,.(from=macsrc, to=hostname_dst,N, label=description)]
        tmp<-tmp[,log(sum(N)+1), by=.(from,to,label)]
        colnames(tmp)[4]<-'weight'
        nodes<-rbind(nodes,data.table(id=unique(tmp$from), type='mac')) %>% unique
        nodes<-rbind(nodes,data.table(id=unique(tmp$to), type='hostname')) %>% unique
        edges<-rbind(edges,tmp)
        #local macdst to local ipdst
        tmp<-pcapinput[ localdst==TRUE,.(from=macdst, to=ipdst,N, label=protocol)]
        tmp<-tmp[,log(sum(N)+1), by=.(from,to,label)]
        colnames(tmp)[4]<-'weight'
        nodes<-rbind(nodes,data.table(id=unique(tmp$from), type='mac')) %>% unique
        nodes<-rbind(nodes,data.table(id=unique(tmp$to), type='ip')) %>% unique
        edges<-rbind(edges,tmp)
        # local ipdst to local hostnamedst
        tmp<-pcapinput[localdst==TRUE,.(from=ipdst, to=hostname_dst,N, label=protocol)]
        tmp<-tmp[,log(sum(N)+1), by=.(from,to,label)]
        colnames(tmp)[4]<-'weight'
        nodes<-rbind(nodes,data.table(id=unique(tmp$from), type='ip')) %>% unique
        nodes<-rbind(nodes,data.table(id=unique(tmp$to), type='hostname')) %>% unique
        edges<-rbind(edges,tmp)
        nodes[type=='ip',shape:='triangleDown']
        nodes[type=='mac',shape:='ellipse']
        nodes[type=='hostname',shape:='square']
        net<-graph_from_data_frame(edges, directed = FALSE)
        cfg<-cluster_fast_greedy(simplify(net))
        membs<-membership(cfg)
        nodes[, group:=membs[id]]
        visNetwork(nodes=nodes, edges=edges, w='100%',h='85%') %>% visIgraphLayout(layout = sellayout) %>% visOptions(highlightNearest = TRUE)


        #pcapinput %>% pcap_to_visNetwork(description = TRUE, type=input$layerselect) %>% get_fgclusters() %>% graph_vobj(glayout=sellayout,w="100%",h="90%")
      }
      
    })
    
    output$selectTable<-DT::renderDataTable({
      if (!is.null(input$current_node_selection) & nrow(rv$selectedPcap)>0){
        return(rv$selectedPcap)
      }
    }, selection='multiple', options = list(scrollX = TRUE))
    output$dynTable<-renderUI({
      if (!is.null(input$current_node_selection) & nrow(rv$selectedPcap)>0){
        box(DT::dataTableOutput('selectTable'), width = 9)
        
      }
    })
    output$digestBoxes<-renderUI({
      if (!is.null(input$selectTable_rows_selected)){
        rownums<-input$selectTable_rows_selected
        digests<-rv$selectedPcap[rownums,digest] %>% unique()
        print(digests)
        # create a tag list
        tl<-lapply(digests,function(x){mjournal$find(paste0('{"digest":"',x,'"}'))})#journalLookup(m=mjournal,digest=x)})
        
        p<-list()
        for (i in 1:length(tl)){
          a<-tl[[i]]
          oldest<-min(a$datetime);print(oldest)
          newest<-max(a$datetime); print(newest)
          numOcc<-sum(a$N); print(numOcc)
          p[[i]]<-box(title=a$digest[1],
              fluidRow(infoBox(oldest,value='Oldest', width=12)),
              fluidRow(infoBox(newest,value='Newest', width=12)),
              fluidRow(infoBox(numOcc,value='Times Seen', width=12)), width=3
              
            )
        }

        tagList(p)
      }
      })
    

    output$chordd <- renderChorddiag({
      #print(input$selectTable_rows_selected)
      if (nrow(rv$pcapinput)>0 & nrow(rv$nodeData)>0){
        pcapinput<-copy(rv$pcapinput)
        ip<-input$current_node_selection
        if (!is.null(ip)){
          pcapinput<-pcapinput[layer_2_src==ip | layer_2_dst==ip | layer_1_src==ip | layer_1_dst==ip]
          rv$selectedPcap<-pcapinput
        }
        #pcapinput[,port:=paste(layer_3_id,port)]
        pc<-pcapinput[,.N, by=.(layer_2_src,layer_2_dst)] %>% unique()
        
        hn<-copy(rv$nodeData)
        hn<-hn[id %in% pc$layer_2_src | id %in% pc$layer_2_dst]
        hn[shape=='circle',title:=paste0(str_extract(id,'^[0-9]+.'),'x.x.x')]
        hn<-hn[,.(id,title)]
        hn[,title:=remTitle(title)]
        #print(hn)
        
        if(input$resolvehn){
          pc<-merge(pc,hn, by.x = 'layer_2_src', by.y = 'id', all.x = TRUE)
          cn<-colnames(pc)
          #print(pc)
          cn[length(cn)]<-'srchost'
          colnames(pc)<-cn
          pc[is.na(srchost), srchost:=layer_2_src]
          pc<-merge(pc,hn, by.x = 'layer_2_dst', by.y = 'id', all.x = TRUE)
          cn<-colnames(pc)
          cn[length(cn)]<-'dsthost'
          colnames(pc)<-cn
          pc[is.na(dsthost), dsthost:=layer_2_dst]
          pc<-pc[,.N, by=.(srchost,dsthost)]
         # print(pc)
        }
        colnames(pc)<-c('srchost','dsthost','N')
        if (nrow(pc)<2){
          pc2<-pc[,.(srchost=dsthost, dsthost=srchost, N)]
          pc<-rbind(pc,pc2)
        }
        dat<-pc[,.(srchost,dsthost)]
        ord1<-dat[,N:=.N, by=srchost][,.(name=srchost,N)] %>% unique
        ord1<-rbind(ord1,unique(dat[,N:=.N, by=dsthost][,.(name=dsthost,N)]))
        n<-max(ord1$N)+1; print(n)
        colors<-colorRampPalette(c('blue','green','yellow','red'))(n)
        ord1[,color:=colors[N]]
        pc<-pc[order(N)]
        pc<-dcast(pc, srchost~dsthost, value.var = 'N')
        #print(pc)
        m<-pc[,3:ncol(pc)]
        m<-as.matrix(m)
        
        rownames(m)<-pc$srchost
       groupnames<-c(rownames(m),colnames(m))
       groupcolors<-groupnames %>% lapply(function(x){ord1[name==x,color]}) %>% unlist
       
        chorddiag(m,type='bipartite' ,groupColors = groupcolors, 
                  showTicks = FALSE, groupnameFontsize = 11,groupPadding = 1,
                  groupnamePadding = 10, margin=100)

      }
    })
    # The below will create (render) the dynamic UI
    fluidRow( # create two plots in a fluid row
      box(checkboxGroupInput('selectsensors','Select Which Sensors to Monitor',sensors$table,sensors$table, inline=TRUE), 
          radioGroupButtons('layout','Layout',choiceNames=c('fr','kk','lgl','mds','star','sugiyama'),choiceValues=c('layout_with_fr','layout_with_kk','layout_with_lgl','layout_with_mds','layout_as_star','layout_with_sugiyama')), 
          width = 12),
      box(
        checkboxGroupInput('layerselect','Layers',choices = c('DNS','MAC','Hostname'), selected = c('DNS','MAC','Hostname'), inline = TRUE),
        visNetworkOutput("networkplot", height='800px') %>% withSpinner(), actionButton("resetbutt","Reset"),# Set Box Details
        title = 'Network Diagram', footer = NULL, status = 'info',
        solidHeader = TRUE, background = NULL, width = 12, height = '900px',
        collapsible = FALSE, collapsed = FALSE),
      uiOutput('dynTable'),uiOutput('digestBoxes'),
    # create a second box with a different plot  
    box(radioButtons('resolvehn','Resolve Hostnames?',choices = c(TRUE,FALSE), selected = TRUE, inline = TRUE),
        chorddiagOutput('chordd', height='800px'), # put another plot in the other
        title = "Chord View", footer = NULL, 
        status = 'info', # other valid status : primary Blue (sometimes dark blue) , success Green , info Blue , warning Orange , danger Red
        solidHeader = TRUE, background = NULL, width = 12, height = '1000px',
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
    
    dt<-v$df
    dt<-setDT(dt)
    dt$region<-tolower(dt$region)
    states <- as.data.table(ggplot2::map_data("state")) # get the map data for states
    
    map.df <-merge(states,dt,key='region')
    map.df <- map.df[order(map.df$order)]
    map.df[,Count:=group] # set an initial value for Count to avoid a warning
    if (!is.null(input$select)){ #avoid warnings by making sure the input has been recorded before setting the values of the column
      map.df$Count<-map.df[,input$select, with=FALSE]
    }
    v$mapData<-as.data.table(dplyr::select(map.df,-Count))
    
    output$map<-renderPlot({ # map a heatmap 
     gp<- ggplot(map.df, aes(x=long,y=lat,group=group))+
        ggtitle(paste(input$select,'by State')) +
        geom_polygon(aes(fill=Count))+
        geom_path()+ 
        scale_fill_gradientn(colours=rev(heat.colors(100)),na.value="grey50")+
        coord_map() +
        theme(plot.title = element_text(color="red", size=32,hjust=.5,face="bold.italic"))
      print(gp)
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

