library(bit)
library(digest)
library(dplyr)
library(ggplot2)
library(highcharter)
library(htmlwidgets)
library(leaflet)
library(leaflet.extras)
library(knitr)
library(magrittr)
library(raster)
library(RColorBrewer)
library(rgdal)
library(rgee) # ee_Initialize() #ee_install()
library(rmarkdown)
library(shiny)
library(shinydashboard)
# library(shinySignals)
#install.packages('shinycssloaders')
library(shinycssloaders)

#setwd('C:/GoogleDrive/IG/server_IG/eii/')
#setwd('/srv/shiny-server/eii/')
ee_Initialize(email = 'gonzalezgarzon.ivan@gmail.com') # as server
#ee_Initialize(email = 'ig299@nau.com')
ee_Initialize(email = 'gonzalez.ivan1990co@gmail.com')

modis0 <- ee$ImageCollection("MODIS/006/MYD13Q1")$select('NDVI')

#rgee::ee_install_upgrade()
# ee_Initialize() #
# ee_install()
#  ee_check()
# ee_check_python()
# ee_check_credentials()
# ee_check_python_packages()

load('others.RData')
#load('studymask_ama_lla.RData')
load('studyleaflet.RData')
load('curves_highchart.RData')
load('curvesLlaAma.RData')
load('GPP.RData')
load('biomSimpl.RData') # 

server <- function(input, output, session) {
  # output$intro <- enderUI({  
  #   rmarkdown::render(input = "introMD.md",
  #                     output_format = html_document(self_contained = TRUE),
  #                     output_file = 'introMD.html')  
  #   shiny::includeHTML('introMD.html') 
  # }) 
  
  # rli  ---------------
  output$rlibox <- renderHighchart({
    highchart() %>% 
      hc_title(text = "Índice de lista roja", 
               margin = 20, align = "left",
               style = list(color = "black", useHTML = TRUE)) %>% 
      hc_xAxis(categories = as.numeric(rownames(rliT))) %>% 
      hc_add_series(name = tax[8], data = rliT$Todos) %>%
      hc_yAxis(max = 1) %>% hc_yAxis(min = 0) %>% 
      hc_tooltip(table = TRUE, sort = TRUE) %>% 
      hc_exporting(enabled = TRUE) %>%  
      hc_credits(enabled = TRUE, text = "UICN Red list", href = "http://www.iucnredlist.org")
  })
  
  
  
  output$rligroup <- renderHighchart({
    highchart() %>%
      hc_title(text = "Especies evaluadas por grupo",
               margin = 20, align = "center",
               style = list(color = "black", useHTML = TRUE)) %>%
      hc_xAxis(categories = spCoun$group) %>%
      hc_chart(type = "column") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_series(name = 'Evaluaciones de lista roja por año', data = spCoun$evaluations)
  })
  
  # propEval  ---------------
  output$propEvalPer <- renderHighchart({
    highchart() %>%
      hc_xAxis(categories = cum$year) %>%
      hc_add_series(name = 'Especies', data = cum$proportion) %>%
      hc_yAxis(max = 100) %>% hc_yAxis(min = 0) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  # gap  ---------------
  output$gapcyear <- renderValueBox({
    valueBox(
      value = input$igapyear,
      subtitle = "Año de la estadística",
      icon = icon("calendar")
    )
  })
  
  output$gapcsize <- renderValueBox({
    valueBox(
      value = paste(input$igapsize, 'Km'),
      subtitle = "Tamaño de lado del pixel",
      icon = icon("pie-chart") # http://fontawesome.io/icons/
    )
  })
  
  output$gapccomp <- renderValueBox({
    d.time <- gaptimeSer[rownames(gaptimeSer) == input$igapsize, colnames(gaptimeSer) == input$igapyear]
    valueBox(
      d.time,
      "% superficie cubierta",
      icon = icon("percent"),
      color = if (d.time <= 10) {'red'}  else
        if(d.time > 10 & d.time <= 20 ) {'orange'} else
          if(d.time > 20 & d.time <= 40 ) {'yellow'} else
            if(d.time > 40 & d.time <= 60 ) {'lime'} else
              if(d.time > 60 & d.time <= 100 ) {'green'}
    )
  })
  
  ## study --------------
  
  output$studymap <- renderLeaflet({
    studyleaflet
  })
  
  ## curves --------------
  
  data_of_curve <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$studymap_shape_click,{
    data_of_curve$clickedMarker <- input$studymap_shape_click
    print(data_of_curve$clickedMarker)
    print(str(data_of_curve$clickedMarker))
  })
  
  
  output$studyLong <- renderHighchart({
    my_curve <- data_of_curve$clickedMarker$id
    if (is.null(my_curve)) {
      null_hc
    } else {
      hc_curves$lt[[my_curve]]
    }
  })
  
  output$studyYear <- renderHighchart({
    my_curve <- data_of_curve$clickedMarker$id
    if (is.null(my_curve)) {
      null_hc 
    } else {
      hc_curves$yearly[[my_curve]]
    }
  })
  
  output$studyMonth <- renderHighchart({
    my_curve <- data_of_curve$clickedMarker$id
    if (is.null(my_curve)) {
      null_hc
    } else {
      hc_curves$monthly[[my_curve]]
    }
  })
  
  
  #   # CLC --------------
  
  data_of_clc <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$clcmap_shape_click,{
    data_of_clc$clickedMarker <- input$clcmap_shape_click
  })
  
  
  # curves  ---------------
  
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$ecomap_shape_click,{
    data_of_click$clickedMarker <- input$ecomap_shape_click
    str(data_of_click$clickedMarker)
    print("-----")
    str(input$ecomap_shape_click)
  })
  
  output$ecomap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addPolygons(data = biom, weight = 0,
                  fillOpacity = .9, fillColor = ~dispal(biom$ID),
                  popup = biom$BIOMA, group = "Ecosistemas", layerId = ~ID) #%>%
  })
  
  output$plotlong <- renderHighchart({
    my_place <- data_of_click$clickedMarker$id
    if (is.null(my_place)) {
      null_hc
    } else {
      GPP[[my_place]]$lt_hc
    }
  })
  
  output$plotYear <- renderHighchart({
    my_place <- data_of_click$clickedMarker$id
    if (is.null(my_place)) {
      null_hc 
    } else {
      GPP[[my_place]]$ya_hc
    }
  })
  
  output$plotMonth <- renderHighchart({
    my_place <- data_of_click$clickedMarker$id
    if (is.null(my_place)) {
      null_hc
    } else {
      GPP[[my_place]]$mo_hc
    }
  })
  
  
  
  
  
  ##### Point -----------
  
  data_of_point <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$pointmap_shape_click,{
    data_of_point$clickedMarker <- input$pointmap_shape_click
    print(data_of_point$clickedMarker)
  })
  
  output$pointLong <- renderHighchart({
    clickxy <- data.frame(id = data_of_point$clickedMarker$id, 
                          x = data_of_point$clickedMarker$lng, 
                          y = data_of_point$clickedMarker$lat)
    
    if (is.null(clickxy$id)) {
      null_hc
    } else {
      hc_curves$lt[[clickxy$id]] #%>% hc_add_series(name="Pixel", type = 'line', data = ee_Ext_sub,  hcaes(x = D, y = eeExt), color = 'red')
    }
  })
  
  output$pointmap <- renderLeaflet({
    studyleaflet2
  })
  
  observeEvent(input$do, {
    
    if(is.null(data_of_point$clickedMarker)){
      print('NULLLLLLLL')
      print(data_of_point$clickedMarker)
    } else { 
      
      print('AAAAAA')
      print(data_of_point$clickedMarker)
      
      output$pointLong <- renderHighchart({
        xy <- data.frame(id = data_of_point$clickedMarker$id, 
                         x = data_of_point$clickedMarker$lng, 
                         y = data_of_point$clickedMarker$lat)
        # print(clickxy); # print(input$idateRng); # print(length(input$idateRng))
        idateRng <- as.character(input$idateRng)
        modis <- modis0$filterDate(start = as.character(idateRng[1]), opt_end = as.character(idateRng[2]))$select('NDVI')
        xye <- ee_as_sf(ee$Geometry$Point(xy$x, xy$y), quiet = TRUE)
        timeee <- system.time(ee_Ext_pt <- ee_extract(x = modis, y = xye, sf = FALSE)) #30
        ee_Ext_df <- data.frame(D = as.Date(gsub('^X', '', gsub('_', '-', names(ee_Ext_pt)))),
                                eeExt = as.numeric(ee_Ext_pt))
        ee_Ext_sub <- ee_Ext_df[which(ee_Ext_df$D >= idateRng[1] & ee_Ext_df$D <= idateRng[2]), ]
        
        hc_curves$lt[[xy$id]] %>% hc_add_series(name="Pixel", type = 'line',
                                                data = ee_Ext_sub, 
                                                hcaes(x = D, y = eeExt), color = 'red') %>% 
          hc_title(text = paste0('<br>', ecosNames[xy$id], '</br> | ', xy$x, ', ', xy$y, ' | ', round(timeee[3], 1), 'secs'))
      })
    }
    
  })
  
  
  ## polygon --------------
  
  output$polymap <- renderLeaflet({
    studyleaflet3
  })
  
  output$polyres <- renderLeaflet({
    studyleaflet4
  })
  
  
  output$polyLong <- renderHighchart({
    clickxy <- data.frame(id = data_of_point$clickedMarker$id, 
                          x = data_of_point$clickedMarker$lng, 
                          y = data_of_point$clickedMarker$lat)
    
    if (is.null(clickxy$id)) {
      null_hc
    } else {
      hc_curves$lt[[clickxy$id]] 
    }
  })
  
  observeEvent(input$do2, {
    
    polDraw <- input$polymap_draw_new_feature
    
    if(is.null(polDraw)){
      print('NULLLLLLLL')
      print(polDraw)
    } else { 
      
      print('AAAAAA')
      print(polDraw)
      
      coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]]); 
      save(coordMat, file = 'coordMat.RData'); print(coordMat)
      # load('coordMat.RData')
      
      #output$polyLong <- renderHighchart({
      output$polyres <- renderLeaflet({
        
        idateRng <- as.character(input$idateRng2)
        #idateRng <- as.Date(c("2010-10-13", "2011-04-05"))
        print(idateRng)
        
        id <- which(input$polylay %in% ecosNames) # id <- 1
        ltindex <- curveLT[[id]]
        ltindex <- ltindex[which(ltindex$D >= idateRng[1] & ltindex$D <= idateRng[2]), ]
        #head(ltindex); dim(ltindex)
        
        coordMat2 <- as.matrix(coordMat)[-nrow(coordMat),]
        eePol <- ee$Geometry$Polygon(
          lapply(
            apply(coordMat2, 1, function(x){ 
              as.matrix(x)
            }), as.numeric)
        ) 
        
        
        ## modis0 <- ee$ImageCollection("MODIS/006/MYD13Q1")$select('NDVI')
        modis2 <- modis0$filterDate(start = as.character(idateRng[1]), 
                                    opt_end = as.character(idateRng[2]))
        modisaoi <- modis2$filterBounds(eePol)
        acumIndex <- modisaoi$first()$multiply(0)
        
        for (i in 1:nrow(ltindex)){
          index.i <- modisaoi$filterDate(as.character(ltindex$D[i]))$first()$clip(eePol)
          index.2 <- index.i$subtract(ltindex$md[i])$abs()
          acumIndex <- acumIndex$add(index.2)
        }
        
        ee_qq <- acumIndex$reduceRegion(
          reducer = ee$Reducer$percentile(c(2, 98)) ,
          geometry = eePol,
          scale = 1000
        )
        
        qq <- ee_qq$values()$getInfo()
        # qq$getInfo()
        # qq$get(1)$getInfo()
        # getInfo()
        # q2 <- ee_qq$values()
        
        
        print('qq')
        vizParams <- list(
          min = qq[1],
          max = qq[2],
          palette = c("00FFFF", "0000FF")
          #palette = c("00FFFF", "0000FF")
        )
        # vizParams <- list(
        #   min = qq$NDVI_p5,
        #   max = qq$NDVI_p95,
        #   palette = c("00FFFF", "0000FF")
        #   #palette = c("00FFFF", "0000FF")
        # )
        
        
        Map$setCenter(lon = mean(unlist(coordMat2[, 1])),
                      lat = mean(unlist(coordMat2[, 2])),
                      zoom = 10)
        eeLeaf <- Map$addLayer(acumIndex, vizParams, name = "Result")
        
        eeLeaf@map
      })
    }
    
  })
  
  
  
} # close

ui <- dashboardPage(
  dashboardHeader(title = "Ecological integrity index", titleWidth = 350),
  
  dashboardSidebar(
    sidebarMenu(
      # Panel  ---------------
      menuItem("Intro", tabName = "intro"),
      menuItem("Ecosystems curves", tabName = "curves", icon = icon("globe")),
      menuItem("Study area curves", tabName = "study", icon = icon("bar-chart-o")), 
      menuItem("Calc the index", tabName = "timeseries", icon = icon("signal"), startExpanded = TRUE,
               menuSubItem("Time-series", tabName = "timeseries", icon = icon("random")),
               menuSubItem("Layer", tabName = "layermap", icon = icon("cube"))
      )
    )
  ), ## - dashboardSidebar
  
  dashboardBody(
    tabItems(
      tabItem("intro",
              fluidRow(
                tabBox(width = 12,
                       tabPanel("Intro",
                                includeMarkdown("md_intro.md")
                       ),
                       tabPanel("Integrity",
                                includeMarkdown("md_integ.md")
                       ),
                       tabPanel("Degradation",
                                includeMarkdown("md_deg.md")
                       ),
                       tabPanel("Example",
                                includeMarkdown("md_exam.md")
                       )
                )
              )
      ), 
      #### Curves  ---------------
      tabItem("curves",
              fluidRow(column(width = 5, leafletOutput('ecomap', height = '600px') %>% withSpinner(color="#0dc5c1")), 
                       column(width = 7,
                              tabsetPanel(type = "tabs",
                                          tabPanel("Long-term", 
                                                   highchartOutput("plotlong", height = "600px") %>% withSpinner(color="#0dc5c1")
                                          ),
                                          tabPanel("Aggregated", 
                                                   fluidRow(
                                                     highchartOutput("plotYear", height = "300px") %>% withSpinner(color="#0dc5c1"),
                                                     highchartOutput("plotMonth", height = "300px") %>% withSpinner(color="#0dc5c1")
                                                   )
                                          )
                              )
                       )
                       
              )
      ),
      
      # study  ---------------
      tabItem("study",
              fluidRow(column(width = 3, 
                              leafletOutput('studymap', height = '700px') %>% withSpinner(color="#0dc5c1")), 
                       column(width = 9,
                              fluidRow(
                                highchartOutput("studyLong", height = "400px") %>% withSpinner(color="#0dc5c1")),
                              fluidRow(
                                column(offset = 0, style='padding:0px;',
                                       highchartOutput("studyYear", height = "300px") %>% withSpinner(color="#0dc5c1"), width = 6),
                                column(offset = 0, style='padding:0px;',
                                       highchartOutput("studyMonth", height = "300px") %>% withSpinner(color="#0dc5c1"), width = 6)
                              )
                       )
              )
              
              
      ),
      
      
      
      # timeseries  ---------------
      tabItem("timeseries",
              
              fluidRow(column(width = 4, 
                              leafletOutput('pointmap', height = '600px') %>% withSpinner(color="#0dc5c1")), 
                       column(width = 8,
                              fluidRow(
                                box(width = 10, status = "info",
                                    
                                    sliderInput(inputId = "idateRng",label = '',
                                                min = as.Date(dayDateRange[1]),
                                                max = as.Date(dayDateRange[2]),
                                                value=as.Date(c('2010-10-13', '2011-04-05')),
                                                timeFormat="%Y-%m-%d")),
                                
                                box(width = 2, status = "info",
                                    actionButton("do", "Go", width = '40px')
                                )
                              ),
                              fluidRow(
                                highchartOutput("pointLong", height = "400px") %>% withSpinner(color="#0dc5c1")
                              )
                       )
              )
      ),
      
      # Polygon ---------------
      tabItem("layermap",
              
              fluidRow(
                box(width = 2, status = "info",
                    selectInput(inputId = "polylay", label = "layer:",
                                choices =  ecosNames, selected = NULL)
                ),
                
                box(width = 9, status = "info",
                    sliderInput(inputId = "idateRng2",label = '',
                                min = as.Date(dayDateRange[1]),
                                max = as.Date(dayDateRange[2]),
                                value=as.Date(c('2010-10-13', '2011-04-05')),
                                timeFormat="%Y-%m-%d")),
                
                box(width = 1, status = "info",
                    actionButton("do2", "Go", width = '40px')
                )
              ),
              
              fluidRow(column(width = 6, 
                              leafletOutput('polymap', height = '500px') %>% withSpinner(color="#0dc5c1")), 
                       column(width = 6,
                              leafletOutput('polyres', height = '500px') %>% withSpinner(color="#0dc5c1"))
                       #,
                       #column(width = 4, highchartOutput("polyLong", height = "400px") %>% withSpinner(color="#0dc5c1"))
                       
                       )
              
              
      )
    )
  )
)

shinyApp(ui = ui, server = server)
