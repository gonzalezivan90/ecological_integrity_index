library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(networkD3)
library(rgdal)
library(bit)
library(digest)
library(dplyr)
library(ggplot2)
library(highcharter)
library(htmlwidgets)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(knitr)
library(magrittr)
library(raster)
library(RColorBrewer)
library(rgdal)
library(rmarkdown)
library(shiny)
library(shinydashboard)
library(shinycssloaders)


# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
# https://shiny.rstudio.com/gallery/superzip-example.html

# setwd('C:/GoogleDrive/IG/server_IG/frag/')
 #setwd('/srv/shiny-server/frag/')
 
ecoLevelsTags <- c('Biome', 'Realm', 'Ecorregion')
ecoLevels <- c('BIOME_NAME', 'REALM', 'ECO_NAME')
YN <- c('Yes', 'No')

# if ( file.exists('data/ecoreg_leaf.RData') ){
#   load('data/ecoreg_leaf.RData')
#   #print(ecoreg$ECO_NAME)
#   print('load')
# } else
  {
  load( file = 'data/ecoreg_shp.RData') # ecoreg
  #print(ecoreg$ECO_NAME)
  #print('save')
  shp <- list(BIOME_NAME = leaflet() %>% addTiles() %>%
              addPolygons(data = ecoreg, 
                          popup = paste0("<strong>Biome</strong>: ", ecoreg$BIOME_NAME,"<br>",
                                         "<strong>Realm</strong>: ", ecoreg$REALM,"<br>",
                                         "<strong>Ecoregion</strong>: ", ecoreg$ECO_NAME,"<br>"),
                          group = "biome", fillOpacity = .7,
                          stroke = TRUE,  opacity = 0,
                          layerId = ~ecoreg$BIOME_NUM,
                          color = ecoreg$BIOME_col) %>%
            addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                             position = "bottomleft",  #"bottomleft",
                             options = layersControlOptions(collapsed = FALSE)) %>%
              addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
              
              addLegend(position = "bottomleft",
                        colors = '#5DC863FF', 
                        labels= 'Tropical & Subtropical Moist Broadleaf Forests',
                        opacity = 1) %>% setView(lng = 20, lat = -50, zoom = 2.3)
            , 
            
            REALM = leaflet() %>% addTiles() %>%
              addPolygons(data = ecoreg, 
                          popup = paste0("<strong>Biome</strong>: ", ecoreg$BIOME_NAME,"<br>",
                                         "<strong>Realm</strong>: ", ecoreg$REALM,"<br>",
                                         "<strong>Ecoregion</strong>: ", ecoreg$ECO_NAME,"<br>"),
                          group = "realm", fillOpacity = .7,
                          layerId = ~ecoreg$REALM_NUM,
                          stroke = TRUE,  opacity = 0,
                          color = ecoreg$REALM_col) %>%
              addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                               position = "bottomleft",  #"bottomleft",
                               options = layersControlOptions(collapsed = FALSE)) %>%
              addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
            addLegend(position = "bottomleft",
                      colors = unique(ecoreg@data[, c('REALM', 'REALM_col')])$REALM_col, 
                      labels= unique(ecoreg@data[, c('REALM', 'REALM_col')])$REALM,
                     opacity = 1) %>% setView(lng = 20, lat = -50, zoom = 2.3)
            ,
            
            
            ECO_NAME = leaflet() %>% addTiles() %>%
              addPolygons(data = ecoreg, 
                          popup = paste0("<strong>Biome</strong>: ", ecoreg$BIOME_NAME,"<br>",
                                         "<strong>Realm</strong>: ", ecoreg$REALM,"<br>",
                                         "<strong>Ecoregion</strong>: ", ecoreg$ECO_NAME,"<br>"),
                          label = ~htmlEscape(ecoreg$ECO_NAME),
                          highlightOptions = highlightOptions(stroke = 1, 
                                                              color = "grey50",
                                                              bringToFront = T, 
                                                              opacity = .4,
                                                              weight = 1),
                          layerId = ~ecoreg$ECO_ID,
                          group = "eco", fillOpacity = .7,
                          stroke = TRUE,  opacity = 0,
                          color = ecoreg$ECO_col) %>%
             
              addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                               position = "bottomleft", 
                               options = layersControlOptions(collapsed = FALSE)) %>%
              addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>% 
              setView(lng = 20, lat = -50, zoom = 2.3)
            )
  #print('save')
  #save(shp, file = 'data/ecoreg_leaf.RData')
}

######## UI -----------------


ui <- navbarPage("Forest transitions", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", 
                                      fixed = TRUE, draggable = TRUE, 
                                     # top = 420, left = 20, right = 20, bottom = 10, width = 950, height = 330,
                                     top = 60, left = "auto", right = 20, bottom = "auto", width = 400, height = 800,
                                     
                                     
                                      # h2("ZIP explorer"),
                                       h3(""),
                                      
                                     # fluidRow(
                                     #   column(width = 6,
                                     #          selectInput("in_level", "Spatial level", ecoLevelsTags),
                                     #          selectInput("in_for", "Incluide no forest? ", YN, selected = "No")),
                                     #   column(width = 6, sankeyNetworkOutput("sankeyplot", height = 200) #, plotOutput("scatterCollegeIncome", height = 250)
                                     #          #plotOutput("sankeyplot", height = 200) #, plotOutput("scatterCollegeIncome", height = 250)
                                     #   ))
                                     
                                     fluidRow(
                                       column(width = 6, selectInput("in_level", "Spatial level", ecoLevelsTags)),
                                       column(width = 6, selectInput("in_for", "Incluide no forest? ", YN, selected = "No")),
                                     ),
                                     sankeyNetworkOutput("sankeyplot", height = 700)
                                     
                                     # selectInput("in_level", "Spatial level", ecoLevelsTags),
                                     # selectInput("in_for", "Incluide no forest? ", YN, selected = "No"),
                                     # sankeyNetworkOutput("sankeyplot", height = 600) #, plotOutput("scatterCollegeIncome", height = 250)
                                            #plotOutput("sankeyplot", height = 200) #, plotOutput("scatterCollegeIncome", height = 250)
                                       
                                     
                                      # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                      #                  # Only prompt for threshold when coloring or sizing by superzip
                                      #                  numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      # ),
                                      
                        ),
                        
                        tags$div(id="cite",
                                 #'Data compiled for ', tags$a('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                                 
                                 'Created by Patrick Jantz Ph.D. and Iván González from ', tags$a(href="https://goetzlab.rc.nau.edu/",'GEODE Lab'), ' @ Northern Arizona university'
                        
                    )
           )
           ), # close tabpanel
           

           
           conditionalPanel("false", icon("crosshair"))
           
)


######### Server --------------



server <- function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  ecolev <- 'ECO_NAME'
  output$map <- renderLeaflet({
    shp[[ecolev]]
  })
  
  
  observe({
    # input <- list(in_level = 'BIOME_NAME')
    ecolev <- ecoLevels[ecoLevelsTags %in% input$in_level]
    
    output$map <- renderLeaflet({
      shp[[ecolev]]
    })
    
  })
  
  observe({
    # input <- list(in_level = 'BIOME_NAME')
    ecolev <- ecoLevels[ecoLevelsTags %in% input$in_level]
    
    output$map <- renderLeaflet({
      shp[[ecolev]]
    })
    
  })
  
  
  
  data_of_curve <- reactiveValues(clickedMarker=NULL)
  observe({
    observeEvent(input$map_shape_click,{
      myClickPol <- data_of_curve$clickedMarker <- input$map_shape_click
      # print(myClickPol)
      # print(str(myClickPol))
      #save(myClickPol, file = 'temp_myClickPol.RData')
      
        rdatName <- paste0('data/rdata/',myClickPol$group,'_', myClickPol$id, '.RData')
        print(paste('RDat: ', rdatName))
        load(rdatName)
        # "nodes2" "links2" "p2"     "links3" "p3" 
        output$sankeyplot <- renderSankeyNetwork({
          inc_for <<- input$in_for
          if(inc_for == 'Yes'){ 
            p2 
          } else if(inc_for == 'No'){ 
              p3 
            }
        })
    })
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
 
}


#shinyApp(ui = ui, server = server)
app <- shinyApp(ui, server)
shinyParallel::runApp(app, max.sessions = 20, users.per.session = 5)