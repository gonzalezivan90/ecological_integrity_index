library(highcharter)
library(dplyr)
#####


# ama <- readOGR('C:/GoogleDrive/Phd/2-F_INF544_RemoteSensing/project/mask_amazon.shp')
# lla <- readOGR('C:/GoogleDrive/Phd/2-F_INF544_RemoteSensing/project/mask_llanos.shp')
# ama$eco <- 'Amazonas'
# ama$ecoID <- 1
# lla$eco <- 'Llanos'
# lla$ecoID <- 2

# amapts <- readOGR('C:/GoogleDrive/Phd/2-F_INF544_RemoteSensing/project/rand1kama.shp')
# llapts <- readOGR('C:/GoogleDrive/Phd/2-F_INF544_RemoteSensing/project/rand1klla.shp')
# goodLla <- readOGR('C:/GoogleDrive/Phd/2-F_INF544_RemoteSensing/project/clean_lla.shp')
# goodAma <- readOGR('C:/GoogleDrive/Phd/2-F_INF544_RemoteSensing/project/clean_ama.shp')
# amapts$train <- !is.na(raster::extract(goodAma, amapts)[, 2]) * 1
# llapts$train <- !is.na(raster::extract(goodLla, llapts)[, 2]) * 1 
# llapts$eco <- 'Llanos'
# amapts$eco <- 'Amazonas'
# goodLla$eco <- 'Llanos'
# goodAma$eco <- 'Amazonas'
# goodLla$ecoID <- 2
# goodAma$ecoID <- 1
#  
# trainmask <- bind(goodAma, goodLla)
# trainpts <- bind(amapts, llapts)
# studymask <- bind(ama, lla)

# studymask$name <- iconv(studymask$name, from = 'utf8')
# studymask <- spTransform(studymask, CRSobj = CRS(pr))
# trainpts$x <- as.numeric(coordinates(trainpts)[, 1])
# trainpts$y <- as.numeric(coordinates(trainpts)[, 2])
# save(pr, studymask, trainpts, file = 'C:/GoogleDrive/IG/server_IG/eii/studymask_ama_lla.RData')
# pr <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


#write("TMP = 'C:/GoogleDrive/IG/server_IG/eii'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))
#write("TMP = '/srv/shiny-server/eii/'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))

Sys.getenv('R_SESSION_TMPDIR')
tmpRdir <- '/home/shiny/tmpR/tmpR'
dir.create(tmpRdir, recursive = TRUE)
write(paste0("R_SESSION_TMPDIR = '", tmpRdir ,"'"), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
write(paste0("TMP = '", tmpRdir ,"'"), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
write(   "TMP = '/home/shiny/tmpR/tmpR'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))
write("TMPDIR = '/home/shiny/tmpR/tmpR'", file=file.path(Sys.getenv('TMPDIR'), '.Renviron'))
write("R_USER = '/home/shiny/tmpR/tmpR'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))
Sys.setenv(R_SESSION_TMPDIR = '/home/shiny/tmpR')

Sys.getenv('R_SESSION_TMPDIR')
setwd('/srv/shiny-server/eii')

#setwd('C:/GoogleDrive/IG/server_IG/eii/')

library(leaflet)
library(raster)
load('studymask_ama_lla.RData')
load('curvesLlaAma.RData')
trainpts$x <- as.numeric(coordinates(trainpts)[, 1])
trainpts$y <- as.numeric(coordinates(trainpts)[, 2])

pal <- colorNumeric(
  palette = "PuBu", #"RdYlBu"
  domain = studymask@data$ecoID
)

palPts <- colorNumeric(
  palette = "inferno", #"RdYlBu"
  domain = 0:2
)
# 
popup <- paste0("<strong>", studymask$eco,"</strong><br>",
                studymask$name, "<br>")
popupPts <- paste0("<strong>", trainpts$eco,"</strong><br>",
                ifelse(trainpts$train == TRUE, 'Train', 'Test'), "<br>")
# 
# 
studyleaflet <- leaflet() %>% addTiles() %>%
  addPolygons(data = studymask,
              popup = popup,
              group = "Ecosystems",
              fillOpacity = .9, 
              color = ~pal(studymask$ecoID+1), 
              layerId = ~ecoID) %>%
  addCircleMarkers(data = trainpts, lng = ~x, lat = ~y, popup = popupPts,#,
  radius = .1, group = 'Points', color = ~palPts(train)# stroke = FALSE, fillOpacity = 0.5
) %>% addLegend("bottomleft",
                colors = pal(unique(studymask$ecoID+1)),
                labels= unique(studymask$eco),
                title= "Ecosystem") %>%
  addLegend("bottomleft",
            colors = palPts(unique(trainpts$train)),
            labels = c('Train', 'Others'), title = "Points") %>%
  addLayersControl(overlayGroups = c('Ecosystems', 'Points'),
                   #position = "topleft",
                   baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup('Points') %>%
  addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )

studyleaflet2 <-
  leaflet() %>% addTiles() %>%
  addPolygons(data = studymask,
              popup = popup,
              group = "Ecosystems",
              fillOpacity = .2, 
              color = ~pal(studymask$ecoID+1), 
              layerId = ~ecoID) %>%
  addCircleMarkers(data = trainpts, lng = ~x, lat = ~y, popup = popupPts,#,
                   radius = .1, group = 'Points', color = ~palPts(train)# stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addLegend("bottomleft",
            colors = pal(unique(studymask$ecoID+1)),
            labels= unique(studymask$eco),
            title= "Ecosystem") %>%
  addLegend("bottomleft",
            colors = palPts(unique(trainpts$train)),
            labels = c('Train', 'Others'), title = "Points") %>%
  addLayersControl(overlayGroups = c('Points'),
                   #position = "topleft",
                   baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup('Points') %>%
  addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )

studyleaflet3 <-
  leaflet() %>% addTiles() %>%
  addPolygons(data = studymask,
              popup = popup,
              group = "Ecosystems",
              fillOpacity = .2, 
              color = ~pal(studymask$ecoID+1), 
              layerId = ~ecoID) %>%
  addCircleMarkers(data = trainpts, lng = ~x, lat = ~y, popup = popupPts,#,
                   radius = .1, group = 'Points', color = ~palPts(train)# stroke = FALSE, fillOpacity = 0.5
  ) %>% 
  addLegend("bottomleft",
            colors = pal(unique(studymask$ecoID+1)),
            labels= unique(studymask$eco),
            title= "Ecosystem") %>%
  addLegend("bottomleft",
            colors = palPts(unique(trainpts$train)),
            labels = c('Train', 'Others'), title = "Points") %>%
  addLayersControl(overlayGroups = c('Ecosystems','Points'),
                   #position = "topleft",
                   baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup('Points') %>%
  addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
  leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                 rectangleOptions = FALSE, circleOptions = FALSE,
                                 markerOptions = FALSE, circleMarkerOptions = FALSE,
                                 editOptions = leaflet.extras::editToolbarOptions())


studyleaflet4 <-
  leaflet() %>% addTiles() %>%
  addPolygons(data = studymask,
              popup = popup,
              group = "Ecosystems",
              fillOpacity = .2, 
              color = ~pal(studymask$ecoID+1), 
              layerId = ~ecoID)

grep('tmp', capture.output(str(studyleaflet2)))
str(studyleaflet)

## 1
# dir.create(paste0(getwd(), '/studyleaflet'))
# sapply(list.files(studyleaflet$dependencies[[1]]$src$file, full.names = TRUE), function(x){
#   file.copy(x, paste0('studyleaflet/', basename(x)))
# })

R.utils::copyDirectory(studyleaflet$dependencies[[1]]$src$file, 'studyleaflet')

## 2
R.utils::copyDirectory(studyleaflet2$dependencies[[1]]$src$file, 'studyleaflet2')

## 3
R.utils::copyDirectory(studyleaflet3$dependencies[[1]]$src$file, 'studyleaflet3')


studyleaflet$dependencies[[1]]$src$file <- paste0(getwd(), '/studyleaflet')
studyleaflet2$dependencies[[1]]$src$file <- paste0(getwd(), '/studyleaflet2')
studyleaflet3$dependencies[[1]]$src$file <- paste0(getwd(), '/studyleaflet3')


# save(studyleaflet, studyleaflet2, studyleaflet3, studyleaflet4, trainpts, file = 'studyleaflet.RData')
load('studyleaflet.RData')



lt_hc <- hchart(subset(curveLT[[1]], type = 'Cons'), name= 'Range', type = "columnrange", 
                hcaes(x = D, low = low, high = upp, color = md)) %>% 
  # hc_add_series(name="All:Upper", type = 'line', data = lt_all, hcaes(x = date, y = upp)) %>%
  # hc_add_series(name="All:Median", type = 'line', data = lt_all, hcaes(x = date, y = med)) %>%
  # hc_add_series(name="All:Lower", type = 'line', data = lt_all, hcaes(x = date, y = low)) %>%
  hc_add_series(name="Median", type = 'line', data = subset(curveLT[[1]], type = 'Cons'), 
                hcaes(x = D, low = low, high = upp, color = md), color = 'white') %>%
  hc_yAxis(title =  list(text = 'Average GPP')) %>% hc_xAxis(title =  list(text = 'Date')) %>% 
  hc_add_theme(hc_theme_db())


hc_curves <- list(lt = list(
  hchart(curveLT[[1]], type = "line", 
         hcaes(x = D, y = md, factor = type, group = type, color = type)) %>% 
    hc_yAxis(title = list(text = "NDVI")) %>% 
    hc_xAxis(title = list(text = "Date")) %>% hc_add_theme(hc_theme_db()) %>% 
    hc_exporting(enabled = TRUE)
  ,
  hchart(curveLT[[2]], type = "line", 
         hcaes(x = D, y = md, factor = type, group = type, color = type)) %>% 
    hc_yAxis(title = list(text = "NDVI")) %>% 
    hc_xAxis(title = list(text = "Date")) %>% hc_add_theme(hc_theme_db()) %>% 
    hc_exporting(enabled = TRUE)
), yearly = list(
  hchart(subset(curveY[[1]], type == 'Cons'), name= 'Range', type = "columnrange", 
         hcaes(x = as.numeric(m), low = low, high = upp, color = md)) %>% 
    hc_add_series(name="Median", type = 'line', data = subset(curveY[[1]], type == 'Cons'), 
                  hcaes(x = as.numeric(m), y = md), color = 'white') %>%
    hc_yAxis(title =  list(text = 'NDVI')) %>% 
    hc_xAxis(title =  list(text = 'Month')) %>% hc_add_theme(hc_theme_db()) %>% 
    hc_exporting(enabled = TRUE)
  ,
  hchart(subset(curveY[[2]], type == 'Cons'), name= 'Range', type = "columnrange", 
         hcaes(x = as.numeric(m), low = low, high = upp, color = md)) %>% 
    hc_add_series(name="Median", type = 'line', data = subset(curveY[[2]], type == 'Cons'), 
                  hcaes(x = as.numeric(m), y = md), color = 'white') %>%
    hc_yAxis(title =  list(text = 'NDVI')) %>% 
    hc_xAxis(title =  list(text = 'Month')) %>% hc_add_theme(hc_theme_db()) %>% 
    hc_exporting(enabled = TRUE)
),
monthly = list(
  hchart(subset(curveM[[1]], Biom == 'Amazonas' & type == 'Cons'), type = "line", 
       hcaes(x = as.numeric(y), y = md, group = m)) %>% 
  hc_yAxis(title = list(text = "NDVI")) %>% 
  hc_xAxis(title = list(text = "Date")) %>% hc_add_theme(hc_theme_db())%>% 
    hc_exporting(enabled = TRUE)

    ,
  hchart(subset(curveM[[2]], Biom != 'Amazonas' & type == 'Cons'), type = "line", 
         hcaes(x = as.numeric(y), y = md, group = as.numeric(m))) %>% 
    hc_yAxis(title = list(text = "NDVI")) %>% 
    hc_xAxis(title = list(text = "Date")) %>% hc_add_theme(hc_theme_db()) %>% 
    hc_exporting(enabled = TRUE)
))


# hc_curves$lt[[1]]
# hc_curves$lt[[2]]
# 
# hc_curves$yearly[[1]]
# hc_curves$yearly[[2]]
# 
# hc_curves$monthly[[1]]
# hc_curves$monthly[[2]]

ecosNames <- c('Amazonas', 'Llanos')

dayDateRange <- range(range(curveLT[[1]]$D), range(curveLT[[2]]$D))
save(ecosNames, hc_curves, dayDateRange, file = 'curves_highchart.RData')



dispal <- colorFactor("Spectral", domain = biom$ID, na.color = "black")
palleteBiom <- dispal(biom$ID)
unPalBiom <- unique(data.frame(col = palleteBiom, biom = biom$BIOMA, stringsAsFactors = FALSE))
null_hc <- hchart(object = data.frame(Click = NA, Map = 1),
                  type = 'scatter', hcaes(x = Click, y = Map)) %>% hc_title(text = "Select a ecosystem on the map")

save(dispal, palleteBiom, unPalBiom, null_hc, file = 'others.RData')
