library(leaflet)
library(tidyverse)
library(htmlwidgets)
library(htmltools)
library(sp)
library(maps)
library(maptools)
library(leaflet)
library(plyr)
library(XML)

# make sure to use the latest maps package
# it was recently updated at the time of the answer

world <- map("world", fill=TRUE, plot=FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), 
                                      FALSE)

setwd("/Users/zhengze/Desktop/Georgetown Course/ANLY 503/Take Home Final Individual")
options(digits=4)

data<- read.csv("choropleth_data.csv")

cnt <- levels(as.factor(data$nationality))
data$country = data$nationality

target <- subset(world_map, country %in% cnt)
countmap <- merge(target, data, by=c("country"))


labels <- sprintf(
  "<strong>Country: </strong> %s<br/><strong>Number of Players born here: </strong> %g",
  countmap$country,countmap$count
) %>% lapply(htmltools::HTML)


# Format popup data for leaflet map.
popup_11 <- paste0("<strong>Country: </strong>", 
                   data$country, 
                   "<br><strong>The most popular position: </strong>", 
                   data$First, 
                   "<br><strong>The second popular position: </strong>", 
                   data$Second, 
                   "<br><strong>The third popular position: </strong>", 
                   data$third)

popup_22 <- paste0("<strong>Country: </strong>", 
                   data$country, 
                   "<br><strong>Average Score: </strong>", 
                   data$ave_score, 
                   "<br><strong>Average height in cm: </strong>", 
                   data$ave_height, 
                   "<br><strong>Average weight in kg: </strong>", 
                   data$ave_weight)


#pal <- colorQuantile("YlOrRd", domain=data$count, n = 9)
bins <- c(0,50,150,300,450,600,900,1200,1650)
pal <- colorBin("YlOrRd", domain = data$count, bins = bins)


icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor="black",
  markerColor='yellow',
  library = 'ion'
)

gmap <- leaflet(countmap) %>%
  # Base groups
  addTiles() %>%
  setView(lng = -20, lat = 40, zoom = 1) %>% 
  addPolygons(fillColor = ~pal(count), 
              fillOpacity = 1, 
              highlight = highlightOptions(
                color = "blue",
                opacity=1,
                weight=2,
                fillOpacity = 2,bringToFront = TRUE,sendToBack = TRUE),
              label=labels,
              labelOptions= labelOptions(direction = 'auto'),
              color = "#000000", 
              weight = 1,
              #popup =popup_dat,
              group = "Distribution of Soccer Players in the World"
  ) %>% 
  addMarkers(data=data,lat=~lat, lng=~long, popup=popup_22, group = "Average information with the Nationality") %>% 
  addAwesomeMarkers(data=data,lat=~lat, lng=~long,icon=icons,popup=popup_11,group="Top 3 Popular position")%>%
  
  # Layers control
  addLayersControl(
    baseGroups = c("Distribution of Soccer Players in the World"),
    overlayGroups = c("Top 3 Popular position","Average information with the Nationality"),
    options = layersControlOptions(collapsed = FALSE)
  )%>% 
  
  addLegend(pal = pal, values = ~count, opacity = 0.7, title = NULL,
            position = "bottomright")


gmap
saveWidget(gmap, 'Distribution of Soccer Players in the World.html', selfcontained = TRUE)

