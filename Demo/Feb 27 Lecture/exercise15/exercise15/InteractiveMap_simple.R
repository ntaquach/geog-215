library(tidyverse)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)
library(sf)


#quick interactive map
r_birthplace_map <- leaflet() %>%
  addTiles() %>%  # use the default base map which is OpenStreetMap tiles
  addMarkers(lng=174.768, lat=-36.852,
             popup="The birthplace of R")
r_birthplace_map




# Read a Geojson or shapefile
data_map <- st_read("Data/Chapel_Hill_Transit_Bus_Stops/Chapel_Hill_Transit_Bus_Stops.shp")
st_crs(data_map)

# Transform to leaflet projection if needed
data_map <- st_transform(data_map, crs = '+proj=longlat +datum=WGS84')

leaflet() %>%
  addTiles() %>%
  setView(lng = -79.0582, lat = 35.91248, zoom = 14) %>%
  addMarkers(data = data_map)




