####### EX 7 ##########

getwd()
library(sf)
library(ggplot2)
library(tidyverse)
library(USA.state.boundaries)
cactus <- st_read("./Data/cactus_shapefile/cactus_shapefile/cactus.shp")
file.exists("./Data/cactus_shapefile/cactus_shapefile/cactus.shp") #check if file exists

states<-state_boundaries_wgs84%>%
  dplyr::filter(NAME!="Alaska")%>%
  dplyr::filter(NAME!="Hawaii")%>%
  dplyr::filter(NAME!="Puerto Rico")%>%
  dplyr::filter(NAME!="U.S. Virgin Islands")

ggplot(cactus) + geom_sf()

ggplot(states) + geom_sf() +
  geom_sf(data=cactus)
#coord_sf to limit

class(cactus$geometry)
