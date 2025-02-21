# I always start my script by loading any libraries that the script
# requires.
library(tidyverse)
library(sf)
library(USA.state.boundaries)
#Recall that clearing your environment at the top of the script can be helpful
rm(list = ls())

# load the state boundaries
data(state_boundaries_wgs84)

# limit to continental US
states<-state_boundaries_wgs84%>%
  dplyr::filter(NAME!="Alaska")%>%
  dplyr::filter(NAME!="Hawaii")%>%
  dplyr::filter(NAME!="Puerto Rico")%>%
  dplyr::filter(NAME!="U.S. Virgin Islands")

# read in cicada data
cicada <- st_read("Data/cicadas/cicadas_April2024.shp")

# make a map
ggplot(cicada)+geom_sf()

# add states for some geographical context
ggplot(states)+
  geom_sf()+
  geom_sf(data=cicada)

# What if we want to zoom into part of the map window
ggplot(states)+
  geom_sf()+
  geom_sf(data=cicada)+
  coord_sf(xlim=c(-93,-75),ylim=c(32,44))

# few other modifications
ggplot(states)+
  geom_sf()+
  geom_sf(data=cicada)+
  coord_sf(xlim=c(-93,-75),ylim=c(32,44))+
  ggtitle("Brood X Cicada Observations in April 2024")+
  theme_bw()+
  theme(panel.background = element_rect(fill = 'blue'))




