#######EX6

library(tidyverse)
library(sf)
rm(list = ls())


cactus <- read.csv("./Data/cactus.csv")
View(cactus)

library(ggplot2)
cactus %>% ggplot(aes(x=East, y=North)) +
  geom_point(aes(size=Area, color=chelinidea)) +
  scale_color_viridis_c(option = "plasma") 




# load the NC data
nc <- st_read(system.file("shape/nc.shp", package="sf"))

#What type of object is nc?
class(nc)

ggplot(nc)+geom_sf()
