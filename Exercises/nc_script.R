


# I always start my script by loading any libraries that the script
# requires.
library(tidyverse)
library(sf)

#Recall that clearing your environment at the top of the script can be helpful
rm(list = ls())

# load the NC data
nc <- st_read(system.file("shape/nc.shp", package="sf"))

#What type of object is nc?
class(nc)

ggplot(nc)+geom_sf()
