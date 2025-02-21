# merge germany counties into one shapefile

library(terra)
library(raster)
# locate directory with files
dir <- "Data/de_shp"

# create file list
ff <- list.files(dir, pattern="\\.shp$", full.names=TRUE)
# read in each one using  raster::shapefile()
x <- lapply(ff, shapefile)

#combine from list into one object
countries <- do.call(rbind, x)

# convert to sf object  and filter only germany
countries_sf<-st_as_sf(countries)%>% 
  filter(str_detect(id, "^DE"))

# plot
ggplot()+
  geom_sf(data=countries_sf)
dev.off()
st_write(countries_sf,"de.shp")

