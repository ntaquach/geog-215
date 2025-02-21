
# The script below applies 2 different interpolation methods to estimate 
# nitrogen oxide pollution concentrations across Germany from 74 air quality
# measurement stations

#---------------------------------
# load packages
#--------------------------------


library(viridisLite)
library(gridExtra)
library(spData)
library(gstat)
library(sf)
library(stars)
library(tidyverse)|>
  suppressPackageStartupMessages()


rm(list=ls())


#---------------------------------
# load data
#--------------------------------
# Load the NO2 data from gstat

no2 <- read_csv("Data/no2.csv")
#NOTE: This is in .csv format (not spatial!) so we need to 
# define crs for NO2 data and identify coordinates

#store the desired coordinate reference system in an object
crs <- st_crs("EPSG:32632")
crs2 <- st_crs(32632)

# NOTE: st_crs() accepts either the EPSG code as a number, or a character string

# To make the non-spatial property values into a spatial object,
#  we need to provide the coordinates AND the coordinate reference system
no2.sf<-no2%>%
  st_as_sf(crs = "OGC:CRS84", 
         coords = c("station_longitude_deg", "station_latitude_deg"))%>%
  st_transform(crs) 

# Why do no2 and no2.sf have different dimensions?

#---------------------------------


#Foundation Data - Germany Boundary
de<-read_sf("Data/de_shp/de.shp")%>%st_transform(crs)

#---------------------------------
# quick plot
#--------------------------------

# Plot the point data
data_plot <- ggplot() + 
  geom_sf(data = de) + #foundation data first
  geom_sf(data = no2.sf, mapping = aes(col = NO2)) #then add no2
data_plot

# Plot the point data
data_plot <- ggplot() + 
  geom_sf(data = de,fill="white") + #foundation data first
  geom_sf(data = no2.sf, mapping = aes(col = NO2))+
  scale_color_viridis_c()
data_plot
#---------------------------------
# One last step
#--------------------------------

# Before we interpolate, we need to create a blank raster over which to interpolate
grd<-de%>%
  st_bbox()%>%  # match the extent of the Germany boundary
  st_as_stars(dx = 10000)%>%  # create an empty raster with 10000 cells in x direction
  st_crop(de) # crop to the Germany boundary
grd

#---------------------------------
# Inverse Distance Weighting
#--------------------------------

# now the interpolation is pretty easy
i <- idw(NO2~1, no2.sf, grd)

# wait, what did we just do?
?idw

i_V2 <- idw(formula=NO2~1, locations=no2.sf, newdata=grd)

#are these the same?


# make a plot
idw_plot<- ggplot() + 
  geom_stars(data = i,aes(fill = var1.pred, x = x, y = y)) + 
  scale_fill_viridis_c(name="NO2")+
  xlab(NULL) + 
  ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf)+
  ggtitle("IDW")
idw_plot

#---------------------------------
# Kriging
#--------------------------------

# for Kriging, we need a variogram
v <- variogram(NO2~1, no2.sf)
v0 <- variogram(NO2~1, no2.sf, cutoff = 100000, width = 10000)
# now fit a model to that variogram
v.m <- fit.variogram(v, vgm(1, "Exp", 50000, 1))

# interpolate, just as with IDW, but with additional argument for the variogram
k <- krige(NO2~1, no2.sf, grd, v.m)


krige_plot<-ggplot() + 
  geom_stars(data = k, aes(fill = var1.pred, x = x, y = y)) + 
  scale_fill_viridis_c(name="NO2") +
  xlab(NULL) + 
  ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf) +
  coord_sf(lims_method = "geometry_bbox") +
  ggtitle("Kriging")
krige_plot
#---------------------------------
# Arrange Multiple Plots into one figure
#--------------------------------

grid.arrange(data_plot,idw_plot,krige_plot,nrow=1)



