
library(sf)
library(stars)
library(tidyverse)


# read in the DEM
lidar_dem <- read_stars("Data/earthanalyticswk3/BLDR_LeeHill/pre-flood/lidar/pre_DTM.tif")

# test plot just to make sure it looks right
plot(lidar_dem,
     main = "Digital Elevation Model")


# What is the coordinate reference system?
st_crs(lidar_dem)


st_bbox(lidar_dem)


#-------------------------------------------------------
#  Histograms
#-------------------------------------------------------

#Note, we're using {base} here because we're just making some plots
#to explore our data, not to communicate ideas, patters, or results.
#

hist(lidar_dem,
     main = "Distribution of surface elevation values",
     xlab = "Elevation (meters)", ylab = "Frequency",
     col = "springgreen")

options(scipen = 999)

# plot histogram
hist(lidar_dem,
     breaks = 3,
     main = "Distribution of surface elevation values with breaks",
     xlab = "Elevation (meters)", ylab = "Frequency",
     col = "springgreen")
summary(lidar_dem)
#-------------------------------------------------------
#  Canopy height model
#-------------------------------------------------------


# read in the DSM
lidar_dsm <- read_stars("Data/earthanalyticswk3/BLDR_LeeHill/pre-flood/lidar/pre_DSM.tif")

lidar_chm <- lidar_dsm - lidar_dem
st_crs(lidar_chm)
plot(lidar_chm,
     main = "Lidar Canopy Height Model (CHM)")

plot(lidar_chm,
     breaks = c(0, 2, 10, 20, 30),
     main = "Lidar Canopy Height Model",
     col = c("white", "brown", "springgreen", "darkgreen"))

# values range from 0 to ~27m


#-------------------------------------------------------
#  Reclassify Raster
#-------------------------------------------------------

# Create a reclassification table
rcl <- data.frame(from = c(0,2,4,7), to = c(2,4,7,Inf), becomes = c(NA,1,2,3))

# reclassify the canopy height model
chm_reclassified<-cut(lidar_chm, c(0, rcl$to), labels = rcl$becomes)


chm_colors <- c("palegoldenrod", "palegreen2", "palegreen4","white")

#plot reclassified canopy height model
canopy <- ggplot()+
  geom_stars(data=chm_reclassified,aes(fill=pre_DSM.tif),na.action=na.omit)+
  coord_sf(crs=sf::st_crs(32613))+
  scale_fill_manual(values=chm_colors,
                    labels=c("Short", "Medium", "Tall"),
                    name="Canopy Height")
canopy
### add building

lidar_bldg <- st_read("Data/earthanalyticswk3/Building_Footprints/Building_Footprints.shp")

canopy + geom_sf(data=lidar_bldg, fill="red") +
  coord_sf(crs=st_crs(32613), xlim =c(472000,476000), ylim=c(4434000, 4436000))

