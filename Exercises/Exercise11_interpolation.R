#---------------------------------
# load packages
#--------------------------------


library(viridisLite)
library(gridExtra)
library(spData)
library(gstat)
library(sf)
library(stars)
library(tidyverse)
library(tmap)
library(terra)

#----

rm(list=ls())

#-----------------------------
# load data
#--------------------------------

d <- properties #property values in Athens, Greece
d$vble <- d$prpsqm

summary(mod1<-lm(vble~dist_metro,data=d))

#foundation data
athens<-depmunic
###### CAN YOU PLOT THIS OBJECT?
ggplot() + geom_sf(data=athens)

#combine to get municipal boundary
boundary <- st_union(depmunic) %>% st_sf()

###### NOW MAKE A PLOT OF THE PROPERTIES AND THEIR VALUE, WITH THE
      # cITY BOUNDARY

ggplot() + 
  geom_sf(data=athens, fill="white") +
  geom_sf(data=d, mapping = aes(color=vble))+
  scale_color_viridis_c()


#---------------------------------
# build empty grid to store the interpolated values
#--------------------------------
#


grid<-boundary%>%
  st_bbox()%>%
  st_as_stars(dx = 100)%>% #number of cells in x direction
  st_crop(boundary)


#---------------------------------
# IDW
#--------------------------------
### Interpolated the property values using Inverse Distance Weighting
### use the posted code from the Germany example as a guide
# now the interpolation is pretty easy
i <- idw(prpsqm~1, d, grid)

# wait, what did we just do?
?idw

i_V2 <- idw(formula=prpsqm~1, locations=d, newdata=grid)

#are these the same?


# make a plot
idw_plot<- ggplot() + 
  geom_stars(data = i,aes(fill = var1.pred, x = x, y = y)) + 
  scale_fill_viridis_c(name="Price")+
  xlab(NULL) + 
  ylab(NULL) +
  geom_sf(data = st_cast(athens, "MULTILINESTRING")) + 
  geom_sf(data = d)+
  ggtitle("IDW")
idw_plot


###### make a plot of your results








