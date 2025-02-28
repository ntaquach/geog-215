
library(tidyverse)
library(sf)
library(GSODR)
# clear environment 
rm(list = ls())


# Read in data
hippos_raw<-st_read("Data/hippos/hippos.shp")
rivers_raw<-st_read("Data/hippos/river_segments.shp")



#buffer rivers by 1000km
rivers_buffered<-rivers_raw%>%
  st_buffer(dist=1000,endCapStyle="FLAT")


hippos_in_segments<-st_join(hippos_raw,rivers_buffered,join=st_within)%>%
  group_by(seg_id)%>%
  summarize(individuals=sum(Total))%>%
  arrange(seg_id)

ggplot()+
  geom_histogram(data=hippos_in_segments,aes(x=individuals))




############# EXTRA CODES#############
#======================================
hippos_in_segments2<-st_join(rivers_buffered,hippos_raw,join=st_contains)%>%
  group_by(seg_id)%>%
  summarize(individuals=sum(Total,na.rm=T))%>%
  arrange(seg_id)


check<-left_join(as.data.frame(hippos_in_segments2),
                 as.data.frame(hippos_in_segments),
                               by="seg_id")





ggplot(hippos_in_segments)+
  geom_histogram(aes(x=individuals), bins=40)


Hippos="Number of Hippos"

ggplot()+
  geom_density(data=hippos_in_segments,aes(x=individuals))+
  xlab(Hippos)
ggplot()+
  geom_density(data=hippos_in_segments,aes(x=individuals),color="red")+
  xlab("Number of Hippos")



