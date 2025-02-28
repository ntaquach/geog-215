

# Load packages
library(sf)
library(tidyverse)
library(leaflet)
library(viridisLite)
library(scales)
#clear environment
rm(list=ls())



#----------------------------------------------------------
#     read in the data
#----------------------------------------------------------


# load the LSOA boundaries (UK version of census blocks)

# NOTE: the join field is called "Code" in the prices data below, so we'll also
#modify these LSOA boundaries to match
LSOA<-st_read("Data/LondonHomePrices/LSOA_boundaries/OA_2021_EW_BGC_V2.shp")%>%
  mutate(Code=LSOA21CD)

# NOTE: There are a lot of features here. Plotting is going to be slow. Its 
#       probably a good idea to see if we have data for all of them first. More
#       on this below.

# read in the home prices data
prices<-read.csv("Data/LondonHomePrices/land-registry-house-prices-LSOA.csv")%>%
  mutate(Value=na_if(Value,":"))%>%  #change colon to NA
  na.omit()%>%                       #now get rid of those
  mutate(Year_num=str_extract_all(Year, '\\d+'), # extract the year and make numeric
         Year_num = map_dbl(Year_num, ~as.numeric(.x)))

head(LSOA)
head(prices)

#----------------------------------------------------------
#     explore data
#----------------------------------------------------------
# Before joining, lets explore the prices data a bit to make sure we join correctly
Code_per_year<-prices%>%
  group_by(Year_num)%>%
  summarize(n_distinct(Code))
           # each year there are 4835 LSOA codes, so we'll only need 4,835 
           # of the ~200,000 boundaries in the LSOA boundaries sf object.

# Next, let's extract the data for 2017 and then average by code
prices_2017<-prices%>%
  filter(Year_num==2017)%>%
  filter(Measure=="Median")%>% #other metrics like the number of sales are included, so let's filter to just median home price
  mutate(Value=as.numeric(Value))%>%
  group_by(Code)%>%
  summarize(MeanHomePrice=mean(Value))
      # note, we don't have data for ~100 or so LSOA codes, because our object now only
      # has 4,713 rows


#----------------------------------------------------------
#     Join
#----------------------------------------------------------

# We'll start with the boundaries, and join the prices data to them
LSOA_with_prices<-LSOA%>%
  left_join(prices_2017,by="Code")%>%
  drop_na(MeanHomePrice)%>% # get ride of LSOA's that didn't have home prices
  group_by(Code)%>%
  summarize(MeanHomePrice=mean(MeanHomePrice)) #looks good!!

#----------------------------------------------------------
#     Summaries
#----------------------------------------------------------

min_mean_price<-min(LSOA_with_prices$MeanHomePrice)
max_mean_price<-max(LSOA_with_prices$MeanHomePrice)

#histogram, but first, turn off scientific notation
   # options(scipen=999)  # delete first "#" to turn of scientific notation
ggplot()+geom_histogram(data=LSOA_with_prices,aes(x=MeanHomePrice))

# make axes ticks more readable
ggplot()+geom_histogram(data=LSOA_with_prices,aes(x=MeanHomePrice))+
  scale_y_continuous(labels=comma)+
  scale_x_continuous(labels=comma)
#----------------------------------------------------------
#     Map
#----------------------------------------------------------
st_crs(LSOA_with_prices)
st_graticule(LSOA_with_prices)
st_bbox(LSOA_with_prices)

# first plot
ggplot()+
  geom_sf(data=LSOA_with_prices,aes(fill=MeanHomePrice))+
  coord_sf(crs = 27700)

# specify max/min and transform color scale
ggplot()+
  geom_sf(data=LSOA_with_prices,aes(fill=MeanHomePrice))+
  scale_fill_gradient(low=min_mean_price,high=max_mean_price,trans="log")+
  coord_sf(crs = 27700)

# different color scale
ggplot()+
  geom_sf(data=LSOA_with_prices,aes(fill=MeanHomePrice))+
  scale_fill_viridis_c(name="Average Median Home Price",trans="log",labels=comma)+
  coord_sf(crs = 27700)



#----------------------------------------------------------
#     Basic Interactive Map
#----------------------------------------------------------

#first reproject to WGS84
LSOA_WGS84<-st_transform(LSOA_with_prices, crs = 4326)

# one color first
m <- leaflet()%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addPolygons(data=LSOA_WGS84,
              stroke=T,
              weight=0.5,
              color="#37B1ED",
              opacity=1)
m # cool!


#----------------------------------------------------------
#     Interactive Chloropleth Map
#----------------------------------------------------------

# Now to make a chloropleth, we need to define colors manually

LSOABins <- c(0,400000,600000,1000000,2000000,5000000,10000000)
LSOAPal<- colorBin("YlGnBu",domain=LSOA_WGS84$MeanHomePrice,bins=LSOABins)

m2 <- leaflet()%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addPolygons(data=LSOA_WGS84,
              stroke=T,
              weight=0.5,
              color="#37B1ED",
              fillColor= ~LSOAPal(MeanHomePrice),
              opacity=1,
              fillOpacity=0.8)
# Add legend
m3 <- leaflet()%>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  addPolygons(data=LSOA_WGS84,
              stroke=T,
              weight=0.5,
              color="#37B1ED",
              fillColor= ~LSOAPal(MeanHomePrice),
              opacity=1,
              fillOpacity=0.8)%>% 
  addLegend("bottomright",opacity = 1,
            colors =c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84"),
            title = "Average House Price</br>2017, all house sales",
            labels= c("<£400,000","£400,000 - £599,999","£600,000 - £799,999","£800,000 - £999,999","£1 - 2 million", "£2 - 5 million", "£5 - 9 million")
  )
