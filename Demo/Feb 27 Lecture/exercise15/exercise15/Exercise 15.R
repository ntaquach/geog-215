#-----------------------------
#           Exercise 15 
#-----------------------------

# Load packages
library(sf)
library(tidyverse)
library(leaflet) # package for making interactive maps
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
LSOA<-st_read("Demo/Feb 27 Lecture/LondonHomePrices/LSOA_boundaries/OA_2021_EW_BGC_V2.shp")%>%
  mutate(Code=LSOA21CD)

# NOTE: There are a lot of features here. Plotting is going to be slow. Its 
#       probably a good idea to see if we have data for all of them first. More
#       on this below.

# read in the home prices data
prices<-read.csv("Demo/Feb 27 Lecture/LondonHomePrices/land-registry-house-prices-LSOA.csv")%>%
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
#    Exercise: Part 1 - Join
#----------------------------------------------------------

# Use a join to get the mean home price for each LSOA code

# hint: think about which dataset you want to start with, and which
#       you want to join to it



# once joined, inspect the result for NA's and other issues. Is there anything
#  else that needs to happen to get one average value for each LSOA code?

#----------------------------------------------------------
#   Exercise: Part 2 -  Create histogram
#----------------------------------------------------------

# Make a histogram to inspect the distribution of mean home prices.


#----------------------------------------------------------
#     Make an interactive map
#----------------------------------------------------------


