################################## Exercise 3 codes######################################

getwd() #get current working dir
setwd("C:/Users/Ng Tien Anh Quach/OneDrive - University of North Carolina at Chapel Hill/UNC/Spring 2025/GEOG 215/Exercises") #set working dir to where you save your data

iris <- read.csv("iris.csv") #read your csv file
head(iris) #check header of data
str(iris) #check structure of data

library(tidyverse) #load the package tidyverse

# There are multiple ways of doing this

#Method 1: Use base R
mean(iris$Petal.Length[iris$Species == "versicolor"])

#Method 2: Filter for versicolor and calculate mean 

#EITHER THIS

versi_mean <- iris %>% #specify that we are piping the iris dataset into next step
  filter(Species == "versicolor")%>% #filter for only the versicolor species
mean(versi_mean$Petal.Length) #mean of Petal length in the versi_mean dataset that only contains versicolor species

#OR THIS
versi_mean <- iris %>%
  filter(Species == "versicolor") %>%
  summarise(mean_petal_length = mean(Petal.Length)) #calculate mean Petal length of versicolor
versi_mean

#OR THIS
versi_mean <- iris %>% 
  group_by(Species) %>% #just playing around, group_by is to group the values in the Species column into differing species group
  summarize(mean_petal_length = mean(Petal.Length, na.rm = TRUE)) #similar to above, but the results will be the means of each species as a table.
versi_mean
