### Assignment 1 ######

getwd()
kind <- read.csv("./Assignments/Assignment 1/CA-Kindergarten-Data-1.csv")
str(kind)
View(kind)

library(tidyverse)
kind[3,8] <- NA
kind[11,8] <- NA
kind <- kind %>% filter(!is.na(UPTODATE)) %>%
  mutate(VaxRate = as.numeric(UPTODATE)/ENROLLMENT)

kind_city <- kind %>% group_by(CITY) %>%
  summarise(avg_rate = mean(VaxRate))
kind_city
kind_city$CITY[which.min(kind_city$avg_rate)]
