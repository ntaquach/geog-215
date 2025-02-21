##### Assignment 2 #########


library(sf)
library(ggplot2)

ch_stream <- read.csv("./Data/ChapelHillStreams.csv")
ch_stream

library(dplyr)
ch_stream_filtered <- ch_stream %>% filter(FeatureType == "STREAM") %>% 
  group_by(Name) %>% summarise(sum_length=sum(SHAPESTLength, na.rm = T)) %>%
  arrange(desc(sum_length)) %>% 
  slice_head(n=10) 

ch_stream_filtered %>% ggplot(aes(x = reorder(Name, sum_length), y = sum_length)) + 
  geom_col() + theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=0.5))

