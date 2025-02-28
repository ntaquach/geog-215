#### Exercise 14 ########

library(sf)
library(tidyverse)
library(stars)
library(ggplot2)

hippos <- st_read("Data/hippos/hippos.shp")
river <- st_read("Data/hippos/river_segments.shp")
plot(hippos)

table(river$RIVERNAME)

river_buffered <- st_buffer(river, dist = 1000)


hippo_by_river <- river_buffered %>% st_join(hippos, join=st_contains) %>%
  group_by(seg_id) %>%
  summarise(num_hippos = sum(Total, na.rm = T)) %>%
  arrange(desc(seg_id))

ggplot() + geom_histogram(data=hippo_by_river, aes(x=num_hippos))

ggplot()+
  geom_sf(data=hippo_by_river,aes(color=num_hippos))+
  scale_color_viridis_c()
