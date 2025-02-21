#### EX12 #######

library(sf)
library(tidyverse)
library(nycflights23)

airlines <- airlines
airports <- airports
planes <- planes
flights <- flights

num_flights<- flights %>% group_by(origin) %>%
  summarise(num_flights=n())%>%
  rename(faa=origin) %>%
  left_join(airports, by="faa") %>%
  st_as_sf(coords=c("lon", "lat"), crs = 4326)

ggplot() + geom_sf(data=num_flights, aes(color=num_flights)) 


table(filtered_flights$carrier)
# 2) Which manufacturer's planes were most used for flights departing NYC airports?

flight_plane <- flights %>% left_join(planes, by = "tailnum")

flight_plane %>% group_by(manufacturer) %>%
  summarise(tol_flights=n()) %>%
  arrange(desc(tol_flights))

 # 1) How many airplane manufacturers had at least one of their planes depart from one of the 3 NYC airports in 2023?
#   
temp <- flights$origin %in% airports$faa
temp_1 <- airports$faa %in% flights$origin
table(temp)

flight_plane %>% group_by(manufacturer) %>%
  summarise(tol_flights=n()) %>%
  arrange(desc(tol_flights)) %>%
  summarise(num_manufacturer = n()) # number of rows in the tol_flights dataset = number of manufacturers


# 3) Are there any flights for which the plane is NOT represented in the planes data?

#Yes!