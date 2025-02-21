
library(tidyverse)

data(mtcars)

# what does the data look like?
head(mtcars)

# mutate cyl variable to be of class factor
cars<-mtcars%>%
  mutate(cyl=factor(cyl))
class(mtcars$cyl)
class(cars$cyl)


# reclassify mpg to a categorical variable of fuel efficiency
cars_categorical_mpg<-mtcars%>%
  mutate(fuel_eff=case_when(
    mpg>20 ~ "efficient",
    mpg<=20 ~ "not efficient"
  ))


#------------------------------------------------------------------
# How does gas milage vary as a function of engine size?
ggplot(cars)+
  geom_boxplot(aes(x=cyl,y=mpg))

#----------------------------------------------------------------

# Is there a relationship between vehicle weight and gas milage
ggplot(cars)+
  geom_point(aes(x=wt,y=mpg,color=cyl))

# axis labels
ggplot(cars)+
  geom_point(aes(x=wt,y=mpg,color=cyl))+
  xlab("Weight (tons)")+ylab("Gas Milage (mpg)")

# increase size of points
ggplot(cars)+
  geom_point(aes(x=wt,y=mpg,color=cyl),size=4)+
  xlab("Weight (tons)")+ylab("Gas Milage (mpg)")

# modify color scale
# https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
ggplot(cars)+
  geom_point(aes(x=wt,y=mpg,color=cyl),size=4)+
  xlab("Weight (tons)")+ylab("Gas Milage (mpg)")+
  scale_color_brewer(palette="Dark2",name="# of Cylinders")

# add title
ggplot(cars)+
  geom_point(aes(x=wt,y=mpg,color=cyl),size=4)+
  xlab("Weight (tons)")+ylab("Gas Milage (mpg)")+
  scale_color_brewer(palette="Dark2",name="# of Cylinders")+
  ggtitle("Motor Trend Cars")

ggplot(cars)+
  geom_violin(aes(x=cyl,y=mpg))+
  geom_point(aes(x=cyl,y=mpg),position="jitter")

#alternatively
ggplot(cars)+
  geom_violin(aes(x=cyl,y=mpg))+
  geom_jitter(aes(x=cyl,y=mpg),width=0.06)

#----------------------------------------------------------------

# violin plots make more sense with more observations
#----------------------------------------------------------------

ggplot(iris)+
  geom_violin(aes(y=Sepal.Length,x=Species))+
  geom_jitter(aes(y=Sepal.Length,x=Species),width=0.05)


#----------------------------------------------------------------
# Continuous color scale
#----------------------------------------------------------------




#___________________________________________
# example complex plot in base R
#_____________________________________________
data(iris)





#basic plot
plot (iris$Petal.Length,iris$Petal.Width)

# axis labels
plot(iris$Petal.Length, iris$Petal.Width, 
     xlab = "Petal Length",
     ylab = "Petal Width")



##par lets us set global parameters for our graphs.
par(mfrow = c(1,2), mar = c(5,5,4,1))
##Simple plot (left)
plot (iris$Petal.Length,iris$Petal.Width)
##Plot showing species subsets (right)
plot(iris$Petal.Length, iris$Petal.Width, 
     xlab = "Petal Length",
     ylab = "Petal Width",
     main = "Petal Width vs Petal Length",
     pch = 20,
     col=ifelse(iris$Species == "setosa","coral1", 
                ifelse(iris$Species == "virginica","cyan4", 
                       ifelse(iris$Species ==  "versicolor",
                              "darkgoldenrod2", "grey"))))
##legend
legend("bottomright", c("setosa","virginica", "versicolor"),
       col = c("coral1","cyan4", "darkgoldenrod2"), pch=20)




##### EX9##########
library(tidyverse, ggplot2)
cactus <- cactus %>% mutate(chel_density=case_when(
  chelinidea<6  ~ "low",
  chelinidea >= 6 & chelinidea <=12 ~ "moderate",
  chelinidea>12 ~ "high")) %>%
  relocate(chel_density, .after=chelinidea)
library(USA.state.boundaries)
library(sf)
fl <- state_boundaries_wgs84%>%
  dplyr::filter(NAME =="Florida")
ggplot(data=fl) + geom_sf() +
  geom_sf(data=cactus,aes(color=chel_density)) +
  scale_color_manual(labels = c("Low", "Moderate", "High"), values = c("#fed976", "#fc4e2a", "#800026"))+
theme(axis.text.x = element_text(angle=45, vjust=0.5)) +
  coord_sf(xlim=c(-81.9980,-81.9990),ylim=c(29.6974,29.6980))+
  labs(title="", color="Chelinidea Density")

ggplot(data=cactus) + geom_sf()

