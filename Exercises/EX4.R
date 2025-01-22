library(tidyverse)

# Load the cars data
data(mtcars)

# Base R
plot(mpg~cyl,data=mtcars)

plot(mpg~factor(cyl),data=mtcars)

#GGplot2
ggplot(mtcars)+
  geom_boxplot(aes(x=factor(cyl),y=mpg))

class(mtcars$cyl)

#make plot within a pipe
mtcars%>%
  mutate(cyl_factor=factor(cyl))%>%
  ggplot()+
  geom_boxplot(aes(x=cyl_factor,y=mpg))

#change axis labels
ggplot(mtcars)+
  geom_point(aes(x=mpg,y=wt))+
  labs(x="Number of Cylinders", y="Miles/Gallon (mpg)")

# export the file
write.csv(mtcars,"mtcars.csv")



###############FOR CLASS EXERCISE ON IRIS DATA#########################
setwd("C:/GitHub Projects/geog-215")
iris <- read.csv("./Data/iris.csv") #relative file path for future reference
head(iris) #checking the header

###ggplot2 
library(tidyverse) #load tidyverse package
iris_boxplot <- iris %>% ggplot(aes(x=Species, y=Sepal.Length)) + # making boxplot
  geom_boxplot() #there is a slight difference in the placement of aes function between my codes and Dr. Taillie's codes
#And it is okay, aes can go to either place.
iris_boxplot

iris_scatter <- iris %>% ggplot(aes(x=Petal.Length, y=Petal.Width)) + #making scatterplot
  geom_point() + theme_minimal()
iris_scatter

ggsave("Scatterplot IRIS.png",iris_scatter,path="Plots", width=7, height=7, bg="white") #save the scatterplot as an image file
#there is an argument called path, it basically tells R to save my image file to the Plots folder in the geog-215 folder.
