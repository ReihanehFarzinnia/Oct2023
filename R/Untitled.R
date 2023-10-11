#test
install.packages("tidyverse")
install.packages("gapminder")
install.packages("ggplot2")

library(tidyverse)
library(gapminder)
library(ggplot2)

head(gapminder)

#We'll filter the data and just use some of it
gapminder_euro2007 <- gapminder %>%
  filter(continent == "Europe" & year == 2007) %>%
  mutate(pop_e6 = pop / 1000000)
  
#Now we can use ggplot plot the data:population with life expectancy 
ggplot(gapminder_euro2007, aes(x = pop_e6, y = lifeExp)) + 
  geom_point(col = "red")

#Your task
#Use ggplot to plot life expectancy with gdpPrecap (GDP per capita)

#Questions
#1.What sort of "model" might fit the relationship between life expectancy and GDP
#   per capita?
#2.Does the pattern look the same for countries in other continents, e.g. Asia?
#3.Does the pattern look the same for years in the mid-late-20th century? 

# Question 1 --------------------------------------------------------------
#
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp)) + geom_point(size=2, shape=23)

#Add the regression line
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  geom_smooth(method=lm)

#Remove the confidence interval
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp)) + geom_point() + 
  geom_smooth(method=lm, se=FALSE)

#Loess method - Fitting quadratic model 

# Question 2 --------------------------------------------------------------

