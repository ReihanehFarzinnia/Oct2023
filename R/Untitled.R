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
ggplot(gapminder_euro2007, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(col = "red")

#Questions
#1.What sort of "model" might fit the relationship between life expectancy and GDP
#   per capita?
#2.Does the pattern look the same for countries in other continents, e.g. Asia? (Find the outlier)
#3.Does the pattern look the same for years in the mid-late-20th century? 


# Question 1 --------------------------------------------------------------
# A line?
# We can ask ggplot to add in a line, based on a linear model (lm)
ggplot(gapminder_euro2007, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(col = "red") +
  geom_smooth(method = lm, formula = y ~ x)
  
# Question 2 --------------------------------------------------------------
#let's try Asia
gapminder_asia2007 <- gapminder %>%
  filter(continent == "Asia" & year == 2007) %>%
  mutate(pop_e6 = pop / 1000000)

ggplot(gapminder_asia2007, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(col = "red") +
  geom_smooth(method = lm, formula = y ~ x)

#Find the outlier 
#In R, you can identify outliers in a linear model by examining the residuals, 
#which represent the differences between the observed values and the predicted 
#values from your linear model. Outliers are typically data points associated 
#with large or extreme residuals. 
#Extract residuals from your model
model <- lm(formula = gdpPercap ~ lifeExp, data = gapminder_asia2007)
residuals <- resid(model)

#Standardized Residuals:Calculate standardized residuals and consider values 
#that are much larger or smaller than the typical range (e.g., beyond ±2 or ±3 
#standard deviations) as potential outliers.
std_residuals <- rstandard(model)
outliers <- abs(std_residuals) > 2  # You can adjust the threshold as needed

# Question 3 --------------------------------------------------------------
# First we have to define what we mean by the "mid-late 20th century. I will
# define this as years between 1950 and 2000, inclusive:
# We'll filter the data
# Europe
gapminder_euro1950_2000 <- gapminder  %>%
  filter(continent == "Europe" & (year >=  1950 & year <= 2000)) %>%
  mutate(pop_e6 = pop / 1000000) 

ggplot(gapminder_euro1950_2000, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(col ="red") +
  geom_smooth(method = lm, formula = y ~ x)

# At middle gdps a straight line seems to fit very well, but at very low and 
# very high gdpPercap, the data do not seem to be well described by a 
# straight line

# Asia 
gapminder_asia1950_2000 <- gapminder  %>%
  filter(continent == "Asia" & (year >=  1950 & year <= 2000)) %>%
  mutate(pop_e6 = pop / 1000000) 

ggplot(gapminder_asia1950_2000, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(col ="red") +
  geom_smooth(method = lm, formula = y ~ x)

# A straight line does not describe these data very well. 
# The slope of the line looks like it is being influenced a lot
#  by a small number of points that have much higher gdps than 
#  the rest. 



# -------------------------------------------------------------------------
#Add the regression line
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  geom_smooth(method=lm)

#Remove the confidence interval
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp)) + geom_point() + 
  geom_smooth(method=lm, se=FALSE)

#Loess method - Fitting quadratic model 
