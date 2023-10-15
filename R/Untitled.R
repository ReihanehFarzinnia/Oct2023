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







#Add the regression line
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point() + 
  geom_smooth(method=lm)

#Remove the confidence interval
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp)) + geom_point() + 
  geom_smooth(method=lm, se=FALSE)

#Loess method - Fitting quadratic model 
