#install.package(gamm4)

library(devtools)
library(mgcv)
library(gamm4)
library(tidyverse)

# Load nCOVID-19 data
covid_data <- read_csv("covid_data.csv")

# Plot over time
covid_data %>% 
  filter(country_region %in% c('Hubei','Italy','Iran','South Korea','USA')) %>% 
  na.omit() %>% 
  ggplot(aes(time, dead, color=country_region)) +
  geom_point() +
  theme_minimal()

# Plot from initial death in region
covid_data %>% 
  filter(country_region %in% c('Hubei','Italy','Iran','South Korea','USA')) %>% 
  na.omit() %>% 
  ggplot(aes(timeInt, dead, color=country_region)) +
  geom_point() +
  theme_minimal()

# Fit a GAM with `dead` as the response a smooth on `timeInt` and `country_region` as covariate.
# In the smooth, use `pc=0`, which indicates a *point constraint*. The smooth will pass through 0 at this point.
resGam= mgcv::gam(
  dead ~ s(timeInt, pc=0) + country_region, 
  data=covid_data, 
  family=poisson(link='log'))

summary(resGam)
coef(resGam)
plot(resGam)

# This model will result in an error.
resGam2= mgcv::gam(
  dead ~ s(timeInt, k=150, pc=0) + country_region, 
  data=covid_data, 
  family=poisson(link='log'))

resGam3= mgcv::gam(
  dead ~ s(timeInt, k=50, pc=0) + country_region, data=covid_data, 
  family=poisson(link='log'), method='ML')
plot(resGam3)
gam.check(resGam3)

resGam4 = mgcv::gam(
  dead ~ s(timeInt, k=20, pc=0) + country_region, data=covid_data, 
  family=poisson(link='log'), method='ML')s