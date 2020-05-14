#install.package(gamm4)

library(devtools)
library(mgcv)
library(gamm4)
library(tidyverse)

# Load nCOVID-19 data
covid_data <- read_csv("covid_data.csv")