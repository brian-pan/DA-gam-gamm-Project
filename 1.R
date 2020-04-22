# libraries
library(cowplot)
library(tidyverse)
library(mgcv)
library(XML)
library(RCurl)

mathGam = gam(
  MathAch ~ s(SES) + Minority*Sex, 
  data=MathAchieve)
knitr::kable(
  summary(mathGam)$p.table[,1:2], 
  digits=1)

plot(mathGam)

mathGam$sp

## Math, SES-minority interaction
mathGamInt = gam(
  MathAch ~ s(SES, by=Minority) + 
    Minority*Sex, 
  data=MathAchieve)
mathGamInt$sp