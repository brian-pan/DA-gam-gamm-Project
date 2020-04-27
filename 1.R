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

knitr::kable(
  summary(mathGamInt)$p.table[,1:2], 
  digits=1)

plot(mathGamInt, select=1)
plot(mathGamInt, select=2)

## Math, common smoothing parameter
mathGamIntC = gam(MathAch ~ 
                    s(SES, by=Minority, id=1) + 
                    Minority*Sex, 
                  data=MathAchieve)
mathGamIntC$sp

knitr::kable(
  summary(mathGamIntC)$p.table[,1:2], 
  digits=1)

plot(mathGamIntC, select=1)
plot(mathGamIntC, select=2)

## Math 2d

mathGam2 = gam(
  MathAch ~ s(SES, MEANSES) +
    Minority*Sex, 
  data=MathAchieve)
plot(mathGam2, scheme=2, n2=100)

mTable1 = XML::readHTMLTable(getURL(
  'https://en.wikipedia.org/wiki/List_of_countries_by_infant_mortality_rate'
), stringsAsFactors=FALSE, header=TRUE)
mTable1 = mTable1[[which.max(unlist(lapply(mTable1, nrow)))]]
mTable = mTable1[grep("^([[:digit:]]|[[:space:]])+$|^$|^Country|^World", mTable1[,1], invert=TRUE), ]
mTable = mTable[,c(1,3)]

colnames(mTable)=c('Country','mortality')

iTable = readHTMLTable(getURL(
  'https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita'
), stringsAsFactors=FALSE,header=TRUE)s

iTable = iTable[[5]]
colnames(iTable) = gsub("([[:punct:]]|[[:space:]]).*", "", colnames(iTable))
iTable$Country = gsub("[[:punct:]]", "", iTable$Country)
mTable$Country = gsub("[[:punct:]]", "", mTable$Country)

iTable$Country = gsub("ô", "o", iTable$Country)
iTable$Country = gsub("ã", "a", iTable$Country)
iTable$Country = gsub("é", "e", iTable$Country)
iTable$Country = gsub("í", "i", iTable$Country)

iMort = merge(iTable, mTable, by='Country')

ineqTable = readHTMLTable(getURL(
  'https://en.wikipedia.org/wiki/List_of_countries_by_income_equality'
), skip.rows=1:2, stringsAsFactors=FALSE)
ineqTable = ineqTable[[1]][-1,]
ineqTable = ineqTable[,c(1,4)]