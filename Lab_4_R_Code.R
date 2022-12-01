#' ---
#' title: "Lab 4 Code"
#' author: "Will Dowling"
#' date: "9 November 2022"
#' ---

setwd("/Users/Will/Desktop/QMSS/Data Analysis/Lab 4")

# Uploading libraries
library(dplyr)
library(psych)
library(tidyverse)
library(gmodels)

## 1. Run a simple regression, with at least two Xs in it, and interpret your results. (Did the results fit your expectations? Why? Why not?)

# Uploading 2017-2022 World Values Survey and inspecting the following variables:
# dem.country: How democratically is this country being governed today? Again using a scale from 1 to 10, where 1 means that it is “not at all democratic” and 10 means that it is “completely democratic,” what position would you choose?
# human.rights: How much respect is there for individual human rights nowadays in this country? Scale: 1 (high) to 4 (low)
# gov.sat:  On a scale from 1 to 10 where “1” is “not satisfied at all” and “10” is “completely satisfied”, how satisfied are you with how the political system is functioning in your country these days?
# lib.cons: In political matters, people talk of "the left" and "the right." How would you place your views on this scale, generally speaking? Scale: 1 (left) to 10 (right)
# dem.favor: How important is it for you to live in a country that is governed democratically? On this scale where 1 means it is “not at all important” and 10 means “absolutely important” what position would you choose?
# urbanicity: Code size of town where interview was conducted (scale of 1-8 ascending order)
# corruption: prevalence of corruption in country (scale of 1 to 10 in ascending order)
wvs = read.csv('~/Desktop/QMSS/Data Analysis/Lab 4/WVS_Cross-National_Wave_7_csv_v4_0.csv') 
wvs = rename(wvs, c("dem.country"="Q251")) 
table(wvs$dem.country)
wvs = rename(wvs, c("gov.sat"="Q252"))
table(wvs$gov.sat)
wvs = rename(wvs, c("income"="Q288"))
wvs = rename(wvs, c("lib.cons"="Q240"))
wvs = rename(wvs, c("human.rights"="Q253"))
wvs = rename(wvs, c("corruption"="Q112"))
wvs = rename(wvs, c("dem.favor"="Q250"))
wvs = rename(wvs, c("urbanicity"="G_TOWNSIZE"))
wvs = rename(wvs, c("country"="B_COUNTRY"))

# Does the level of corruption in a country impact one's satisfaction with the political system, controlling for income, democracy, and respect for human rights
lm1 = lm(gov.sat ~ corruption + dem.country + dem.favor + human.rights + urbanicity, data=wvs)
summary(lm1)

## 2. Add an interaction term to that model that you think might moderate the original relationship between X1 and X2. Explain why you think an interaction might be present and in what direction it would work. Explain your results. Did it work out? Yes? No?
# Add in interaction for urbanicity level that may further explain corruption. Those who live in more highly populated areas may experience more corruption than those in more rural districts.
table(wvs$urbanicity)
wvs$urban.new = ifelse((wvs$urbanicity>4), 1, 0)
table(wvs$urban.new)

#subsetting data for urbanicity. Urban respondents counted in wvs.u. Rural respondents counted in wvs.o.
wvs.u = subset(wvs, urban.new == 1)
wvs.o = subset(wvs, urban.new == 0)

#Creating linear models for both groups
lm2 = lm(gov.sat ~ corruption + dem.country + dem.favor + human.rights, data=wvs.u)
summary(lm2)

lm3 = lm(gov.sat ~ corruption + dem.country + dem.favor + human.rights, data=wvs.o)
summary(lm3)

#Creating linear model that includes interaction
#It would appear that urban residents do feel that corruption is more of an influence on their favorability toward democracy. 
lm4 = lm(gov.sat ~ corruption:as.factor(urban.new) + dem.country + dem.favor + human.rights + urbanicity, data=wvs)
summary(lm4)





