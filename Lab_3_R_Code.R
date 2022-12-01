#' ---
#' title: "Lab 3 Code"
#' author: "Will Dowling"
#' date: "3 November 2022"
#' ---

setwd("/Users/Will/Desktop/QMSS/Data Analysis/Lab 3")

# Uploading libraries
library(dplyr)
library(psych)
library(tidyverse)
library(gmodels)


## 1. Run a simple bivariate regression, and interpret your results. (Did the results fit your expectations? 
## Why? Why not?)

# Uploading 2020 ANES cumulative data file
anes = read.csv("~/Desktop/QMSS/Data Analysis/Lab 4/WVS_Cross-National_Wave_7_csv_v4_0.csv") 

# Observing and cleaning dependent variable: V202440 which indicates satisfaction with US democracy (more detail below).Ratings are indexed on a 1 (high) to 5 (low) scale. 
# Cleaning data 
table(anes$V202440)
anes["V202440"][anes["V202440"] == -9] <- NA
anes["V202440"][anes["V202440"] == -8] <- NA
anes["V202440"][anes["V202440"] == -7] <- NA
anes["V202440"][anes["V202440"] == -6] <- NA
anes["V202440"][anes["V202440"] == -5] <- NA
table(anes$V202440)

# Renaming and cleaning dependent and independent variables.
# V202440: On the whole, are you very satisfied, fairly satisfied, not very satisfied, or not at all satisfied with the way democracy works in the United States? 
# Recoded this in new 'democracy.sat.new' variable to be 1(low) to 5(high) for easier interpretation, given scales of variables below.
# V201352: How much do you trust the officials who oversee elections where you live? Scale: 1(low) to 5(high)
# V202023: How many days in the past week did you talk about politics with family or friends? Scale: 0(low) to 7(high)
# V201617x: Total (family) income. Scale: 1(low) to 22(high)
# V201233: How often can you trust the federal government in Washington to do what is right? Recoded scale in new 'gov.trust.new' variable: 1(low) to 5(high)
names(anes)[names(anes) == 'V202440'] <- 'democracy.sat'
anes$democracy.sat.new[anes$democracy.sat==1 ] <- 5
anes$democracy.sat.new[anes$democracy.sat==2 ] <- 4
anes$democracy.sat.new[anes$democracy.sat==4 ] <- 2
anes$democracy.sat.new[anes$democracy.sat==5 ] <- 1

names(anes)[names(anes) == 'V202023'] <- 'politics.talk'
anes["politics.talk"][anes["politics.talk"] == -9] <- NA
anes["politics.talk"][anes["politics.talk"] == -7] <- NA
anes["politics.talk"][anes["politics.talk"] == -6] <- NA
anes["politics.talk"][anes["politics.talk"] == -1] <- NA

names(anes)[names(anes) == 'V201617x'] <- 'income'
anes["income"][anes["income"] == -9] <- NA
anes["income"][anes["income"] == -5] <- NA

names(anes)[names(anes) == 'V201352'] <- 'election.trust'
anes["election.trust"][anes["election.trust"] == -9] <- NA

names(anes)[names(anes) == 'V201233'] <- 'gov.trust'
anes["gov.trust"][anes["gov.trust"] == -9] <- NA
anes["gov.trust"][anes["gov.trust"] == -8] <- NA
anes["gov.trust"][anes["gov.trust"] == -7] <- NA
anes$gov.trust.new[anes$gov.trust==1 ] <- 5
anes$gov.trust.new[anes$gov.trust==2 ] <- 4
anes$gov.trust.new[anes$gov.trust==3 ] <- 3
anes$gov.trust.new[anes$gov.trust==4 ] <- 2
anes$gov.trust.new[anes$gov.trust==5 ] <- 1

# Simple Bivariate Regression results of satisfaction in US democracy and trust in federal government to do what is right. 
# Results: For each unit increase in trust in the federal government (1-5 scale), this model predicts a 0.25*** unit increase in satisfaction in democracy (1-5 scale). R-quared value is ~0.03.
lm1 = lm(democracy.sat.new ~ gov.trust.new, data=anes)
summary(lm1)

CrossTable(anes$democracy.sat.new, anes$gov.trust.new, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, format="SPSS")

## 2. Add an additional variable that might mediate or partly "explain" the initial association from that simple regression above -- and explain your results. Did it work out? Yes? No?
# Results: Including additional variables that represent trust in election officials, income, and frequency of political discussion appear to further explain my results, as the R-squared has increased to ~0.06.
lm2 = lm(democracy.sat.new ~ gov.trust.new + election.trust + income + politics.talk, data=anes)
summary(lm2)
  
## 3. Run another multiple regression. 
## Tell me how you expect your dependent variable to be affected by the independent variables. Interpret your results.
# I chose to create a linear model predicting Biden support. I first renamed and cleaned support for my dependent variable: V201033, which indicates pre-election support for presidential candidates in 2020.
# Removed values indicating 'Refused', 'Don't Know', and other inapplicable values
names(anes)[names(anes) == 'V201033'] <- 'biden.raw'
anes["biden.raw"][anes["biden.raw"] == -9] <- NA
anes["biden.raw"][anes["biden.raw"] == -8] <- NA
anes["biden.raw"][anes["biden.raw"] == -1] <- NA
anes["biden.raw"][anes["biden.raw"] == 11] <- NA
anes["biden.raw"][anes["biden.raw"] == 12] <- NA

# Binary recode, where I chose to represent support for Biden as '1' and all other candidates as '0'.
anes$biden.support = ifelse((anes$biden.raw>1), 0, 1) 
table(anes$biden.support)
table(anes$biden.raw)

# Next, I chose to clean my first independent variable, which represent party ID of respondents. 
# I removed irrelevant values. Variable is on a semi-continuous scale with 1 representing 'strong democrat' and 7 representing 'strong republican'.
names(anes)[names(anes) == 'V201231x'] <- 'party.id'
anes["party.id"][anes["party.id"] == -9] <- NA
anes["party.id"][anes["party.id"] == -8] <- NA
table(anes$party.id)

#Logistic support model predicting biden support using party id and income level.
biden_simple_model <- glm(biden.support ~ party.id + income, 
                      data = anes,
                      family = "binomial")
summary(biden_simple_model)

## 4. Now add another independent variable to that model in Question 3, specifically a set of dummy variables. 
## Tell me why you added that new set of variables and what effect you expected them to have. 
## Did they have an effect? Interpret that new model. 
# I chose to clean and recode a variable representing race into multiple dummy variables.
names(anes)[names(anes) == 'V201549x'] <- 'race'
anes["race"][anes["race"] == -9] <- NA
anes["race"][anes["race"] == -8] <- NA
table(anes$race)
anes$race.white = ifelse((anes$race==1), 1, 0)
anes$race.black = ifelse((anes$race==2), 1, 0)
anes$race.hispanic = ifelse((anes$race==3), 1, 0)
table(anes$race.white)

# Logistic support model indicating support for Biden that includes party ID, and dummy variables for white, black, and hispanic voters, as well as income. 
biden_full_model <- glm(biden.support ~ party.id + race.white + race.black + race.hispanic + income, 
                          data = anes,
                          family = "binomial")
summary(biden_full_model)





