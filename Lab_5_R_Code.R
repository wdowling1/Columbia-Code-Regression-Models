#' ---
#' title: "Lab 5 Code"
#' author: "Will Dowling"
#' date: "21 November 2022"
#' ---

setwd("/Users/Will/Desktop/QMSS/Data Analysis/Lab 5")

# Uploading ANES 2020 data file
anes <- read.csv("~/Desktop/QMSS/Data Analysis/Lab 3/anes_timeseries_2020_csv_20220210/anes_timeseries_2020_csv_20220210.csv")

# Uploading libraries
library(dplyr)
library(psych)
library(tidyverse)
library(gmodels)
library(ggplot2)

## 1. Run a multiple linear probability model (have at least 2 Xs in the model). Tell me how you think your independent variables will affect your dependent variable. Interpret your results. Were your expectations correct? Why or why not?
# I chose to create a linear model predicting Biden support. I first renamed and cleaned support for my dependent variable: V201033, which indicates pre-election support for presidential candidates in 2020.
# Removed values indicating 'Refused', 'Don't Know', and other inapplicable values
names(anes)[names(anes) == 'V201033'] <- 'biden.raw'
anes["biden.raw"][anes["biden.raw"] == -9] <- NA
anes["biden.raw"][anes["biden.raw"] == -8] <- NA
anes["biden.raw"][anes["biden.raw"] == -1] <- NA
anes["biden.raw"][anes["biden.raw"] == 11] <- NA
anes["biden.raw"][anes["biden.raw"] == 12] <- NA
table(anes$biden.raw)

# Next, I chose to clean my first independent variable, which represent party ID of respondents. 
# I removed irrelevant values. Variable is on a semi-continuous scale with 1 representing 'strong democrat' and 7 representing 'strong republican'.
names(anes)[names(anes) == 'V201231x'] <- 'party.id'
anes["party.id"][anes["party.id"] == -9] <- NA
anes["party.id"][anes["party.id"] == -8] <- NA
table(anes$party.id)

# I chose to clean variable representing race.
names(anes)[names(anes) == 'V201549x'] <- 'race'
anes["race"][anes["race"] == -9] <- NA
anes["race"][anes["race"] == -8] <- NA
table(anes$race)

# For another set of independent variables, I wanted to examine how urbanicity impacts the likelihood of support for Biden.
names(anes)[names(anes) == 'V202355'] <- 'urbanicity'
table(anes$urbanicity)
anes["urbanicity"][anes["urbanicity"] == -9] <- NA
anes["urbanicity"][anes["urbanicity"] == -8] <- NA
anes["urbanicity"][anes["urbanicity"] == -7] <- NA
anes["urbanicity"][anes["urbanicity"] == -6] <- NA
anes["urbanicity"][anes["urbanicity"] == -5] <- NA

# Finally, looking at how important religion is to a respondent as another independent variable. Scale is from 1 (extremely important) to 5(not important at all). 
names(anes)[names(anes) == 'V201433'] <- 'religion'
table(anes$religion)
anes["religion"][anes["religion"] == -9] <- NA
anes["religion"][anes["religion"] == -8] <- NA

# Subsetting data to omit NAs
anes.sub <- anes[, c("biden.raw", "party.id", "race", "urbanicity", "religion")]
anes.sub <- na.omit(anes.sub)

# Binary recode, where I chose to represent support for Biden as '1' and all other candidates as '0'.
anes.sub$biden.support = ifelse((anes.sub$biden.raw>1), 0, 1) 
table(anes.sub$biden.support)
table(anes.sub$biden.raw)

# Creating race categories, recoding into multiple dummy variables.
anes.sub$race.white = ifelse((anes.sub$race==1), 1, 0)
anes.sub$race.black = ifelse((anes.sub$race==2), 1, 0)
anes.sub$race.hispanic = ifelse((anes.sub$race==3), 1, 0)
table(anes.sub$race.white)

# Running linear probability model
biden.linear = lm(biden.support ~ party.id + race.black + race.hispanic + urbanicity + religion, data = anes.sub)
summary(biden.linear)

## 2. Run a multiple (binary) logistic model. (It can be the same as the above LPM or a new model.) If it is a new model, tell me how you think your independent variables will affect your dependent variable. Interpret your results in the logit scale. Were your expectations correct? Why or why not?
# Converting prior linear probability model predicting Biden support into a logistic model.
biden.logit = glm(biden.support ~ party.id + race.black + race.hispanic + urbanicity + religion, 
                  data = anes.sub, 
                  family = binomial)
summary(biden.logit)

## 3. Get odds ratios from your logit model in Question 2 and interpret some of them.
exp(coef(biden.logit))

## 4. Extra Credit: Get predicted probabilities from your logit model in Question 2 for some constellations of X values and interpret the results.
biden.logit.2 = glm(biden.support ~ party.id + race + urbanicity + religion, 
                  data = anes.sub, 
                  family = binomial)
summary(biden.logit.2)

# Generating predictions for specific x-values
predict(biden.logit.2, type = "response", newdata = data.frame(party.id = c(4,7), race = c(1,1), urbanicity = c(1,1), religion = c(1,1))) # sub-setting to examine results among non-religious, rural, white independents and republicans

predict(biden.logit.2, type = "response", newdata = data.frame(party.id = c(1,4), race = c(2,2), urbanicity = c(4,4), religion = c(1,1))) # sub-setting to examine results among non-religious, urban, black democrats and independents 

predict(biden.logit.2, type = "response", newdata = data.frame(party.id = c(4,4), race = c(1,1), urbanicity = c(2,3), religion = c(5,5))) # sub-setting to examine results among suburban and small-town religious independent whites

# Visualizing results
anes.sub$pred = predict(biden.logit.2, anes.sub, type="response")

biden.plot <- ggplot(anes.sub, aes(x=party.id, y=pred)) + 
  geom_point(alpha=.2) +
  labs(title = "Predicted Support for Joe Biden by Party ID",
       subtitle = "2020 Presidential Election",
       caption = "Data source: 2020 ANES Data File",
       x = "Party ID", y = "Support Probability") +
  geom_smooth(method="glm", se=FALSE, method.args = list(family=binomial), color = "cornflowerblue")
show(biden.plot)




