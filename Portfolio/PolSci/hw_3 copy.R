### Homework 3
# Rain Lasch


# Load Packages and Data --------------------------------------------------

library(tidyverse)
library(sjPlot)
library(dplyr)
library(stargazer)

setwd('/Users/rainlasch/Desktop/School/CU Thangs/PSCI3075/Assignments/hw_3/')

load('PSCI_2075_v2.1.RData')


# States ----------------------------------------------------

# recode turnout so it is measured as a percentage rather than a proportion
#states <- states %>%
  #mutate(turnout = turnout*100)
states$turnout <- states$turnout*100 # both methods here work

mean(states$turnout) # 59.96
sd(states$turnout) # 6.589912
# hist(states$turnout) # we can pretty this up down below # add a comma for xlab= and main= for 
# axis and title

# histogram for state turnout percent
ggplot(states, aes(turnout)) +
  geom_histogram(binwidth = 2, fill = 'purple', color = 'black') +
  labs(x = 'Turnout (%)', y = 'Number of States', title = 'Chart 1: Frequency of State Voter Turnout') +
  theme_gray() +
  scale_x_continuous(breaks = seq(from = 40, to = 80, by = 5))

ggsave('hist_1.png', height = 6, width = 8, path = '/Users/rainlasch/Desktop/School/CU Thangs/PSCI3075/Assignments/hw_3')

# region and turnout percent model
biv <- lm(turnout ~ region, states)

# multivariate model
multi <- lm(turnout ~ region + density + hsdiploma + margin, states) # check to see what these are measuring
# density = population density
# hsdiploma = percent of population with a high school diploma
# margin = average percent margin of victory in all electoral races

# set the labels for our table below
# pred_labels <- c('Intercept', 'Pop. Density', 'Diploma', 'Margin of Victory', 'West', 'Midwest', 'Northeast')
dv_labels <- c('Bivariate', 'Multivariate')

# regression table
tab_model(biv, multi,
          dv.labels = dv_labels,
          show.ci = F,
          p.style = 'numeric_stars',
          show.se = T,
          show.reflvl = T,
          title = 'Table 1: OLS Regression on Voter Turnout')

# add a save file line: file = 'bivariate_table.htm'


# NES ------------------------------------------------------------

# thanks to  Scarlett
# mutate(female = ifelse(v960066==1, 0, 1)

# recode a female dummy variable
nes <- nes %>%
  mutate(female = ifelse(gender == 'Male', 0, 1)) # ifelse, yes, no --> yes = return values if True, no = return values if False
# females thus equal 1 for the female variable

# create an age variable
nes <- nes %>%
  mutate(age = 2012 - birthyr)

# create a dummy variable (called college) indicating individuals who have completed four years of college or post-grad: 
# Individuals who have completed four years of college or post-grad should be coded as 1 while all other respondents 
# should be coded as zero
nes <- nes %>%
  mutate(college = ifelse(educ=='4-year'|educ=='Post-grad', 1, 0)) # | acts as an 'or' statement, does 1 if true, 0 if not
# for the college variable, it'll thus equal 1 if the respondent went to some college

# linear regression with fthrc as the dependent variable and the variables you’ve just coded, age, 
# female, and college, as independent variables
regresh <- lm(fthrc ~ age + female + college, nes)

# fthrc = Feelings towards Hillary Clinton (0 = absolute disgust, 100 = absolutely love, 
# 50 = indifferent) ---> continuous DV 

# interaction with female and age
interact <- lm(fthrc ~ college + age*female, nes)

pred_labels_too <- c('Intercept', 'Age', 'Female', 'College', 'Age*Female')
dv_lables_too <- c('Non-Interactive', 'Interactive')

# regression table
tab_model(regresh, interact,
          dv.labels = dv_lables_too,
          pred.labels = pred_labels_too,
          show.ci = F,
          p.style = 'numeric_stars',
          show.se = T,
          title = 'Table 2: OLS Regression and Age/Gender Interaction ')

# file = 'multi_model.htm' OR, if you don't want to save the html file, you can open the 
# table in a viewer window and take a screenshot there
