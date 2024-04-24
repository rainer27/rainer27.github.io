### Homework 4
# Rain Lasch


# Load Packages -----------------------------------------------------------

setwd('/Users/rainlasch/Desktop/School/CU Thangs/PSCI3075/Datasets & Labs/')

# states <- load('states.mask.Rdata')

library(crosstable)
library(dplyr)
library(gmodels)
library(sjPlot)
library(ggeffects)
library(ggplot2)
library(visreg)



# Analysis ----------------------------------------------------------------



mean(states$stand) # 50% of states have a stand your ground law

# south has more states that have stand your ground
# west has less states that have stand your ground
# midwest has more states with stand your ground laws
# northeast has less states with stand your ground laws
CrossTable(states$stand, states$region,
           prop.r = F, prop.c = T, chisq = F, prop.chisq = F) 

logistic_reg <- glm(stand ~ house + senate + democrat + log(poptotal), data = states, 
                    family = 'binomial'(link = 'logit'))

# senate is the most significant independent variable with a coefficient of 0.39
# intercept is 0.46 ---> states less likely to have stand your ground laws
tab_model(logistic_reg,
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T)

# variables:
# senate --> State senate ideological median (higher = conservative; 2014)
# democrat --> Percent of population identifying as Democrat (2010)
# house --> State house ideological median (higher = conservative; 2014)
# stand --> Does the state have a "Stand your Ground" law? (0=No, 1=Yes)
# poptotal --> Total population (2008)

ggpredict(logistic_reg, "senate")

plot(ggpredict(logistic_reg, 'senate')) +
  labs(y = 'Stand Your Ground',
       x = 'Senate Ideology',
       title = 'Chart 1: Senate Ideology and Stand Your Ground Laws',
       caption = 'Higher ideology, more conservative')




