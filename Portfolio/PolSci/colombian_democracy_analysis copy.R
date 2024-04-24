### V-Dem Analysis on Colombia 

library(dplyr) 
library(tidyverse)
library(ggplot2)
library(ggthemes)

setwd("/Users/rainlasch/Desktop/School/Practice Data/V-Dem Data/Country_Year_V-Dem_Full_v11.1")

v_dem <- read.csv('V-Dem-CY-Full+Others-v11.1.csv')

options(scipen=999)


# Cleanup for Colombia ----------------------------------------------------

colombia <- v_dem %>%
  filter(country_name == 'Colombia')

# we want to keep these columns: country_name, year, project, historical, histname, v2x_polyarchy, v2x_libdem, v2x_partipdem, 
# v2x_delibdem, v2x_egaldem, v2xel_frefair, v2xeg_eqaccess, v2eltrnout, e_civil_war, e_wbgi_rle

# turnout, civil war, and rule of law do not have enough data so we'll remove them

# the historical variable is a constant, so we'll just remove that too 

colombia <- colombia %>%
  select(country_name, year, project, histname, v2x_polyarchy, v2x_libdem, 
         v2x_partipdem, v2xel_frefair, v2xeg_eqaccess)

colombia <- na.omit(colombia)

# alright, we have to gather the indicators so we can plot by indicator, but we will also rename each of the indexes
# #gather data from columns 2 and 3
# gather(df, key="year", value="points", 2:3)

colombia <- colombia %>%
  rename('Electoral Democracy' = v2x_polyarchy, 'Liberal Democracy' = v2x_libdem,
         'Participatory Democracy' = v2x_partipdem, 'Clean Elections' = v2xel_frefair,
         'Equal Access' = v2xeg_eqaccess)

colombia <- gather(colombia, key = 'Index', value = 'Value', 5:9)

civil_war_colombia <- colombia %>%
  filter(year >= 1964)

# Investigation -----------------------------------------------------------

# Look at v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem, v2xel_frefair, v2xeg_eqaccess in the codebook

# all of them are indexes ranging from 0 (low) to 1 (high) --> polyarchy refers to electoral democracy, v2xel_frefair refers to the clean elections index, 
# eqaccess refers to the equal access index measuring equal access to political power

# create a ggplot line plot of different variables 

# ggplot(civil_war_colombia, aes(year, v2x_polyarchy)) +
  # geom_line()

# ggplot(df, aes(x=x_var, y=y_var)) + 
# geom_line(aes(color=group_var)) +
  # scale_color_manual(name='legend_title', labels=c('lab1', 'lab2', 'lab3'),
                     # values=c('color1', 'color2', 'color3'))

#theme(axis.text.x=element_blank(), #remove x axis labels
      #axis.ticks.x=element_blank(), #remove x axis ticks
      #axis.text.y=element_blank(),  #remove y axis labels
      #axis.ticks.y=element_blank()  #remove y axis ticks
#)

ggplot(civil_war_colombia, aes(year, Value)) +
  geom_line(aes(color=Index), size=1.05) +
  scale_x_discrete(limits = c(1965, 1975, 1985, 1995, 2005, 2015)) +
  theme_fivethirtyeight() +
  labs(x='', y='', title = 'Figure 1: Colombian Democracy Indicators Since 1964', caption = 'Rain Lasch, V-Dem Dataset.') +
  scale_color_brewer(palette="Dark2") +
  theme(legend.position = 'right', legend.direction = 'vertical', plot.caption = element_text(hjust = 1.4))
  
# geom_vline(xintercept = 2016, lty = 'dashed', color = 'red', size = 1.05) +
# geom_text(aes(x=2011, label="2016 FARC Deal", y=0.15), colour="black", size=3)
  

# scale_color_manual(name = 'Index')


