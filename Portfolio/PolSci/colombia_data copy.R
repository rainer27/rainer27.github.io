library(dplyr) 
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(readxl)

setwd("/Users/rainlasch/Desktop/School/CU Thangs/PSCI/Demo, Ineq, and Viol in LA/Final Paper")

homicides <- read_xlsx('homicides_per_100k.xlsx')

colombia <- read.csv('colombia.csv')


# Homicides Cleanup -------------------------------------------------------

homicides <- homicides %>%
  filter(`Country Name` == 'Colombia')

homicides <- homicides %>%
  select(`Country Name`, `Indicator Name`, `1990`:`2020`)

homicides <- gather(homicides, key='Year', value = 'Value', 3:33)


# Merging the Datasets ----------------------------------------------------

colombia <- colombia %>%
  rename('Country Name' = country_name, 'Year' = year, 'Index Value' = Value)

homicides <- homicides %>%
  rename('Homicides per 100k' = Value)

homicides <- homicides %>%
  select(`Country Name`, Year, `Homicides per 100k`)

merged_set <- merge(homicides, colombia)

merged_set <- merged_set %>%
  select(`Country Name`, `Homicides per 100k`, Year, Index, `Index Value`)

merged_set <- merged_set %>%
  filter(Index == 'Electoral Democracy')


# Write the CSV File -------------------------------------------------------------------------

write.csv(merged_set,"/Users/rainlasch/Desktop/School/CU Thangs/PSCI/Demo, Ineq, and Viol in LA/Final Paper/colombia_data.csv", row.names = FALSE)  
  
  
  