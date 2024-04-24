# INFO Viz HW #2
# Rain Lasch


# Load Data and Packages --------------------------------------------------

library(dplyr)
library(tidyverse)
library(readxl)

setwd("/Users/rainlasch/Desktop/Research Project Data/The Good Stuff/")

cpi <- read.csv("CPI2020_GlobalTablesTS_210125.csv")

fdi <- read.csv("API_BX.KLT.DINV.CD.WD_DS2_en_csv_v2_2918754.csv")

pop_dens <- read_xlsx('pop_density.xlsx')

gdp <- read_xlsx("current_gdp.xlsx")

resources <- read_xlsx('API_NY.GDP.TOTL.RT.ZS_DS2_en_csv_v2_3165158.xlsx')

governance <- read_xlsx('Data_Extract_From_Worldwide_Governance_Indicators.xlsx')

options(scipen = 999)


# Cleanup -----------------------------------------------------------------

# CORRUPTION
cpi <- cpi %>%
  rename('2020' = CPI.score.2020, '2019' = CPI.score.2019, '2018' = CPI.score.2018, '2017' = CPI.score.2017,
         '2016' = CPI.score.2016, '2015' = CPI.score.2015, '2014' = CPI.score.2014, '2013' = CPI.Score.2013,
         '2012' = CPI.Score.2012)
cpi <- cpi %>%
  select(Country, Region, `2012`, `2013`, `2014`, `2015`)
cpi_long <- gather(cpi, year, cpi_score, `2012`:`2015`, factor_key = TRUE)
cpi_long <- na.omit(cpi_long)

# FOREIGN DIRECT INVESTMENT
fdi <- fdi %>%
  rename('2012' = X2012, '2013' = X2013, '2014' = X2014, '2015' = X2015, "Country" = Country.Name)
fdi <- fdi %>%
  select(Country, `2012`, `2013`, `2014`, `2015`)
fdi_long <- gather(fdi, year, fdi, `2012`:`2015`, factor_key = TRUE)
fdi_long <- fdi_long %>%
  mutate(log_fdi = log(fdi))
fdi_long <- na.omit(fdi_long)

# POPULATION DENSITY
pop_dens <- pop_dens %>%
  rename('Country' = `Country Name`)
pop_dens <- pop_dens %>%
  select(Country, `2012`, `2013`, `2014`, `2015`)
pop_dens_long <- gather(pop_dens, year, pop_density, `2012`:`2015`, factor_key = TRUE)
pop_dens_long <- na.omit(pop_dens_long)

# GDP (current $US)
gdp <- gdp %>%
  rename('Country' = `Country Name`)
gdp <- gdp %>%
  select(Country, `2012`, `2013`, `2014`, `2015`)
gdp_long <- gather(gdp, year, gdp_current, `2012`:`2015`, factor_key = TRUE)
gdp_long <- na.omit(gdp_long)
gdp_long <- gdp_long %>%
  mutate(log_gdp = log(gdp_current))

# NATURAL RESOURCE RENTS (% of GDP)
resources <- resources %>%
  rename('Country' = `Country Name`)
resources <- resources %>%
  select(Country, `2012`, `2013`, `2014`, `2015`)
resources_long <- gather(resources, year, resource_rent, `2012`:`2015`, factor_key = TRUE)
resources_long <- na.omit(resources_long)

# GOVERNANCE
governance <- governance %>%
  select(Country, year, `Regulatory Quality: Percentile Rank`, `Rule of Law: Percentile Rank`, 
         `Political Stability and Absence of Violence/Terrorism: Percentile Rank`, 
         `Government Effectiveness: Percentile Rank`, `Voice and Accountability: Percentile Rank`)
governance <- governance %>%
  rename('reg_quality_rank' = `Regulatory Quality: Percentile Rank`, 'law_rank' = `Rule of Law: Percentile Rank`,
         'stability_rank' = `Political Stability and Absence of Violence/Terrorism: Percentile Rank`, 
         'effectiveness_rank' = `Government Effectiveness: Percentile Rank`, 'accountability_rank' = `Voice and Accountability: Percentile Rank`)
columns_to_numeric <- c('reg_quality_rank', 'law_rank', 'stability_rank', 'effectiveness_rank', 'accountability_rank')
# data[ , i] <- apply(data[ , i], 2,            # Specify own function within apply
function(x) as.numeric(as.character(x)))
governance[ , columns_to_numeric] <- apply(governance[ , columns_to_numeric], 2, function(x) as.numeric(as.character(x)))
governance <- na.omit(governance)

# Merging -----------------------------------------------------------------

merged_set <- merge(cpi_long, fdi_long)

merged_set <- merge(merged_set, gdp_long)

merged_set <- merge(merged_set, pop_dens_long)

merged_set <- merge(merged_set, resources_long)

merged_set <- merge(merged_set, governance) 

merged_set <- na.omit(merged_set) 

# remove all files but the merged data set ---> https://stackoverflow.com/questions/6190051/how-can-i-remove-all-objects-but-one-from-the-workspace-in-r
rm(list=setdiff(ls(), c("merged_set")))

# Write CSV ---------------------------------------------------------------

write.csv(merged_set, 'info_hw2.csv')
