### Research Project 
# Rain Lasch
# PSCI 3075

# look at homework 2 for more help in what you're doing

# dependent variable: corruption perception

# independent variable: foreign direct investment

# control variables: population density, literacy rate, electric power consumption (kwH per capita), current $US
# GDP, natural resource rents (as % of GDP), and regional (for regional fixed effects)

###

# Load in Data and Packages -----------------------------------------------

library(dplyr) 
library(tidyverse) 
library(readxl) 

setwd("/Users/rainlasch/Desktop/Research Project Data/The Good Stuff/")

cpi <- read.csv("CPI2020_GlobalTablesTS_210125.csv")

fdi <- read.csv("API_BX.KLT.DINV.CD.WD_DS2_en_csv_v2_2918754.csv")

pop_dens <- read_xlsx('pop_density.xlsx') 

# I exclude GINI in the final data set as it's not as important and reduces sample size too much
gini <- read_xlsx("gini_scores.xlsx") 

literacy <- read_xlsx('API_SE.ADT.LITR.ZS_DS2_en_csv_v2_3158952.xlsx')

electric <- read_xlsx('API_EG.USE.ELEC.KH.PC_DS2_en_csv_v2_3161810.xlsx')

gdp <- read_xlsx("current_gdp.xlsx")

resources <- read_xlsx('API_NY.GDP.TOTL.RT.ZS_DS2_en_csv_v2_3165158.xlsx')

# Same situation as GINI scores for these governance variables
governance <- read_xlsx('Data_Extract_From_Worldwide_Governance_Indicators.xlsx')

# get rid of scientific notation as one of our files uses it
options(scipen = 999)


# Cleanup -----------------------------------------------------------------

# CORRUPTION
cpi <- cpi %>%
  rename('2020' = CPI.score.2020, '2019' = CPI.score.2019, '2018' = CPI.score.2018, '2017' = CPI.score.2017,
         '2016' = CPI.score.2016, '2015' = CPI.score.2015, '2014' = CPI.score.2014, '2013' = CPI.Score.2013,
         '2012' = CPI.Score.2012)
cpi <- cpi %>%
  select(Country, Region, `2012`, `2013`, `2014`, `2015`)
cpi_long <- gather(cpi, year, cpi_score, `2012`:`2015`, factor_key = TRUE) # perfect
cpi_long <- na.omit(cpi_long) # skoooo


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


# LITERACY RATE (% population)
literacy <- literacy %>%
  rename('Country'=`Country Name`)
literacy <- literacy %>%
  select(Country, `2012`, `2013`, `2014`, `2015`)
lit_long <- gather(literacy, year, literacy_rate, `2012`:`2015`, factor_key = TRUE)
lit_long <- na.omit(lit_long)

# ELECTRICITY CONSUMPTION (per capita kwH)
electric <- electric %>%
  rename('Country' = `Country Name`)
electric <- electric %>%
  select(Country, `2012`, `2013`, `2014`, `2015`)
electric_long <- gather(electric, year, electr_consum, `2012`:`2015`, factor_key = TRUE)
electric_long <- electric_long %>%
  mutate(log_electr = log(electr_consum))
electric_long <- na.omit(electric_long)


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

# GINI SCORES
# gini <- gini %>%
  # rename('Country' = `Country Name`)
# gini <- gini %>%
  # select(Country, `2012`, `2013`, `2014`, `2015`)
# gini_long <- gather(gini, year, gini_score, `2012`:`2015`, factor_key = TRUE)
# gini_long <- na.omit(gini_long)

# GOVERNANCE
# governance <- governance %>%
  # select(Country, year, `Regulatory Quality: Percentile Rank`, `Rule of Law: Percentile Rank`, 
         # `Political Stability and Absence of Violence/Terrorism: Percentile Rank`, 
         # `Government Effectiveness: Percentile Rank`, `Voice and Accountability: Percentile Rank`)
# governance <- governance %>%
  # rename('reg_quality_rank' = `Regulatory Quality: Percentile Rank`, 'law_rank' = `Rule of Law: Percentile Rank`,
         # 'stability_rank' = `Political Stability and Absence of Violence/Terrorism: Percentile Rank`, 
         # 'effectiveness_rank' = `Government Effectiveness: Percentile Rank`, 'accountability_rank' = `Voice and Accountability: Percentile Rank`)
# columns_to_numeric <- c('reg_quality_rank', 'law_rank', 'stability_rank', 'effectiveness_rank', 'accountability_rank')
# data[ , i] <- apply(data[ , i], 2,            # Specify own function within apply
# function(x) as.numeric(as.character(x)))
# governance[ , columns_to_numeric] <- apply(governance[ , columns_to_numeric], 2, function(x) as.numeric(as.character(x)))
# governance <- na.omit(governance)

# Merge Files -------------------------------------------------------------

merged_set <- merge(cpi_long, fdi_long)

merged_set <- merge(merged_set, electric_long)

merged_set <- merge(merged_set, gdp_long)

merged_set <- merge(merged_set, lit_long)

merged_set <- merge(merged_set, pop_dens_long)

merged_set <- merge(merged_set, resources_long)

# merged_set <- merge(merged_set, gini_long)

# merged_set <- merge(merged_set, governance) 

# we wanna omit the variables last so we keep as much stuff as possible 
merged_set <- na.omit(merged_set) 


# remove all files but the merged data set ---> https://stackoverflow.com/questions/6190051/how-can-i-remove-all-objects-but-one-from-the-workspace-in-r
rm(list=setdiff(ls(), c("merged_set")))


# Model Making (not to be used as final products) ------------------------------------------------------------

ggplot(merged_set, mapping = aes(cpi_score)) +
  geom_histogram(bins = 25, color = 'black', fill = 'purple') +
  theme_classic() +
  labs(x = 'CPI Score', y = 'Frequency', title = 'Chart 1: Frequency of CPI Scores')

# bivariate model of CPI and log FDI
biv_model <- lm(cpi_score ~ log_fdi, merged_set)

### biv_model Notes:
# Neither the intercept nor log_fdi is significant
# intercept negative (but standard error could put its value in the positives too)
# log_fdi positive
### Not a particularly interesting model

# simple multivariate model with all controls (no fixed effects)
simp_multi <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + literacy_rate + pop_density + 
                   resource_rent + gini_score + reg_quality_rank + law_rank + stability_rank +
                   effectiveness_rank + accountability_rank, merged_set)

### simp_multi Notes:
# Intercept highly significant and positive (over 100)
# log_electr negative and significant
# pop_density highly significant and negative
# law_rank significant and positive
# effectiveness_rank highly significant and positive
# Insignificant variables: log_fdi (negative), log_gdp and literacy_rate (negative), resource_rent (positive),
# gini_score (negative), reg_quality_rank (negative), stability_rank and accountability_rank (positive)

tab_model(biv_model, simp_multi,
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T)

# multivariate model year-fixed effects included
time_multi <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + literacy_rate + pop_density +
                   resource_rent + gini_score + reg_quality_rank + accountability_rank +
                   + law_rank + effectiveness_rank + stability_rank + year, merged_set)

# multivariate model Region-fixed effects included
region_multi <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + literacy_rate + pop_density +
                     resource_rent + gini_score + reg_quality_rank + accountability_rank +
                     + law_rank + effectiveness_rank + stability_rank + Region, merged_set)

tab_model(simp_multi, time_multi,
          show.ci = F,
          p.style = 'numeric_stars',
          show.se = T)

### time_multi Notes (relative to the simp_multi):
# log_gdp now positive but still insignificant
# everything else is about the same between the simp_multi (with no fixed effects) and the 
# time_multi (with year fixed effects)
# all years have negative coefficients but none are significant 

tab_model(simp_multi, region_multi,
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T)

### region_multi Notes (relative to the simp_multi):
# log_electr more significant with a bigger negative effect
# log_gdp now positive with a larger positive effect
# pop_density the same but less significant
# resource_rent now negative with a greater negative effect (insignificant though)
# reg_quality_rank now significant with a greater negative effect
# law_rank more significant with a greater positive effect
# effectiveness_rank still highly significant and positive
# Now, Asia-Pacific and Western Europe are both significant and negative (AP has a greater negative effect)
# MENA is the only positive region
### Overall, pretty interesting change from no fixed effects to region fixed effects


# multivariate regression with all the included fixed effects
fixed_multi <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + literacy_rate + pop_density +
                    resource_rent + gini_score + reg_quality_rank + accountability_rank +
                    + law_rank + effectiveness_rank + stability_rank + Region + year, merged_set) # this is the model we'll build off of 

stargazer(region_multi, time_multi, fixed_multi,
          type = 'text')

### fixed_multi Notes:
# 

# multivariate model with natural resource and region interaction
resource_region <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + literacy_rate + 
                        pop_density + year + resource_rent + Region + resource_rent*Region, merged_set)

# multivariate model with literacy and region interaction
literacy_region <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + pop_density +
                        year + resource_rent + literacy_rate*Region, merged_set) # better interaction than before ---> these interactions with Region yield some interesting results

# multivariate regression with year-fixed, region-fixed, resource*log_fdi interaction, and all the other variables
big_reg <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + pop_density +
                resource_rent + gini_score + log_fdi*resource_rent +
                reg_quality_rank + law_rank + effectiveness_rank + stability_rank +
                accountability_rank + year + Region, merged_set)

# table for the big regression
tab_model(big_reg, 
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T)
### Notes on the big regression
# Significant and positive intercept (how to interpret?)
# log_electr highly significant and negative
# pop_density highly significant and slightly negative
# resource_rent highly significant and positive
# law_rank and effectiveness_rank both highly significant and positive
# GINI score significant and negative
# Asia-Pacific and Europe-Central Asia both significant and negative
# Middle East-North Africa significant and positive
# Western Europe highly significant and negative 
# log_fdi*resource_rent highly significant and negative (-0.61)


# let's try another multivariate model ---> exclude reg_quality_rank, accountability_rank, and stability_rank ---> is this just omitted variable bias?
big_reg_too <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + pop_density +
                    resource_rent + gini_score + law_rank + effectiveness_rank +
                    log_fdi*resource_rent + year + Region, merged_set)

### big_reg_too Notes:
# Intercept becomes more significant and positive
# log_electr still highly significant but smaller negative effect
# pop_density the exact same (highly significant and negative)
# resource_rent more significant and bigger positive effect
# law_rank and effectiveness_rank both essentially the same
# GINI score more significant but essentially the same negative value
# Asia-Pacific and Europe-Central Asia are now more significant and more negative
# MENA and WE/EU are about the same
# log_fdi*resource_rent still highly significant with about the same negative value as before (-0.63)

tab_model(big_reg, big_reg_too,
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T)


# let's do another model with a different interaction ---> law_rank
big_reg_three <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + pop_density +
                      resource_rent + gini_score + effectiveness_rank + law_rank +
                      log_fdi*law_rank + year + Region, merged_set)

tab_model(big_reg, big_reg_three,
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T)

### big_reg_three Notes:
# Intercept has become much more positive
# log_fdi now highly negative and significant
# log_electr less significant with smaller effect
# pop_density less significant but about the same
# law_rank no longer significant and is now negative
# effectiveness_rank stays about the same
# gini_score now more significant with about the same negative value
# log_fdi*law_rank interaction insignificant
### Might be a less interesting, and more fragile, model specification as it changes various signs and magnitudes


# let's try another model with a log_fdi*reg_quality_rank interaction
big_reg_four <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + pop_density +
                     resource_rent + gini_score + law_rank + effectiveness_rank + stability_rank +
                     accountability_rank + reg_quality_rank + log_fdi*reg_quality_rank, 
                   merged_set)

tab_model(big_reg, big_reg_four,
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T)

### big_reg_four Notes:
# Intercept is significant and exceeds 100
# Gini score no longer significant and has a much smaller effect
# everything else stays about the same with the same signs
# interaction between log_fdi*reg_quality_rank is insignificant 
### Might not be the best model to use


# let's try an FDI*GDP interaction just for shits and giggles
big_reg_five <- lm(cpi_score ~ log_fdi + log_electr + log_gdp +
                     pop_density + resource_rent + gini_score + 
                     reg_quality_rank + law_rank + effectiveness_rank + 
                     stability_rank + accountability_rank + log_fdi*log_gdp + 
                     year + Region, merged_set)

tab_model(big_reg, big_reg_five,
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T)

### big_reg_five Notes:
# Intercept is less statistically significant and immensely negative (-328)
# log_fdi now significant and much more positive
# log_electr still highly significant and negative
# log_gdp now significant and much more positive
# pop_density less significant but with about the same value
# resource_rent no longer significant and now negative
# reg_quality_rank now negative but still not significant
# law_rank and effectiveness_rank still highly significant but both are more positive
# GINI score no longer significant and has a smaller negative effect
# Western Europe and Asia-Pacific the only two significant regions ---> both still negative
# log_fdi*log_gdp less significant than the fdi*resource interaction, but the effect of this new interaction is more negative (-0.78)
# slightly lower R^2 than the initial model
### This model may make the most theoretical sense as the effect of FDI on countries' corruption
### likely depends on those countries' levels of economic development and it captures a negative effect for 
### increased resource rents which makes theoretical sense


# let's do one more model with an fdi*gini_score interaction just to see the results
big_reg_six <- lm(cpi_score ~ log_fdi + log_electr + log_gdp + pop_density +
                    resource_rent + gini_score + reg_quality_rank + law_rank +
                    effectiveness_rank + stability_rank + accountability_rank +
                    log_fdi*gini_score + year + Region, merged_set)

### big_reg_six Notes:
# Intercept no longer significant and now negative
# log_fdi greater positive effect but still not significant
# log_electr still negative and highly significant
# pop_density still significant with an identically negative value
# resource_rent no longer significant and now slightly negative (makes sense)
# GINI score now positive and only slightly significant (makes sense)
# law_rank and effectiveness_rank still highly significant and positive
# Asia-Pacific and Western Europe only two significant regions ---> still negative
# log_fdi*gini_score interaction less significant but it is still a negative interaction (-0.14)
### This makes theoretical sense as the effect of FDI on corruption may depend on 
### state income inequality, so this model is a good candidate for a final model inclusion


tab_model(big_reg, big_reg_six,
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T)


# let's do a stargazer of every model so far
stargazer(biv_model, simp_multi, time_multi, resource_region, literacy_region, 
          type="text",
          title = 'Models on FDI and Corruption',
          column.labels = c('Bivariate','Multivariate','Time-Fixed Multi','Resource-Region Interaction','Literacy-Region Interaction'))

# quick table
tab_model(biv_model, simp_multi,
          p.style = "numeric_stars",
          show.ci = F,
          show.se = T) # Americas are the baseline region

tab_model(literacy_region, resource_region, 
          p.style = 'numeric_stars',
          show.ci = F,
          show.se = T) # baseline year of 2012


# Model 3: Multivariate Model with all Controls Present 
big_multi <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + 
                  pop_density + resource_rent + gini_score + reg_quality_rank +
                  law_rank + stability_rank + effectiveness_rank + accountability_rank, merged_set)

stargazer(simp_multi, big_multi)

ggplot(big_multi, aes(x = cpi_score, y = log_fdi)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_bw() # pretty uninformative scatter plot of the big_multi regression, but with a trendline 
# it shows that our big_multi ultimately sees CPI scores increasing with increased FDI

# now VIF works ---> load it in from the cars package ---> we'll visualize the VIF values to get
# a good understanding of where we're at
# https://www.statology.org/variance-inflation-factor-r/ 
big_multi_vif <- vif(big_multi)
# https://stackoverflow.com/questions/9981929/how-to-display-all-x-labels-in-r-barplot ---> make labels fit the plot
multi_vif <- barplot(big_multi_vif, main = 'VIF Values: Multivariate w/ All Controls', col = 'purple', las = 2, cex.names = 0.5)

# add a vertical line at 10
abline(h = 10, lwd = 2, lty = 2)

# log_gdp and law_rank have Variance Inflation Factors (VIFs) over 10 ---> these variables
# may be highly correlated with other variables in the model ---> this may be problematic
# as our model is looking to make inferences of the relative importance of each predictor,
# but there is no simple remedy like removing variables so we will keep all the controls 
# for now

# Model 4: model with a few less predictors so as to possibly reduce multicollinearity
less_big_multi <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate +
                       pop_density + resource_rent + gini_score + law_rank, merged_set)

less_big_vif <- vif(less_big_multi)
less_big_vifualization <- barplot(less_big_vif, main = 'VIF Values: Multivariate w/ Fewer Controls', col = 'purple', las = 2, cex.names = 0.6)

stargazer(big_multi, less_big_multi) # stargazer seems to show that the law_rank is
# even more significant, so is this a biased model? 

# now, there are no VIF values higher than ~6.5, and this is much easier to work with 
# furthermore, the law_rank VIF has significantly decreased, so this variable will be much 
# better to work with out of all the governance indicators than the previous others that
# were also included

# All in all, the less_big_multi seems like the perfect base multivariate regression to 
# work from

# Model 5: just using the stability_rank
stable_multi <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate +
                     pop_density + resource_rent + gini_score + stability_rank, merged_set)

stargazer(less_big_multi, stable_multi)

# Model 6: just using the regulatory quality rank 
reg_quality_multi <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate +
                          pop_density + resource_rent + gini_score + reg_quality_rank, merged_set)

stargazer(less_big_multi, reg_quality_multi)

# Model 7: Multivariate Model with No Governance Indicators
no_governance <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density +
                      resource_rent + gini_score, merged_set)

no_governance_vif <- vif(no_governance)
no_governance_vifualization <- barplot(no_governance_vif, main = 'VIF Values: Multivariate w/ Fewer Controls', col = 'purple', las = 2, cex.names = 0.6)

ggplot(no_governance, aes(x = log_fdi, y = cpi_score)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_bw() # this graph shows that many of our model's predictions are insignificant,
# but the trend between FDI and corruption is, nonetheless, a positive one. 
# Looks like about 12 of the 50 observations fall into the confidence bounds (24% significance)



stargazer(less_big_multi, no_governance)




# Would making a dummy variable for whether or not the country is in Middle East-North Africa help?
# This could help to reduce the influence/leverage of those points (Iraq for instance)
# Dummy out something that is causing high leverage or influence!
merged_set$Pakistan <- ifelse(merged_set$Country == 'Pakistan', 1, 0)
merged_set$Mauritius <- ifelse(merged_set$Country == 'Mauritius', 1, 0)
merged_set$Iraq <- ifelse(merged_set$Country == 'Iraq', 1, 0)

# We could use these dummies, but they're just accounting for the leverage of three data points, so it might not be entirely 
# necessary to completely dummy them out in our models as this might make the model less accurate and generalizable. 

# Model 8: Model w/ Dummies for High Leverage Countries
leverage_control_reg <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density +
                             resource_rent + gini_score + Pakistan + Mauritius + Iraq, merged_set)

stargazer(no_governance, leverage_control_reg)


### Report on high leverage cases in your model and say what you've done about them (if anything)


# Model 9: Introducing the GDP*FDI interaction
gdp_fdi <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density +
                resource_rent + gini_score + log_fdi*log_gdp, merged_set)

# interplot shows that this interaction is significant after around 22.5 Log GDP
interplot(m = gdp_fdi, var1 = 'log_gdp', var2 = 'log_fdi')

stargazer(leverage_control_reg, gdp_fdi)


# Model 10: Literacy*FDI Interaction
lit_fdi <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density +
                resource_rent + gini_score + log_fdi*literacy_rate, merged_set)

stargazer(gdp_fdi, lit_fdi)

interplot(m = lit_fdi, var1 = 'literacy_rate', var2 = 'log_fdi')

# Model 11: GINI*FDI Interaction
gini_fdi <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density +
                 resource_rent + gini_score + log_fdi*gini_score, merged_set)

interplot(m = gini_fdi, var1 = 'gini_score', var2 = 'log_fdi')

# Model 12: FDI*Electricity Interaction
electr_fdi <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density +
                   resource_rent + gini_score + log_fdi*log_electr, merged_set)

interplot(m = electr_fdi, var1 = 'log_electr', var2 = 'log_fdi')

stargazer(gdp_fdi, electr_fdi)


# Model 13: Adding Year and Region Fixed Effects to the GDP Interaction
fixed_gdp <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density +
                  resource_rent + gini_score + log_fdi*log_gdp + Region + year, merged_set)

stargazer(gdp_fdi, fixed_gdp)

interplot(m = fixed_gdp, var1 = 'log_gdp', var2 = 'log_fdi')
abline(h = 0, lwd = 2, lty = 2)

# Model 3: Adding the FDI*Resource Interaction
fdi_resource <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density + resource_rent +
                     log_fdi*resource_rent, merged_set)

interplot(m = fdi_resource, var1 = 'log_fdi', var2 = 'resource_rent', hist = T) +
  geom_hline(yintercept = 0, lty = 'dashed')

stargazer(simp_multi, fdi_resource)


# Model 4: GDP*FDI Interaction
fdi_gdp <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density + resource_rent +
                log_fdi*log_gdp, merged_set)

interplot(m = fdi_gdp, var1 = 'log_fdi', var2 = 'log_gdp', hist = T) +
  geom_hline(yintercept = 0, lty = 'dashed')

stargazer(fdi_resource, fdi_gdp)


# Model 5: Literacy*FDI Interaction
fdi_literacy <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density + resource_rent +
                     log_fdi*literacy_rate, merged_set)

interplot(m = fdi_literacy, var1 = 'log_fdi', var2 = 'literacy_rate', hist = T) +
  geom_hline(yintercept = 0, lty = 'dashed')

stargazer(fdi_resource, fdi_literacy)



# Final Models ------------------------------------------------------------

### This section is for final models made with the final data set, which has 98 observations over 13 variables
### with 57 countries 

# have to run these libraries in this block as they block some necessary dependencies for data cleaning
library(car) # for VIF
library(sjPlot)
library(stargazer)
library(gmodels)
library(crosstable)
library(interplot) # for plotting marginal effects from interactions
library(visreg)
library(ggplot2)
library(gapminder)
library(ggeffects)
library(dotwhisker)

# Model 1: bivariate model
bivariate_model <- lm(cpi_score ~ log_fdi, merged_set)

stargazer(bivariate_model) # this gives us a block of Latex code, which is the essential code
# needed for converting from code to table (stargazer --> latex & recompile --> tables)

# Model 2: a multivariate model with all the controls
simp_multi <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density + resource_rent, merged_set)

stargazer(bivariate_model, simp_multi)


# Model 3: Adding Fixed Effects to the Model
fixed_model <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density + resource_rent +
                     Region + year, merged_set)

stargazer(simp_multi, fixed_model)


# Model 4: Fixed Effects with FDI*GDP
fixed_fdi_gdp <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density + resource_rent +
                           log_fdi*log_gdp + Region + year, merged_set)

interplot(m = fixed_fdi_gdp, var1 = 'log_fdi', var2 = 'log_gdp', hist = T) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  labs(x = 'Log GDP ($US)', y = 'Marginal Effect', title = 'Figure 6: Marginal Effect of FDI*GDP',
       caption = 'Histogram is frequency of GDP')

predicted_gdp_fdi <- plot(ggpredict(model = fixed_fdi_gdp, terms = 'log_gdp'))

predicted_gdp_fdi +
  labs(x = 'Log GDP ($US)', y = 'CPI Score',
       title = 'Figure 7: Predicted CPI Scores with FDI*GDP')

stargazer(fdi_gdp, fixed_fdi_gdp)


# Model 5: Fixed Effects with FDI*Literacy
fixed_fdi_literacy <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density + resource_rent +
                      log_fdi*literacy_rate + Region + year, merged_set)

interplot(m = fixed_fdi_literacy, var1 = 'log_fdi', var2 = 'literacy_rate', hist = T) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  labs(x = 'Literacy Rate (% of People 15 and Up)', y = 'Marginal Effect', title = 'Figure 8: Marginal Effect of FDI*Literacy',
       caption = 'Histogram is frequency of literacy rates')

stargazer(fdi_literacy, fixed_fdi_literacy)

predicted_literacy_fdi <- plot(ggpredict(model = fixed_fdi_literacy, terms = 'literacy_rate'))

predicted_literacy_fdi +
  labs(x = 'Literacy Rate (% of People 15 and Up)', y = 'CPI Score',
       title = 'Figure 9: Predicted CPI Scores with FDI*Literacy')


# Model 6: Adding Fixed Effects to FDI*Resource
fixed_fdi_resource <- lm(cpi_score ~ log_fdi + log_gdp + log_electr + literacy_rate + pop_density + resource_rent +
                           log_fdi*resource_rent + Region + year, merged_set)

stargazer(fdi_resource, fixed_fdi_resource)

interplot(m = fixed_fdi_resource, var1 = 'log_fdi', var2 = 'resource_rent', hist = T) +
  geom_hline(yintercept = 0, lty = 'dashed') +
  labs(x = 'Resource Rents (% of GDP)', y = 'Marginal Effect', title = 'Figure 10: Marginal Effect of FDI*Resource Rents',
       caption = 'Histogram is frequency of resource rents')

ggpredict(model = fixed_fdi_resource, terms = "resource_rent")

predicted_resource_fdi <- plot(ggpredict(model = fixed_fdi_resource, terms = "resource_rent"))

predicted_resource_fdi +
  labs(x = 'Resource Rents (% of GDP)', y = 'CPI Score', 
       title = 'Figure 11: Predicted CPI Scores with FDI*Resource')


### Final Stargazers ---> put these into the Latex editor

stargazer(bivariate_model, simp_multi)
stargazer(fixed_model, fixed_fdi_gdp, fixed_fdi_literacy, fixed_fdi_resource)

### Descriptive Stats

ggplot(merged_set, aes(cpi_score)) +
  geom_histogram(color = 'black', fill = 'purple', binwidth = 3) +
  labs(x = 'CPI Score', y = 'Frequency', title = 'Figure 1: Frequency of Corruption Scores')

ggplot(merged_set, aes(log_fdi, cpi_score, color = Region)) +
  geom_point() +
  labs(x = 'Log FDI Inflows (Net $US)', y = 'CPI Score', title = 'Figure 2: CPI Scores and FDI') +
  scale_x_continuous(breaks = seq(0, 26)) +
  scale_color_manual(values = c('AME' = 'red', 'AP' = 'blue',
                                'ECA' = 'chartreuse', 'MENA' = 'purple',
                                'SSA' = 'black', 'WE/EU' = 'magenta'))

region_corruption <- merged_set %>%
  group_by(Region) %>%
  summarise(corruption = mean(cpi_score)) # get the mean CPI (corruption score) per region

region_fdi <- merged_set %>%
  group_by(Region) %>%
  summarise(mean_fdi = mean(log_fdi))

region_resource <- merged_set %>%
  group_by(Region) %>%
  summarise(mean_resource = mean(resource_rent))

### We'll name and label this plot later 
# simple bar plot of corruption by Region (important) ---> Europe-Central Asia is the most corrupt region in our dataset
barplot(region_corruption$corruption, names.arg = region_corruption$Region, col = 'purple', las = 1,
        xlab = 'Region', ylab = 'Average CPI Score', main = 'Figure 3: Average CPI Score by Region')

barplot(region_fdi$mean_fdi, names.arg = region_fdi$Region, col = 'purple', las = 1,
        xlab = 'Region', ylab = 'Average Log FDI Inflow ($US)', main = 'Figure 4: Average FDI by Region')

barplot(region_resource$mean_resource, names.arg = region_resource$Region, col = 'purple', las = 1,
        xlab = 'Region', ylab = 'Average Resource Rents (% of GDP)', main = 'Figure 5: Resource Rents by Region')


# Helpful Code Chunks -----------------------------------------------------

# from Lab 3
# titan_gender <- titan_dat %>% 
# group_by(Sex) %>% 
# summarise(survived = mean(Survived))

# plot the above summary
# ggplot(titan_gender, aes(survived)) +
# geom_bar(color = "black", fill = "orange") + 
# labs(x = "Gender", y = "Proportion", title = "Survival of the Titanic Crash by Gender") +
# scale_x_discrete(labels = c("Female", "Male")) +
# theme_classic()

### Use Lab 10 to help with regression diagnostics presentation

### Use Lab 11 for plotting predicted values 

# Interplots:
# https://mran.microsoft.com/snapshot/2017-02-20/web/packages/interplot/vignettes/interplot-vignette.html


# For converting from wide to long data
# library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)
# data_long <- gather(olddata_wide, condition, measurement, control:cond2, factor_key=TRUE)
# data_long


#  ******


# From the homework 2 where you cleaned up the same dataset
# select variables and years
# cpi_2012 <- cpi  %>%
  # select(Country, Region, 'CPI Score' = CPI.Score.2012)

# fdi_2012 <- fdi %>%
  # select('Country' = Country.Name, 'Net FDI Inflow' = X2012)

# get rid of na's 
# fdi_2012 <- na.omit(fdi_2012) 
# cpi_2012 <- na.omit(cpi_2012)

# merge the datasets ---> no need to subtract any rows beforehand, merges the two by the countries that it can
# readily match
# comb_dat <- merge(fdi_2012, cpi_2012)

# sort the data in a better way
# comb_dat <- comb_dat %>%
  # select(Country, Region, `Net FDI Inflow`, `CPI Score`)

# let's add a logged net FDI inflow ---> this could control for the massive dispersion in FDI values
# comb_dat <- comb_dat %>%
  # mutate('Logged FDI Inflow' = log(comb_dat$`Net FDI Inflow`))

# remove the NaN's that mutating the variables created ---> can't log negative numbers
# comb_dat <- na.omit(comb_dat)


# Notes on Models -------------------------------------------------------------------

# models may have some trouble due to the narrow time frame (2012-2015)

# what variables would work best for interactions? ---> resource*aid interaction viable option

# you should plan on using a lagged dependent variable in your model since you plan to do time-series

# should I have year-fixed effects or just a lagged DV?

# Include year-fixed and region-fixed effects ---> look up how to interpret these, and there's no need to make
# dummy variables for these different regions (unless you wanted ones for SSA and East Asia but that might be
# too much work)

# lm.beta(model) ---> create standardized coefficients (if you wanna look up what those are and interpret 
# them: Chapter 10.5 FPSR)

# use vif(model) ---> detect multicollinearity in the model

# we don't want a binomial probit or logit model as our DV is continuous, not dummy

# Time Series: Maybe a  theoretical basis for using a First Differenced DV as I am looking to find what 
# causes shifts in corruption/CPI scores from year to year

# Time Series: maybe wanna use a lagged DV in a distributed lag model to get the cumulative impact (beta) of
# FDI on corruption

# Time Series: use a Koyck transformation with a lagged DV model to make the distributed lag model less 
# cumbersome

# Logistic Regression is also not the kind of regression that we want to work with as we're dealing
# with a continuous DV

# when doing interactions, include both the ordinary variables and the interaction term in the model

# regions essentially having different intercepts each

# Marginal Effects Plot ---> shows the effect of a coefficient of one variable over the range of another variable (Lab 9)
# Interplot for an interaction between two continuous variables

# Show separate interactions with essential control variables (simpler models due to the slimness of your data)
