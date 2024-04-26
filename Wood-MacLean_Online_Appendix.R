# Bradley D. Wood-MacLean
# Satisfaction with Democracy in Canada During the COVID-19 Pandemic Replication Code
# April 26th, 2024

# For the MEO survey data used in this project please contact the Media Ecosystems Observatory directly: https://www.mediaecosystemobservatory.com/

## Part 1: Combining The Different Datasets

library(tidyverse)
library(dplyr)

meo <- read.csv("/Users/bradleywood-maclean/Desktop/POL499/Thesis/MEO/MEO.csv")
cases <- read.csv("/Users/bradleywood-maclean/Desktop/POL499/Thesis/Control Variables/cases_pt.csv")
irpp <- read_csv("/Users/bradleywood-maclean/Desktop/POL499/Thesis/IRPP/2022-09-20-INDEX Centre of Excellence COVID Policy Data(1).csv")

meo <- meo %>% 
  mutate(MEO0005 = recode(MEO0005,
                          "1" = "NL",
                          "2" = "PEI",
                          "3" = "NB",
                          "4" = "NS",
                          "5" = "QC",
                          "6" = "ON",
                          "7" = "MB",
                          "8" = "SK",
                          "9" = "AB",
                          "10" = "BC") )


irpp <- irpp %>% 
  mutate(Province.Territory = recode(Province.Territory,
                                     "Newfoundland and Labrador" = "NL",
                                     "Prince Edward Island" = "PEI",
                                     "New Brunswick" = "NB",
                                     "Nova Scotia" = "NS",
                                     "Quebec" = "QC",
                                     "Ontario" = "ON",
                                     "Manitoba" = "MB",
                                     "Saskatchewan" = "SK",
                                     "Alberta" = "AB",
                                     "British Columbia" = "BC") )

cases <- cases %>% 
  mutate(region = recode(region,
                         "PE" = "PEI"))

meo$MEO_date <- as.Date(meo$MEO_date)

cases <- cases %>% 
  rename(total_cases = value)

cases <- cases %>% 
  rename(new_cases = value_daily)

cases <- cases[,-1]
control_data <- left_join(cases, deaths, by = c("date" = "date", "region" = "region"))
control_data <- left_join(control_data, hosp_admissions, by = c("date" = "date", "region" = "region"))
control_data <- left_join(control_data, icu_admissions, by = c("date" = "date", "region" = "region"))
control_data <- left_join(control_data, vaccine_total_doses, by = c("date" = "date", "region" = "region"))

write.csv(control_data, "control_data.csv", row.names = FALSE)

irpp <- irpp %>% 
  rename(region = Province.Territory)

irpp_and_control <- left_join(irpp, control_data, by = c("date" = "date", "region" = "region"))
write.csv(irpp_and_control, "irpp_and_control_data.csv", row.names = FALSE)

meo <- meo %>% 
  rename(region = MEO0005)

meo <- meo %>% 
  rename(date = MEO_date)

meo_combined <- left_join(meo, irpp_and_control, by = c("date" = "date", "region" = "region"))

## Part 2: Prepare the New Dataset for Analysis

indv_meo <- read.csv("/Users/bradleywood-maclean/Desktop/POL499/Thesis/meo_combined.csv")

# Satisfaction with Democracy
indv_meo <- indv_meo %>% 
  rename(satdem = MEO0165_02)
# COVID Concern
indv_meo <- indv_meo %>% 
  rename(covidconcern = MEO0007)
indv_meo <- indv_meo %>% 
  mutate(covidconcern = recode(covidconcern, `1` = 3, `2` =2, `3` = 1, `4` = 0))
# Political News Consumption Frequency
indv_meo <- indv_meo %>% 
  rename(newsfreq = MEO0022)
indv_meo <- indv_meo %>% 
  mutate(newsfreq = recode(newsfreq, `1` = 4, `2` = 3, `3` = 2, `4` = 1, `5` = 0)) 
# Personal Financial Situation
indv_meo <- indv_meo %>% 
  rename(personalfinc = MEO0048)
indv_meo <- indv_meo %>% 
  mutate(personalfinc = recode(personalfinc, `1` = 3, `2` = 1, `3` = 2, `4` = NA_real_))
# Political Spectrum
indv_meo <- indv_meo %>% 
  rename(polspectrum = MEO0057_01)
# Partisanship
indv_meo <- indv_meo %>% 
  rename(partisanship = MEO0055)
indv_meo <- indv_meo %>% 
  mutate(partisanship = recode(partisanship, `1` = "Liberal", `2` = "Conservative", 
                               `3` = "NDP", `4` = "Green", '5' = "Bloc",
                               '6' = "Other", '7' = "None", '8'="IDK"))
# Social Media
indv_meo <- indv_meo %>% 
  rename(social_media = MEO0024)
indv_meo <- indv_meo%>% 
  mutate(social_media = recode(social_media, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1, `6` = 0)) 

# Select the variables of interest
indv_meo_vars<- indv_meo %>% 
  select(satdem, MEO_stringencyindex, new_cases, MEO_wave, date, partisanship,
         covidconcern, newsfreq, personalfinc, polspectrum, region, social_media, MEO_respid) 

# Load the original cases data
cases_data <- read.csv("/Users/bradleywood-maclean/Desktop/POL499/Thesis/Control Variables/cases_pt.csv")
cases_data$region <- gsub("PE", "PEI", cases_data$region)

# Make a rolling_cases variable in the survey data that is the rolling average 
# of cases from the previous 7 days for that respondent's region

indv_meo_vars$date <- as.Date(indv_meo_vars$date, format="%Y-%m-%d")
cases_data$date <- as.Date(cases_data$date, format="%Y-%m-%d")

library(dplyr)
library(lubridate)
library(zoo)

# Make a rolling average for each date within the cases data
cases_data <- cases_data %>%
  arrange(region, date) %>%
  group_by(region) %>%
  mutate(rolling_cases = rollmean(value_daily, 7, align='right', fill=NA))

# Merge the rolling cases into the survey data
indv_meo_vars <- indv_meo_vars %>%
  left_join(cases_data %>% select(region, date, rolling_cases), by = c("region", "date"))

indv_meo_vars2 <- indv_meo_vars
indv_meo_vars <- na.omit(indv_meo_vars)

write.csv(indv_meo_vars, "indv_meo_vars_with_rolling_cases.csv", row.names = FALSE)

# Prepare the population data (to eventually create standardized case counts)
# Population data is measured quarterly by province and comes from Stats Canada 

pop_data <- read.csv("/Users/bradleywood-maclean/Desktop/POL499/Thesis/Population Data/Provincial_Population_Data_Clean.csv")

# Fix the naming conventions for merging #[1] "NL"  "PEI" "NB"  "NS"  "QC"  "ON"  "MB"  "SK"  "AB"  "BC"

pop_data <- pop_data %>% rename(region = Geography)
pop_data$region <- gsub("Newfoundland and Labrador", "NL", pop_data$region)
pop_data$region <- gsub("Prince Edward Island", "PEI", pop_data$region)
pop_data$region <- gsub("New Brunswick", "NB", pop_data$region)
pop_data$region <- gsub("Nova Scotia", "NS", pop_data$region)
pop_data$region <- gsub("Quebec", "QC", pop_data$region)
pop_data$region <- gsub("Ontario", "ON", pop_data$region)
pop_data$region <- gsub("Manitoba", "MB", pop_data$region)
pop_data$region <- gsub("Saskatchewan", "SK", pop_data$region)
pop_data$region <- gsub("Alberta", "AB", pop_data$region)
pop_data$region <- gsub("British Columbia", "BC", pop_data$region)

# Combining pop_data with indv_meo_vars

library(reshape2)
population_long <- melt(pop_data, id.vars = "region", variable.name = "quarter_year", value.name = "population")

indv_meo_vars$date <- as.Date(indv_meo_vars$date)
indv_meo_vars$quarter <- as.integer(format(indv_meo_vars$date, "%m")) %/% 3 + 1
indv_meo_vars$year <- format(indv_meo_vars$date, "%Y")
indv_meo_vars$quarter_year <- paste("Q", indv_meo_vars$quarter, ".", indv_meo_vars$year, sep = "")

merged_data <- merge(indv_meo_vars, population_long, by = c("region", "quarter_year"))

# Make a standardized rolling case count
merged_data$rolling_cases <- as.numeric(merged_data$rolling_cases)
merged_data$population <- gsub(",", "", merged_data$population) # commas in the numbers were messing up the conversion to numeric
merged_data$population <- as.numeric(merged_data$population)
merged_data$stand_rolling_cases <- (merged_data$rolling_cases / merged_data$population) * 1000

# Finalize the dataset to fit with the later code
indv_meo_vars <- merged_data

indv_dataset <- indv_meo_vars %>%
  # Arrange by region and MEO_wave to ensure correct ordering
  arrange(region, MEO_wave) %>%
  # Group by region to create lag within each province
  group_by(region) %>%
  # Create lagged MEO_stringencyindex
  mutate(lag_MEO_stringencyindex = lag(MEO_stringencyindex)) %>%
  # Ungroup for further operations
  ungroup()

# Adjust the newsfreq variable
# The original variable takes 1-5

indv_dataset$adjusted_newsfreq <- indv_dataset$newsfreq - 1

# Convert partisanship to a factor and change the reference category
indv_dataset$partisanship <- factor(indv_dataset$partisanship, 
                                    levels = unique(indv_dataset$partisanship))
indv_dataset$partisanship <- relevel(indv_dataset$partisanship, ref = "Liberal")

# Loading the packages for clustered SE
library(lmtest)
library(sandwich)
library(jtools)
library(officer)
library(huxtable)

# Making province-wave dummies
indv_dataset$region_wave <- paste0(indv_dataset$region, indv_dataset$MEO_wave)


# add a tiny constant only to zero values before logging
indv_dataset$log_cases <- log(ifelse(indv_dataset$stand_rolling_cases == 0,
                                     0.00001, 
                                     indv_dataset$stand_rolling_cases))

## Part 3: Conduct the Analyses 

## Model 1 with heteroscedastic standard error

indv_model2.2 <- lm(satdem ~ MEO_stringencyindex + log_cases + 
                      newsfreq + social_media + partisanship + polspectrum +
                      personalfinc + region + covidconcern + MEO_wave, 
                    data = indv_dataset)

# Summary of the model with heteroscedasticity-robust standard errors
summary_indv_model2.2 <- coeftest(indv_model2.2, vcov = vcovHC(indv_model2.2, type = "HC3"))

# Print the summary
print(summary_indv_model2.2)

## Model 1 with clustered standard error with province clusters

indv_model2.3 <- lm(satdem ~ MEO_stringencyindex + log_cases + 
                      newsfreq + social_media + partisanship + polspectrum +
                      personalfinc + region + covidconcern + MEO_wave, 
                    data = indv_dataset)

robust_se2.3 <- vcovBS(indv_model2.3, type = "wild", cluster = ~ region)
summary_cse2.3 <- coeftest(indv_model2.3, robust_se2.3)
print(summary_cse2.3)

## Model 1 with clustered standard error with province-wave clusters

indv_model2.4 <- lm(satdem ~ MEO_stringencyindex + log_cases + 
                      newsfreq + social_media + partisanship + polspectrum +
                      personalfinc + region + covidconcern + MEO_wave, 
                    data = indv_dataset)

robust_se2.4 <- vcovBS(indv_model2.4, type = "wild", cluster = ~ region_wave)
summary_cse2.4 <- coeftest(indv_model2.4, robust_se2.4)
print(summary_cse2.4)

## Model 2 with heteroscedastic standard error

indv_model1.2 <- lm(satdem ~ MEO_stringencyindex * newsfreq + 
                      log_cases * newsfreq +
                      log_cases * covidconcern + 
                      MEO_stringencyindex * covidconcern +
                      MEO_stringencyindex * social_media +
                      log_cases * social_media +
                      social_media + partisanship + polspectrum + personalfinc +
                      region  + MEO_wave, data = indv_dataset)


# Summary of the model with heteroscedasticity-robust standard errors
summary_indv_model1.2 <- coeftest(indv_model1.2, vcov = vcovHC(indv_model1.2, type = "HC3"))

# Print the summary
print(summary_indv_model1.2)

summary(indv_model1.2)

## Model 2 with clustered standard error with province clusters

indv_model1.3 <- lm(satdem ~ MEO_stringencyindex * newsfreq + 
                      log_cases * newsfreq +
                      log_cases * covidconcern + 
                      MEO_stringencyindex * covidconcern +
                      MEO_stringencyindex * social_media +
                      log_cases * social_media +
                      social_media + partisanship + polspectrum + personalfinc +
                      region  + MEO_wave, data = indv_dataset)


robust_se1.3 <- vcovBS(indv_model1.3, type = "wild", cluster = ~ region)
summary_cse1.3 <- coeftest(indv_model1.3, robust_se1.3)
print(summary_cse1.3)

## Model 2 with clustered standard error with province-wave clusters

indv_model1.4 <- lm(satdem ~ MEO_stringencyindex * newsfreq + 
                      log_cases * newsfreq +
                      log_cases * covidconcern + 
                      MEO_stringencyindex * covidconcern +
                      MEO_stringencyindex * social_media +
                      log_cases * social_media +
                      social_media + partisanship + polspectrum + personalfinc +
                      region  + MEO_wave, data = indv_dataset)


robust_se1.4 <- vcovBS(indv_model1.4, type = "wild", cluster = ~ region_wave)
summary_cse1.4 <- coeftest(indv_model1.4, robust_se1.4)
print(summary_cse1.4)

## Model 3 with heteroscedastic standard error

indv_model3.2 <- lm(satdem ~ social_media + partisanship + polspectrum +
                      personalfinc + region + MEO_wave +
                      log_cases * newsfreq +
                      log_cases * covidconcern +
                      log_cases * social_media , data = indv_dataset)

# Summary of the model with heteroscedasticity-robust standard errors
summary_indv_model3.2 <- coeftest(indv_model3.2, vcov = vcovHC(indv_model3.2, type = "HC3"))

# Print the summary
print(summary_indv_model3.2)

summary(indv_model3.2)

## Model 3 with clustered standard error with province clusters

indv_model3.3 <- lm(satdem ~ social_media + partisanship + polspectrum +
                      personalfinc + region + MEO_wave +
                      log_cases * newsfreq +
                      log_cases * covidconcern +
                      log_cases * social_media , data = indv_dataset)

robust_se3.3 <- vcovBS(indv_model3.3, type = "wild", cluster = ~ region)
summary_cse3.3 <- coeftest(indv_model3.3, robust_se3.3)
print(summary_cse3.3)

## Model 3 with clustered standard error with province-wave clusters

indv_model3.4 <- lm(satdem ~ social_media + partisanship + polspectrum +
                      personalfinc + region + MEO_wave +
                      log_cases * newsfreq +
                      log_cases * covidconcern +
                      log_cases * social_media , data = indv_dataset)

robust_se3.4 <- vcovBS(indv_model3.4, type = "wild", cluster = ~ region_wave)
summary_cse3.4 <- coeftest(indv_model3.4, robust_se3.4)
print(summary_cse3.4)

## Model 4 with heteroscedastic standard error

indv_model4.2 <- lm(satdem ~ social_media + partisanship + polspectrum + personalfinc +
                      region + MEO_stringencyindex * newsfreq + 
                      MEO_stringencyindex * covidconcern + 
                      MEO_stringencyindex * social_media +
                      MEO_stringencyindex * social_media +MEO_wave, 
                    data = indv_dataset)

# Summary of the model with heteroscedasticity-robust standard errors
summary_indv_model4.2 <- coeftest(indv_model4.2, vcov = vcovHC(indv_model4.2, type = "HC3"))

# Print the summary
print(summary_indv_model4.2)

summary(indv_model4.2)

## Model 4 with clustered standard error with province clusters

indv_model4.3 <- lm(satdem ~ social_media + partisanship + polspectrum + personalfinc +
                      region + MEO_stringencyindex * newsfreq + 
                      MEO_stringencyindex * covidconcern + 
                      MEO_stringencyindex * social_media +MEO_wave, 
                    data = indv_dataset)

robust_se4.3 <- vcovBS(indv_model4.3, type = "wild", cluster = ~ region)
summary_cse4.3 <- coeftest(indv_model4.3, robust_se4.3)
print(summary_cse4.3)

## Model 4 with clustered standard error with province-wave clusters

indv_model4.4 <- lm(satdem ~ social_media + partisanship + polspectrum + personalfinc +
                      region + MEO_stringencyindex * newsfreq + 
                      MEO_stringencyindex * covidconcern + 
                      MEO_stringencyindex * social_media +MEO_wave, 
                    data = indv_dataset)

robust_se4.4 <- vcovBS(indv_model4.4, type = "wild", cluster = ~ region_wave)
summary_cse4.4 <- coeftest(indv_model4.4, robust_se4.4)
print(summary_cse4.4)

## Model 5 with heteroscedastic standard error

indv_model5.2 <- lm(satdem ~ MEO_stringencyindex * newsfreq + 
                      log_cases * newsfreq +
                      log_cases * covidconcern + 
                      log_cases * social_media +
                      MEO_stringencyindex * covidconcern +
                      MEO_stringencyindex * social_media +
                      social_media + partisanship + polspectrum + personalfinc +
                      region, data = indv_dataset)


# Summary of the model with heteroscedasticity-robust standard errors
summary_indv_model5.2 <- coeftest(indv_model5.2, vcov = vcovHC(indv_model5.2, type = "HC3"))

# Print the summary
print(summary_indv_model5.2)

## Model 5 with clustered standard error with province clusters

indv_model5.3 <- lm(satdem ~ MEO_stringencyindex * newsfreq + 
                      log_cases * newsfreq +
                      log_cases * covidconcern + 
                      MEO_stringencyindex * covidconcern +
                      MEO_stringencyindex * social_media +
                      log_cases * social_media +
                      social_media + partisanship + polspectrum + personalfinc +
                      region, data = indv_dataset)


robust_se5.3 <- vcovBS(indv_model5.3, type = "wild", cluster = ~ region)
summary_cse5.3 <- coeftest(indv_model5.3, robust_se5.3)
print(summary_cse5.3)

## Model 5 with clustered standard error with province-wave clusters

indv_model5.4 <- lm(satdem ~ MEO_stringencyindex * newsfreq + 
                      log_cases * newsfreq +
                      log_cases * covidconcern + 
                      MEO_stringencyindex * covidconcern +
                      MEO_stringencyindex * social_media +
                      log_cases * social_media +
                      social_media + partisanship + polspectrum + personalfinc +
                      region, data = indv_dataset)


robust_se5.4 <- vcovBS(indv_model5.4, type = "wild", cluster = ~ region_wave)
summary_cse5.4 <- coeftest(indv_model5.4, robust_se5.4)
print(summary_cse5.4)

## Model 6: Just COVID-Concern

Robust_Check_1 <- lm(satdem ~ log_cases * covidconcern + 
                       MEO_stringencyindex * covidconcern +
                       social_media + newsfreq + partisanship + polspectrum + personalfinc +
                       region  + MEO_wave, data = indv_dataset)


# Summary of the model with heteroscedasticity-robust standard errors
summary_Robust_Check_1 <- coeftest(Robust_Check_1, vcov = vcovHC(Robust_Check_1, type = "HC3"))

# Print the summary
print(summary_Robust_Check_1)

## Model 7: Just News Consumption

Robust_Check_2 <- lm(satdem ~ log_cases * newsfreq + 
                       MEO_stringencyindex * newsfreq +
                       social_media + covidconcern + partisanship + polspectrum + personalfinc +
                       region  + MEO_wave, data = indv_dataset)


# Summary of the model with heteroscedasticity-robust standard errors
summary_Robust_Check_2 <- coeftest(Robust_Check_2, vcov = vcovHC(Robust_Check_2, type = "HC3"))

# Print the summary
print(summary_Robust_Check_2)

## Model 8: Just Social Media

Robust_Check_3 <- lm(satdem ~ log_cases * social_media + 
                       MEO_stringencyindex * social_media +
                       newsfreq + covidconcern + partisanship + polspectrum + personalfinc +
                       region  + MEO_wave, data = indv_dataset)


# Summary of the model with heteroscedasticity-robust standard errors
summary_Robust_Check_3 <- coeftest(Robust_Check_3, vcov = vcovHC(Robust_Check_3, type = "HC3"))

# Print the summary
print(summary_Robust_Check_3)

## Part 4: Figures

## Figure 1 

figure1 <- ggplot(indv_meo, aes(x = MEO_wave, y = satdem)) +
  stat_summary(fun = "mean", geom = "point") +
  labs(title = "Figure 1. Average Satisfaction with Democracy Score for Each Wave",
       x = "Survey Wave (Mar. 2020 - Oct. 2021)",
       y = "Satisfaction with Democracy") +
  scale_x_continuous(breaks = seq(0, max(indv_meo$MEO_wave), by = 5)) +
  geom_smooth(method = "loess", se = FALSE, color = "black") +
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure1.png", plot = figure1, width = 8, height = 6)

print(figure1)

## Figure 2

irpp <- read_csv("/Users/bradleywood-maclean/Desktop/POL499/Thesis/IRPP/2022-09-20-INDEX Centre of Excellence COVID Policy Data(1).csv")

figure2 <- ggplot(irpp, aes(x = date, y = stringencyIndex)) +
  geom_line(aes(color = Province.Territory)) +
  labs(color = "Region") +
  theme(legend.position = "bottom", panel.border = element_rect(color = "black", fill = NA)) +
  xlab("Date") +
  ylab("Stringency Index") +
  ggtitle("Figure 2. Stringency Index by Province Over Time") +
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure2.png", plot = figure2, width = 8, height = 6)

print(figure2)

## Figure 3

figure3 <- ggplot(indv_dataset, aes(x = MEO_wave, y = MEO_stringencyindex)) +
  stat_summary(fun = "mean", geom = "point") +
  labs(title = "Figure 3. National Average of Stringency Index Over Time",
       x = "Survey Wave (Mar. 2020 - Oct. 2021)",
       y = "Average Stringency Index") +
  scale_x_continuous(breaks = seq(0, max(indv_dataset$MEO_wave), by = 5)) +
  geom_smooth(method = "loess", se = FALSE, color = "black")+
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure3.png", plot = figure3, width = 8, height = 6)

print(figure3)

# Code to retrieve the summary stats referenced with the figure 
mean(irpp$stringencyIndex) 
sd(irpp$stringencyIndex) 

## Figure 4

figure4 <- ggplot(indv_dataset, aes(x = MEO_wave, y = stand_rolling_cases)) +
  stat_summary(fun = "mean", geom = "point") +
  labs(title = "Figure 4. National Rolling Standardized Case Count Over Time",
       x = "Survey Wave (Mar. 2020 - Oct. 2021)",
       y = "Average Rolling Standardized Case Count") +
  scale_x_continuous(breaks = seq(0, max(indv_dataset$MEO_wave), by = 5)) +
  geom_smooth(method = "loess", se = FALSE, color = "black")+
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure4.png", plot = figure4, width = 8, height = 6)

print(figure4)

## Figure 5

figure5 <- ggplot(indv_dataset, aes(x = MEO_wave, y = stand_rolling_cases)) +
  labs(title = "Figure 5. Provincial Rolling Standardized Case Count Over Time",
       x = "Survey Wave (Mar. 2020 - Oct. 2021)",
       y = "Average Rolling Standardized Case Count") +
  scale_x_continuous(breaks = seq(0, max(indv_dataset$MEO_wave), by = 10)) +
  geom_smooth(method = "loess", se = FALSE, color = "black")+
  facet_wrap(~region)+
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure5.png", plot = figure5, width = 8, height = 6)

print(figure5)

# Code to retrieve the summary stats referenced with the figure 
mean(indv_dataset$stand_rolling_cases)
sd(indv_dataset$stand_rolling_cases)

## Figure 6

# Getting a summary with heteroscedasticity-robust standard errors
summary_indv_model1.2 <- coeftest(indv_model1.2, vcov = vcovHC(indv_model1.2, type = "HC3"))

# Extracting the coefficients and robust standard errors
coef_MEO <- coef(summary_indv_model1.2)["MEO_stringencyindex"]
se_MEO <- sqrt(vcovHC(indv_model1.2, type = "HC3")["MEO_stringencyindex", "MEO_stringencyindex"])
coef_MEO_COVID <- coef(summary_indv_model1.2)["MEO_stringencyindex:covidconcern"]
se_MEO_COVID <- sqrt(vcovHC(indv_model1.2, type = "HC3")["MEO_stringencyindex:covidconcern", "MEO_stringencyindex:covidconcern"])

# Extract Estimates using heteroscedasticity-robust SE
vcov_matrix <- vcovHC(indv_model1.2, type = "HC3")
var_MEO_HC <- vcov_matrix["MEO_stringencyindex", "MEO_stringencyindex"]
var_MEO_COVID_HC <- vcov_matrix["MEO_stringencyindex:covidconcern", "MEO_stringencyindex:covidconcern"]
cov_MEO_MEO_COVID_HC <- vcov_matrix["MEO_stringencyindex", "MEO_stringencyindex:covidconcern"]

# Create Data for Graph
covid_levels <- unique(indv_dataset$covidconcern)
hyp_data1.2 <- data.frame(covidconcern = covid_levels) |>
  mutate(mefct = coef_MEO + coef_MEO_COVID * covidconcern) |>
  mutate(se_mefct = sqrt(var_MEO_HC + (covidconcern^2)*var_MEO_COVID_HC + 2*covidconcern*cov_MEO_MEO_COVID_HC)) |>
  mutate(ci95_mefct_U = mefct + qnorm(0.975) * se_mefct) |> 
  mutate(ci95_mefct_L = mefct - qnorm(0.975) * se_mefct)    

# Create Discrete Graph
dev.new()
par(mar=c(6.1, 5.1, 5.1, 2.1))
plot(hyp_data1.2$covidconcern, hyp_data1.2$mefct,
     pch = 16, type = 'p',
     xlim = c(-0.5, 3.5),  # Set the range of x-axis
     ylim = range(c(hyp_data1.2$ci95_mefct_L, hyp_data1.2$ci95_mefct_U)),
     xaxt = 'n',  # Do not plot x-axis automatically
     ylab = "Marginal Effect of the Stringency Index on Satisfaction with Democracy",
     xlab = "COVID-19 Concern",
     main = "Figure 6. Marginal Effects with 95% Confidence Intervals")

# Manually add x-axis with specified ticks for 0, 1, 2, 3 (I'm assuming you meant 0,1,2,3 not just 1,2,3)
axis(1, at = 0:3, labels = 0:3)

# Add arrows for confidence intervals
arrows(hyp_data1.2$covidconcern, hyp_data1.2$ci95_mefct_U,
       hyp_data1.2$covidconcern, hyp_data1.2$ci95_mefct_L,
       length=0.05, angle=90, code=3)

# Add a horizontal line at 0
abline(h = 0, lty = 2, col = "red")

## Part 5: Appendix (and figures contained therein)

## Appendix 2: Missing Observations

# Step 1: raw data un-edited
total_unedited <- nrow(indv_meo)
print(total_unedited)

# Step 2: Total amount of observations after dropping the first four waves and keeping just the variables of interest
total_vars_of_interest <- nrow(indv_meo_vars)
print(total_vars_of_interest)

# Step 3: Total amount of observations in just waves 1-4 from raw un-edited data
wave_counts <- sapply(1:4, function(wave) nrow(indv_meo[indv_meo$MEO_wave == wave, ]))
waves_1_4 <- sum(wave_counts)
print(waves_1_4)

# Specify the variables of interest
variables_of_interest <- c("partisanship", "newsfreq", "personalfinc",
                           "polspectrum", "region", "covidconcern", "social_media")

# Check for missing values in the specified variables
missing_values <- colSums(is.na(indv_meo[, variables_of_interest]))

# Print variables with missing values
print("Number of missing values for each variable:")
print(missing_values[missing_values > 0])

# Specify the variables of interest
variables_of_interest <- c("partisanship", "newsfreq", "personalfinc",
                           "polspectrum", "region", "covidconcern", "social_media")

# Number of waves in your dataset
num_waves <- max(indv_meo$MEO_wave)

# Initialize a list to store the results
missing_values_by_wave <- list()

# Iterate over each wave
for(wave in 1:num_waves) {
  # Subset the data for the current wave
  data_wave <- indv_meo[indv_meo$MEO_wave == wave, ]

  # Calculate the number of missing values for each variable of interest in the current wave
  missing_values <- colSums(is.na(data_wave[, variables_of_interest]))

  # Store the results in the list, with wave number as the key
  missing_values_by_wave[[paste("Wave", wave)]] <- missing_values
}

# Print the number of missing values for each variable in each wave
print("Number of missing values for each variable in each wave:")
print(missing_values_by_wave)

## Appendix 3: Correlation Matrix

selected_dataset2 <- indv_dataset %>%
  ungroup() %>%
  select(satdem, MEO_stringencyindex, stand_rolling_cases,
         covidconcern, newsfreq, personalfinc,
         polspectrum, social_media, MEO_wave)

correlation_matrix2 <- cor(selected_dataset2, use = "complete.obs")

library(reshape2)

# Reshape the correlation matrix for visualization
melted_correlation_matrix2 <- melt(correlation_matrix2)

# Create the heatmap
heatmap <- ggplot(data = melted_correlation_matrix2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), size = 3, vjust = 1) +
  scale_fill_gradient2(low = "blue", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  scale_x_discrete(labels = c("Satisfaction with Democracy", "Stringency Index",
                              "Standardized Rolling Cases", "COVID Concern",
                              "News Frequency", "Personal Finance Concern",
                              "Political Spectrum", "Social Media Usage", "Survey Wave")) +
  scale_y_discrete(labels = c("Satisfaction with Democracy", "Stringency Index",
                              "Standardized Rolling Cases", "COVID Concern",
                              "News Frequency", "Personal Finance Concern",
                              "Political Spectrum", "Social Media Usage", "Survey Wave")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, vjust = 1))+
  xlab("Variable 1")+
  ylab("Variable 2")+
  ggtitle("Figure 7. Correlation Matrix")

ggsave(filename = "figures_for_final_draft/heatmap.png", plot = heatmap, width = 8, height = 6)

print(heatmap)

## Appendix 4: Time Trends for Control Variables

## Figure 8

figure8 <- ggplot(indv_dataset, aes(x = MEO_wave, y = covidconcern)) +
  stat_summary(fun = "mean", geom = "point") +
  labs(title = "Figure 8. Average Concern About COVID-19 Over Time", 
       x = "Survey Wave (Mar. 2020 - Oct. 2021)", 
       y = "Average Concern About COVID-19") +
  scale_x_continuous(breaks = seq(0, max(indv_dataset$MEO_wave), by = 5)) +
  geom_smooth(method = "loess", se = FALSE, color = "black")+
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure8.png", plot = figure8, width = 8, height = 6)
print(figure8)

## Figure 9

figure9 <- ggplot(indv_dataset, aes(x = MEO_wave, y = newsfreq)) +
  stat_summary(fun = "mean", geom = "point") +
  labs(title = "Figure 9. National Average of News Consumption Over Time", 
       x = "Survey Wave (Mar. 2020 - Oct. 2021)", 
       y = "Average News Consumption") +
  scale_x_continuous(breaks = seq(0, max(indv_dataset$MEO_wave), by = 5)) +
  geom_smooth(method = "loess", se = FALSE, color = "black")+
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure9.png", plot = figure9, width = 8, height = 6)
print(figure9)

## Figure 10

figure10 <- ggplot(indv_dataset, aes(x = MEO_wave, y = personalfinc)) +
  stat_summary(fun = "mean", geom = "point") +
  labs(title = "Figure 10. National Average of Personal Financial Perceptions Over Time", 
       x = "Wave", 
       y = "Survey Wave (Mar. 2020 - Oct. 2021)") +
  scale_x_continuous(breaks = seq(0, max(indv_dataset$MEO_wave), by = 5)) +
  geom_smooth(method = "loess", se = FALSE, color = "black")+
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure10.png", plot = figure10, width = 8, height = 6)
print(figure10)

## Figure 11

figure11 <- ggplot(indv_dataset, aes(x = MEO_wave, y = polspectrum)) +
  stat_summary(fun = "mean", geom = "point") +
  labs(title = "Figure 11. National Average Ideology Over Time", 
       x = "Survey Wave (Mar. 2020 - Oct. 2021)", 
       y = "Average Political Spectrum") +
  scale_x_continuous(breaks = seq(0, max(indv_dataset$MEO_wave), by = 5)) +
  geom_smooth(method = "loess", se = FALSE, color = "black")+
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure11.png", plot = figure11, width = 8, height = 6)
print(figure11)

## Figure 12

figure12 <- ggplot(indv_dataset, aes(x = MEO_wave, y = social_media)) +
  stat_summary(fun = "mean", geom = "point") +
  labs(title = "Figure 12. National Average Social Media Use Over Time", 
       x = "Survey Wave (Mar. 2020 - Oct. 2021)", 
       y = "Average Social Media Use") +
  scale_x_continuous(breaks = seq(0, max(indv_dataset$MEO_wave), by = 5)) +
  geom_smooth(method = "loess", se = FALSE, color = "black")+
  theme_bw()

ggsave(filename = "figures_for_final_draft/figure12.png", plot = figure12, width = 8, height = 6)
print(figure12)

## Appendix 6

## 7.6.1 Aggregate-Level Analyses

aggregate_dataset <- indv_meo_vars %>%
  group_by(region, MEO_wave) %>%
  summarise(
    agg_satdem = mean(satdem, na.rm = TRUE),
    agg_MEO_stringencyindex = mean(MEO_stringencyindex, na.rm = TRUE),
    agg_stand_rolling_cases = mean(stand_rolling_cases, na.rm = TRUE),
    agg_covidconcern = mean(covidconcern, na.rm = TRUE),
    agg_newsfreq = mean(newsfreq, na.rm = TRUE),
    agg_personalfinc = mean(personalfinc, na.rm = TRUE),
    agg_polspectrum = mean(polspectrum, na.rm = TRUE),
    agg_social_media = mean(social_media, na.rm = TRUE))

# Drop waves 1-3 (No SatDem question in these early waves)
aggregate_dataset <- aggregate_dataset %>%
  filter(!MEO_wave %in% c(1, 2, 3))

## Differenced Dependent Variable

differenced_dataset <- aggregate_dataset

# Filtering out observations where MEO_wave is 1, 2, or 3
differenced_dataset <- differenced_dataset %>%
  filter(!MEO_wave %in% c(1, 2, 3))

# Group by region and then make the differenced avg_satdem
differenced_dataset <- differenced_dataset %>%
  group_by(region) %>%
  arrange(MEO_wave, .by_group = TRUE) %>%
  mutate(diff_agg_satdem = c(NA, diff(agg_satdem)))

# Now add a lagged version of the stringency index

differenced_dataset <- differenced_dataset %>% 
  mutate(
    lag_agg_MEO_stringencyindex = lag(agg_MEO_stringencyindex, 1)
  )

# Lagged Cases

differenced_dataset <- differenced_dataset %>% 
  mutate(
    lag_agg_rolling_cases = lag(agg_stand_rolling_cases, 1)
  )

# Linear model
differenced_model <- lm(diff_agg_satdem ~ agg_MEO_stringencyindex +
                          lag_agg_MEO_stringencyindex + agg_stand_rolling_cases + 
                          lag_agg_rolling_cases + agg_newsfreq + 
                          agg_covidconcern + agg_personalfinc + 
                          agg_polspectrum + agg_social_media,
                        data = differenced_dataset)

# Model summary
summary(differenced_model)

## Koyck Transformation Model

laggedDV_dataset <- aggregate_dataset %>%
  group_by(region) %>%
  arrange(MEO_wave, .by_group = TRUE) %>%
  mutate(lag_agg_satdem = lag(agg_satdem))

# Adding a lagged stringency index
laggedDV_dataset <- laggedDV_dataset %>% 
  mutate(
    lag_agg_MEO_stringencyindex = lag(agg_MEO_stringencyindex, 1)
  )

# Lagged Cases

laggedDV_dataset <- laggedDV_dataset %>% 
  mutate(
    lag_agg_stand_rolling_cases = lag(agg_stand_rolling_cases, 1)
  )

# Create the lagged-dependent-variable model with provincial fixed effects
laggedDV_model <- lm(agg_satdem ~ lag_agg_satdem + agg_MEO_stringencyindex + 
                       lag_agg_MEO_stringencyindex + agg_newsfreq + 
                       agg_stand_rolling_cases + lag_agg_stand_rolling_cases + 
                       agg_covidconcern + agg_personalfinc + agg_polspectrum + 
                       agg_social_media + factor(region),  # Adding provincial fixed effects
                     data = laggedDV_dataset)

# Summary of the regression model
summary(laggedDV_model)

## 7.6.2 Interacting the Stringency Index Directly with Partisanship

# Build the dummies for subsequent interaction
indv_dataset$Conservative_dummy <- ifelse(indv_dataset$partisanship == "Conservative", 1, 0)
indv_dataset$Liberal_dummy <- ifelse(indv_dataset$partisanship == "Liberal", 1, 0)
indv_dataset$NDP_dummy <- ifelse(indv_dataset$partisanship == "NDP", 1, 0)

# the model:

inter_part_model <- lm(satdem ~ MEO_stringencyindex * Conservative_dummy + 
                    MEO_stringencyindex * Liberal_dummy +
                    MEO_stringencyindex * NDP_dummy +
                    stand_rolling_cases + newsfreq + social_media +
                    polspectrum + personalfinc + region + covidconcern + MEO_wave, 
                  data = indv_dataset)

summary(inter_part_model)