library(readr)
library(tidyverse)

data = readr::read_csv('./data/daily_SPEC_2014.csv.bz2')
data2 = readxl::read_xlsx('./data/aqs_sites.xlsx')

##### joining data #####
# The combination of a "State.Code", a "County.Code", and a "Site.Num", 
# uniquely identifies a monitoring site
# data %>% select(`State Code`, `County Code`, `Site Num`) %>% summary()
# data2 %>% select(`State Code`, `County Code`, `Site Number`) %>% summary()
# different data types, different column name :(
data_temp = data %>%
  mutate(`State Code` = as.numeric(`State Code`),
         `County Code` = as.numeric(`County Code`),
         `Site Number` = as.numeric(`Site Num`)) %>%
  select(-`Site Num`)
# data_temp %>% select(`State Code`, `County Code`, `Site Number`) %>% summary()

joined_data = data_temp %>%
  right_join(data2, by = c("State Code", "County Code", "Site Number"))
# duplicating columns are kept with .x and .y suffixes


##### QUESTION 1 #####
# What is average Arithmetic.Mean for “Bromine PM2.5 LC”
# in the state of Wisconsin in this dataset?
data %>%
  filter(`Parameter Name` == "Bromine PM2.5 LC",
         `State Name` == "Wisconsin") %>%
  summarise(avg = mean(`Arithmetic Mean`, na.rm = TRUE))
# 0.00396

##### QUESTION 2 #####
# Calculate the average of each chemical constituent across all states,
# monitoring sites and all time points. 
# Which constituent Parameter.Name has the highest average level?
data %>%
  filter(`Parameter Name` %in%
           c("Sodium PM2.5 LC",
             "EC2 PM2.5 LC",
             "Sulfur PM2.5 LC",
             "OC CSN Unadjusted PM2.5 LC TOT")) %>%
  group_by(`Parameter Name`, `State Name`) %>%
  summarise(avg = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(avg)) %>%
  head(1)
# OC CSN Unadjusted PM2.5 LC TOT

##### QUESTION 3 #####
# Which monitoring site has the highest average level
# of “Sulfate PM2.5 LC” across all time?
data %>%
  filter(`Parameter Name` %in%
           c("Sulfate PM2.5 LC")) %>%
  group_by(`State Code`, `County Code`, `Site Num`) %>%
  summarise(avg = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(avg)) %>%
  head(1)
# State 39 County 081 Site 0017

##### QUESTION 4 #####
# What is the absolute difference in the average levels of “EC PM2.5 LC TOR”
# between the states California and Arizona, across all time and all monitoring sites?
df = data %>%
  filter(`Parameter Name` %in%
           c("EC PM2.5 LC TOR"),
         `State Name` %in%
           c("California", "Arizona")) %>%
  group_by(`State Name`) %>%
  summarise(avg = mean(`Arithmetic Mean`, na.rm = TRUE))
abs(df[df$`State Name`=="Arizona", ]$avg - df[df$`State Name`=="California", ]$avg)
# 0.01856696

##### QUESTION 5 #####
# What is the median level of “OC PM2.5 LC TOR”
# in the western United States, across all time?
# Define western as any monitoring location that has a Longitude LESS THAN -100.
data %>%
  filter(`Parameter Name` %in%
           c("OC PM2.5 LC TOR"),
         `Longitude` < -100) %>%
  summarise(median = median(`Arithmetic Mean`, na.rm = TRUE))
# 0.43

##### QUESTION 6 #####
# How many monitoring sites are labelled as 
# both RESIDENTIAL for "Land Use" and SUBURBAN for "Location Setting"?
data2 %>%
  filter(`Land Use` == 'RESIDENTIAL',
         `Location Setting` == "SUBURBAN") %>%
  nrow()
# 3527

##### QUESTION 6 #####
# What is the median level of “EC PM2.5 LC TOR” 
# amongst monitoring sites that are labelled as both “RESIDENTIAL” and “SUBURBAN”
# in the eastern U.S.,
# where eastern is defined as Longitude greater than or equal to -100?
joined_data %>%
  filter(`Parameter Name` %in%
           c("EC PM2.5 LC TOR"),
         `Land Use` == 'RESIDENTIAL',
         `Location Setting` == "SUBURBAN",
         `Longitude.x` >= -100) %>%
  summarise(median = median(`Arithmetic Mean`, na.rm = TRUE))
# 0.61

##### QUESTION 8 #####
# Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use", 
# which month of the year has the highest average levels of "Sulfate PM2.5 LC"?
joined_data %>%
  filter(`Parameter Name` %in%
           c("Sulfate PM2.5 LC"),
         `Land Use` == 'COMMERCIAL') %>%
  select(`Date Local`,`Arithmetic Mean`) %>%
  mutate(month = lubridate::month(`Date Local`)) %>%
  group_by(month) %>%
  summarise(mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  arrange(desc(mean)) %>%
  head(1)
# February                             

##### QUESTION 9 #####
# Take a look at the data for the monitoring site identified by 
# State Code 6, County Code 65, and Site Number 8001 (this monitor is in California).
# At this monitor, for how many days is
# the sum of "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" greater than 10? 
# For each of the chemical constituents, 
# there will be some dates that have multiple `Arithmetic Mean` values 
# at this monitoring site. 
# When there are multiple values on a given date, 
# take the average of the constituent values for that date.
joined_data %>%
  filter(`State Code` == 6,
         `County Code` == 65,
         `Site Number` == 8001,
         `Parameter Name` %in%
           c("Sulfate PM2.5 LC",
             "Total Nitrate PM2.5 LC")) %>%
  group_by(`Parameter Name`, `Date Local`) %>%
  summarise(mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(`Date Local`) %>%
  summarise(sum = sum(mean, na.rm = TRUE)) %>%
  filter(sum > 10) %>%
  nrow()
# 11

##### QUESTION 10 #####
# Which monitoring site in the dataset has the highest correlation between 
# "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" across all dates? 
# Identify the monitoring site by it's State, County, and Site Number code.
joined_data %>%
  filter(`Parameter Name` %in%
           c("Sulfate PM2.5 LC",
             "Total Nitrate PM2.5 LC")) %>%
  group_by(`State Code`, `County Code`, `Site Number`, `Parameter Name`, `Date Local`) %>%
  summarise(mean = mean(`Arithmetic Mean`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = `Parameter Name`, values_from = `mean`) %>%
  group_by(`State Code`, `County Code`, `Site Number`) %>%
  summarise(correlation = cor(`Sulfate PM2.5 LC`, `Total Nitrate PM2.5 LC`)) %>%
  arrange(desc(correlation)) %>%
  head(1)
# State 2, County 90 Site 35