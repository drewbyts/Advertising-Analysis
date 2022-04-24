#1. Goals and Setup
## Primary Goal: Identify the social media campaign that effectively converted the most customers
## Secondary Goals:
## Measure performance of ads and campaigns
## Visualize nominal trends in regard to each campaign by age group
## Calculate descriptive kpis to measure success of each campaign
## Identify correlative/non-correlative relationships between variables of interest

# Call Libraries
library(tidyverse)
library(DT)
library(janitor)
library(corrplot)

# remove scientific notation 
options(scipen = 999)

# Ingest Data Set and Turn to Tibble
data <- read.csv("KAG_conversion_data.csv")
data <- as_tibble(data)

# quick view to udnerstand the format
str(data)
glimpse(data)
summary(data)

# create a data dictionary 
## ad_id: unique ID for each ad.
## xyzcampaignid: an ID associated with each ad campaign of XYZ company
## fbcampaignid*: an ID associated with how FB tracks each campaign
## age, gender, 
## an interest as denoted by Facebook
## impressions: number of times AD was shown
## clicks: number of clicks on for that ad
## spent*: amount paid by company xyz to Facebook, to show that ad. 
## Total Conversion: total number of people who enquired about the product after seeing the ad
## Approved conversion: total number of people who bought the product after seeing the ad

# Data Cleaning / Transformations
## What are my data outliers?  Use IQR Method or Z Score Method 
## Do I have NAs/Missing Values?
sum(is.na(data))
## Should I change column names to be better understood?
# Remove duplicate records (if needed) by column name?
data %>% get_dupes(xyz_campaign_id) # there are many xyz_campaign_id and thus many duplicates
data %>% get_dupes(ad_id) #there are only unique ad_ids and thus no duplicates.
# Do I need to change data format from numeric to date?
# No, no date change needed


# 2. Analysis

#2.1 EDA
# Create Frequency Tables of Items in Data set (Overview)
tabyl(data, Total_Conversion) %>% arrange(desc(percent))

## Two Way Tabyl - by Campaign Id
data %>% tabyl(xyz_campaign_id, age)

## Three Way Tabyl - by Campaign Id
data %>% tabyl(xyz_campaign_id, age, gender)

## Create Bar Plots to Identify Nominal Impact of Ads Shown to Various Age Groups
d <- ggplot(data, aes(age))
d + geom_bar()

f <- ggplot(data, aes(age, Impressions))
f + geom_col()

l <- ggplot(data, aes(age, Clicks))
l + geom_col()

# Sum of Total Impressions of Each Campaign
data %>% group_by(xyz_campaign_id) %>% 
  summarise(Impressions = sum(Impressions))

# Sum of Total Clicks of Each Campaign
data %>% group_by(xyz_campaign_id) %>% 
  summarise(Clicks = sum(Clicks))

# Total Cost of Each Campaign
data %>% group_by(xyz_campaign_id) %>%
  summarise(Spent = sum(Spent))

# Calculate kpis of CTR, CPC, CPM, CAC, and CR
all_kpis <- data %>% select(xyz_campaign_id, Impressions, Clicks, Spent, Total_Conversion, Approved_Conversion) %>%
  group_by(xyz_campaign_id) %>% 
  summarise(Clicks = sum(Clicks),
            Approved_Conversion = sum(Approved_Conversion),
            Impressions = sum(Impressions),
            Spent = sum(Spent),
            Total_Conversion = sum(Total_Conversion)) %>%
  mutate(CTR = (Clicks/Impressions) * 100,
         CPC = Spent/Clicks,
         CPM = (Spent/Impressions) * 1000,
         CAC = Spent/Approved_Conversion,
         CR = Approved_Conversion/Clicks)

# 2.2 Diagnostic Analytics
diag_data <- data %>% select(Impressions, Clicks, Spent, Total_Conversion, Approved_Conversion)
A = cor(diag_data)
corrplot(A, method = "number")



# 3. Findings:
## Campaign 916 had of 21.2% conversion rate. A CTR of 2.3%. The CPC was $1.32. The number of impressions was 482,925.
## Campaign 916. CPM is $0.31. The total cost of the campaign $150. The CAC was $2.77. Total number of people who bought a product was 54. 

## Campaign 936 had a 9% conversions rate. It had a CTR of 2.4% and the CPC was $1.45.The number of impressions was 8,128,187. 
## Campaign 936. CPM is $0.35. The total cost of the campaign was $2893. CAC is $6.23. Total number who bought a product was 464. 

## Campaign 1178 had a 2.4% conversion rate. It had a CTR of 1.7% and the CPC was $1.54. The number of impressions was 204,823,716. 
## CPM is $0.27 The total cost of the campaign was $55,662. CAC is $89.05. Total number who bought a product was 625.  


# Diagnostic Analytics Findings
## Clicks and Impressions have a 0.95 correlation coefficient CC
## Clicks and Approved_Conversions have a 0.56 CC
## Spent and Clicks have a 0.99 CC
## Spent and Approved_Conversion have a 0.59 CC

#4. Recommendations

## Analyze Campaign 916 further to understand why it was more effective than the other campaigns. 
## We need to find a table to pair the FB numerical interest value with its associated character value.
## Create a linear regression model that incorporates amount spent per ad and clicks for each campaign. 
