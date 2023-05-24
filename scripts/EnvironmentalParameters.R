## Title: Environmental parameters from Hawaii 2022 samples 
## Author: Jill Ashey 
## Date created: 20230524

### This script will evaluate the environmental parameters from the Hawaii 2022 pH experiment. Parameters will be compared across treatments to identify differences and plotted.

# Load packages
library(tidyverse)
library(multcomp)
library(car)

# Set working directory 
setwd("/Users/jillashey/Desktop/PutnamLab/Repositories/Hawaii2022_pH_Temp_Mcap_Pacu/")

# Read in data 
TA <- read.csv("Raw_data/TA/TA_Metadata.csv")

# Remove NAs (samples I haven't processed yet)
TA <- na.omit(TA)

# Plot TA value by treatment 
ggplot(TA, aes(x = treatment, y = TA.Average)) +
  geom_boxplot()

# Remove outliers 
TA_sub <- TA %>%
  filter(TA.Average <= 2200)

# Plot TA values w/o outlier by treatment 
ggplot(TA_sub, aes(x = treatment, y = TA.Average)) +
  geom_boxplot()

# Compare by one-way anova 
res.aov <- aov(TA.Average ~ treatment, data = TA_sub)
summary(res.aov)

# See if there is any differences between specific treatments 
TukeyHSD(res.aov)

# Check ANOVA assumptions 
plot(res.aov, 1) 
leveneTest(TA.Average ~ treatment, data = TA_sub)
