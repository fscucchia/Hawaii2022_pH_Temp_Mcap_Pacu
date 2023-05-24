## Title: Environmental parameters from Hawaii 2022 samples 
## Author: Jill Ashey 
## Date created: 20230524

### This script will evaluate the environmental parameters from the Hawaii 2022 pH experiment. Parameters will be compared across treatments to identify differences and plotted.

# Load packages
library(tidyverse)
library(multcomp)
library(car)
library(readxl)

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

### no differences by treatment when outliers are removed 





### Discrete measurements via Orion Star handheld monitor
orion <- read_excel("Raw_data/Water_chemistry_OrionStar.xlsx")

## Temperature 
# Plot temperature values by treatment (aka tank in this dataframe)
ggplot(orion, aes(x = tank, y = temp)) +
  geom_boxplot() # looks like there are some outliers in control treatment 

#### may need to remove outliers in control trt

# Compare by one-way anova 
res.aov <- aov(temp ~ tank, data = orion)
summary(res.aov)

# See if there is any differences between specific treatments 
TukeyHSD(res.aov)

# Check ANOVA assumptions 
plot(res.aov, 1) 
leveneTest(temp ~ tank, data = orion) ## need to transform data 

### Differences between all 3 treatments 


## pH
# Plot pH values by treatment (aka tank in this dataframe)
ggplot(orion, aes(x = tank, y = pH)) +
  geom_boxplot()

#### may need to remove outliers in medium trt

# Compare by one-way anova 
res.aov <- aov(pH ~ tank, data = orion)
summary(res.aov)

# See if there is any differences between specific treatments 
TukeyHSD(res.aov)

# Check ANOVA assumptions 
plot(res.aov, 1) 
leveneTest(pH ~ tank, data = orion)

### Differences between all 3 treatments 


## pH mV
# Plot pH mV values by treatment (aka tank in this dataframe)
ggplot(orion, aes(x = tank, y = `pH mV`)) +
  geom_boxplot()

#### may need to remove outliers in medium & high trts

# Compare by one-way anova 
res.aov <- aov(`pH mV` ~ tank, data = orion)
summary(res.aov)

# See if there is any differences between specific treatments 
TukeyHSD(res.aov)

# Check ANOVA assumptions 
plot(res.aov, 1) 
leveneTest(`pH mV` ~ tank, data = orion)

### Differences between all 3 treatments 


## Salinity 
# Plot salinity values by treatment (aka tank in this dataframe)
ggplot(orion, aes(x = tank, y = salinity)) +
  geom_boxplot() # outlier in medium trt

#### may need to remove outlier in medium trt

# Compare by one-way anova 
res.aov <- aov(salinity ~ tank, data = orion)
summary(res.aov)

# See if there is any differences between specific treatments 
TukeyHSD(res.aov)

# Check ANOVA assumptions 
plot(res.aov, 1) 
leveneTest(`pH mV` ~ tank, data = orion)

### No differences between treatments 




