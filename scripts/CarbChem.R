## Title: Carb chem calculations from Hawaii 2022 samples 
## Author: Jill Ashey 
## Date created: 20230528

### This script will calculate various carbonate chemistry parameters for the Hawaii 2020 experimental dataset. 

# Load packages
library(tidyverse)
library(multcomp)
library(car)
library(readxl)
library(seacarb)

# Set working directory 
setwd("/Users/jillashey/Desktop/PutnamLab/Repositories/Hawaii2022_pH_Temp_Mcap_Pacu/")

# Load TA data
TA <- read.csv("Raw_data/TA/TA_Metadata.csv")
TA <- na.omit(TA)
TA_sub <- TA %>%
  filter(TA_Average <= 2200) %>%
  filter(TA_Average >=2080)

# Load orion data 
orion <- read_excel("Raw_data/Water_chemistry_OrionStar.xlsx")

# Average by ID
orion_avg <- orion %>%
  group_by(ID) %>%
  summarise_at(vars(temp, salinity, pH, `pH mV`), list(name = mean))

# Merge dataframes 
all <- full_join(TA_sub, orion_avg, by = "ID") %>% 
  na.omit()

###### NEED TO CALCULATE TOTAL PH VALUE USING TRIS CALIBRATION INFORMATION

## Avg dataframes by treatment 
test <- orion %>%
  group_by(tank) %>% 
  summarise_at(vars(temp, salinity, pH, `pH mV`), list(name = mean))

# Calculate CO2 parameters using seacarb 
carb.output <- carb(flag=8, var1=all$pH_name, var2=all$TA_Average, S= all$salinity_name, T=all$temp_name, P=0, Pt=0, Sit=0, pHscale="T", kf="pf", k1k2="l", ks="d") #calculate seawater chemistry parameters using seacarb
carb.output$ALK <- carb.output$ALK*1000000 #convert to µmol kg-1
carb.output$CO2 <- carb.output$CO2*1000000 #convert to µmol kg-1
carb.output$HCO3 <- carb.output$HCO3*1000000 #convert to µmol kg-1
carb.output$CO3 <- carb.output$CO3*1000000 #convert to µmol kg-1
carb.output$DIC <- carb.output$DIC*1000000 #convert to µmol kg-1
carb.output <- carb.output[,-c(1,4,5,8,10:13,19)] #subset variables of interest
carb.output <- cbind(all$treatment, all$squaricle, all$ID, carb.output) # combine sample info with seacarb output 
colnames(carb.output) <- c("Treatment", "Squarical", "ID", "Salinity", "Temperature", "pH", "CO2", "pCO2", "HCO3", "CO3", "DIC", "TA", "AragSat")


## Plot by each parameter 
ggplot(carb.output, aes(x = Treatment, y = Salinity)) +
  geom_boxplot()
 
ggplot(carb.output, aes(x = Treatment, y = Temperature)) +
  geom_boxplot() 

ggplot(carb.output, aes(x = Treatment, y = pH)) +
  geom_boxplot()  

ggplot(carb.output, aes(x = Treatment, y = CO2)) +
  geom_boxplot()  

ggplot(carb.output, aes(x = Treatment, y = pCO2)) +
  geom_boxplot()

ggplot(carb.output, aes(x = Treatment, y = HCO3)) +
  geom_boxplot()

ggplot(carb.output, aes(x = Treatment, y = CO3)) +
  geom_boxplot()

ggplot(carb.output, aes(x = Treatment, y = DIC)) +
  geom_boxplot()

ggplot(carb.output, aes(x = Treatment, y = TA)) +
  geom_boxplot()

ggplot(carb.output, aes(x = Treatment, y = AragSat)) +
  geom_boxplot()


### Once I have all the data, I will run stats for all environmental variables by treatment. 
### I will also make tables for the manucsript 

