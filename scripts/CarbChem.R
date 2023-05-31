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
library(cowplot)

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
# carb.output$ALK <- carb.output$ALK*1000000 #convert to µmol kg-1
# carb.output$CO2 <- carb.output$CO2*1000000 #convert to µmol kg-1
# carb.output$HCO3 <- carb.output$HCO3*1000000 #convert to µmol kg-1
# carb.output$CO3 <- carb.output$CO3*1000000 #convert to µmol kg-1
# carb.output$DIC <- carb.output$DIC*1000000 #convert to µmol kg-1
carb.output <- carb.output[,-c(1,4,5,8,10:13,19)] #subset variables of interest
carb.output <- cbind(all$treatment, all$squaricle, all$ID, carb.output) # combine sample info with seacarb output 
colnames(carb.output) <- c("Treatment", "Squarical", "ID", "Salinity", "Temperature", "pH", "CO2", "pCO2", "HCO3", "CO3", "DIC", "TA", "AragSat")

# Sort treatment levels
carb.output$Treatment <- factor(carb.output$Treatment, levels = c("control", "medium", "high"))

# Run ANOVA for each parameter
## Test for differences in salinity by treatment with one-way ANOVA
res.aov <- aov(Salinity ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(Salinity ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 

## Test for differences in temperature by treatment with one-way ANOVA
res.aov <- aov(Temperature ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(Treatment ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 

## Test for differences in pH by treatment with one-way ANOVA
res.aov <- aov(pH ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(pH ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 

## Test for differences in CO2 by treatment with one-way ANOVA
res.aov <- aov(CO2 ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(CO2 ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 
#### VARIANCE IS NOT HOMOGENOUS

## Test for differences in pCO2 by treatment with one-way ANOVA
res.aov <- aov(pCO2 ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(pCO2 ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 

## Test for differences in HCO3 by treatment with one-way ANOVA
res.aov <- aov(HCO3 ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(HCO3 ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 

## Test for differences in CO3 by treatment with one-way ANOVA
res.aov <- aov(CO3 ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(CO3 ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 

## Test for differences in DIC by treatment with one-way ANOVA
res.aov <- aov(DIC ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(DIC ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 

## Test for differences in TA by treatment with one-way ANOVA
res.aov <- aov(TA ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(TA ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 

## Test for differences in AragSat by treatment with one-way ANOVA
res.aov <- aov(AragSat ~ Treatment, data = carb.output)
summary(res.aov)
TukeyHSD(res.aov) # See if there is any differences between specific treatments 
plot(res.aov, 1) # Check ANOVA assumptions of normality 
leveneTest(AragSat ~ Treatment, data = carb.output) # Check ANOVA assumptions of homogenity of variance 









# Plot by each parameter 
salinity_plot <- ggplot(carb.output, aes(x = Treatment, y = Salinity, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("Salinity (psu)") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none")
 
temp_plot <- ggplot(carb.output, aes(x = Treatment, y = Temperature, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("Temperature (°C)") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none")

pH_plot <- ggplot(carb.output, aes(x = Treatment, y = pH, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("pH") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 

CO2_plot <- ggplot(carb.output, aes(x = Treatment, y = CO2, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("CO2 (µmol kg-1)") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 

carb.output_sub <- carb.output[-c(15,16,21,60,68),] # remove outlier 
pCO2_plot <- ggplot(carb.output_sub, aes(x = Treatment, y = pCO2, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("pCO2 (µmol kg-1)") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none")
## need to remove outlier

HCO3_plot <- ggplot(carb.output, aes(x = Treatment, y = HCO3, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("HCO3 (µmol kg-1)") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 

CO3_plot <- ggplot(carb.output, aes(x = Treatment, y = CO3, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("CO3 (µmol kg-1)") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none")

DIC_plot <- ggplot(carb.output, aes(x = Treatment, y = DIC, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("Dissolved Inorganic Carbon (µmol kg-1)") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none")

TA_plot <- ggplot(carb.output, aes(x = Treatment, y = TA, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("Total Alkalinity (µmol kg-1)") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none")

AragSat_plot <- ggplot(carb.output, aes(x = Treatment, y = AragSat, fill = Treatment)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab("Aragonite Saturation (µmol kg-1)") +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none")


all_plots<-plot_grid(salinity_plot, temp_plot, pH_plot, CO2_plot, pCO2_plot, HCO3_plot, CO3_plot, DIC_plot, TA_plot, AragSat_plot, 
                     nrow=2, ncol=5, label_y=1, labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), label_size=18)
ggsave(all_plots, file="output/CarbChem/CarbChem_panel.png", width=25, height=10)


### Once I have all the data, I will run stats for all environmental variables by treatment. 
### I will also make tables for the manuscript 



## Plot different way 

