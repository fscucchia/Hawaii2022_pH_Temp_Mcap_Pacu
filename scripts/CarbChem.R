## Title: Carb chem calculations from Hawaii 2022 samples 
## Author: Jill Ashey 
## Date created: 20230528

### This script will calculate various carbonate chemistry parameters for the Hawaii 2020 experimental dataset. Using scripts from Sam Gurr (https://github.com/SamGurr/Geoduck_transgen_offspring_OA/blob/master/RAnalysis/Scripts/CarbChem.R) and Emma Strand (https://github.com/hputnam/Acclim_Dynamics/blob/master/Scripts/CarbChem.Rmd)

# Load packages
library(tidyverse)
library(multcomp)
library(car)
library(readxl)
library(seacarb)
library(cowplot)

# Set working directory 
setwd("/Users/jillashey/Desktop/PutnamLab/Repositories/Hawaii2022_pH_Temp_Mcap_Pacu/")

##### DISCRETE pH CALCULATIONS #####
path <-("Raw_data/pH_tris")
file.names<-list.files(path = path, pattern = "csv$") #list all the file names in the folder to get only get the csv files
pH.cals <- data.frame(matrix(NA, nrow=length(file.names), ncol=3, dimnames=list(file.names,c("Date", "Intercept", "Slope")))) #generate a 3 column dataframe with specific column names

for(i in 1:length(file.names)) { # for every file in list start at the first and run this following function
  Calib.Data <-read.table(file.path(path,file.names[i]), header=TRUE, sep=",", na.string="NA", as.is=TRUE) #reads in the data files
  model <-lm(mVTris ~ TTris, data=Calib.Data) #runs a linear regression of mV as a function of temperature
  coe <- coef(model) #extracts the coeffecients
  summary(model)$r.squared
  plot(Calib.Data$mVTris, Calib.Data$TTris)
  pH.cals[i,2:3] <- coe #inserts them in the dataframe
  pH.cals[i,1] <- substr(file.names[i],1,8) #stores the file name in the Date column
}
colnames(pH.cals) <- c("Calib.Date",  "Intercept",  "Slope") #rename columns
pH.cals

#constants for use in pH calculation 
R <- 8.31447215 #gas constant in J mol-1 K-1 
F <-96485.339924 #Faraday constant in coulombs mol-1

# Read in daily measurements 
daily <- read.csv("Raw_data/Water_chemistry_OrionStar_TA.csv")[,1:10]
daily <- na.omit(daily)

# Merge daily measurements with pH tris 
SW.chem <- merge(pH.cals, daily, by = "Calib.Date")

# Calculate pH total
mvTris <- SW.chem$temp*SW.chem$Slope+SW.chem$Intercept #calculate the mV of the tris standard using the temperature mv relationships in the measured standard curves 
STris<-34.5 #salinity of the Tris
phTris<- (11911.08-18.2499*STris-0.039336*STris^2)*(1/(SW.chem$temp+273.15))-366.27059+ 0.53993607*STris+0.00016329*STris^2+(64.52243-0.084041*STris)*log(SW.chem$temp+273.15)-0.11149858*(SW.chem$temp+273.15) #calculate the pH of the tris (Dickson A. G., Sabine C. L. and Christian J. R., SOP 6a)
SW.chem$pH.Total<-phTris+(mvTris/1000-SW.chem$pH_mV/1000)/(R*(SW.chem$temp+273.15)*log(10)/F) #calculate the pH on the total scale (Dickson A. G., Sabine C. L. and Christian J. R., SOP 6a)


##### DISCRETE TA CALCULATIONS #####
TA <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv")
TA$SampleID <- gsub("^[^_]*_", "", TA$SampleID) # remove 1_ or 2_ from sample ID
TA_mean <- TA %>%
  filter(Type == "Sample") %>%
  group_by(SampleID) %>%
  summarise(mean_TA = mean(TA), .groups = "drop")



# Merge calculated pH and daily measures with TA data and run seacarb
SW.chem <- merge(SW.chem, TA_mean, by="SampleID", all = TRUE, sort = T) #merge seawater chemistry with total alkalinity
SW.chem <- na.omit(SW.chem)

# Calculate CO2 parameters using seacarb 
carb.output <- carb(flag=8, var1=SW.chem$pH.Total, var2=SW.chem$mean_TA, S= SW.chem$salinity, T=SW.chem$temp, P=0, Pt=0, Sit=0, pHscale="T", kf="pf", k1k2="l", ks="d") #calculate seawater chemistry parameters using seacarb
#carb.output$ALK <- carb.output$ALK*1000000 #convert to µmol kg-1
#carb.output$CO2 <- carb.output$CO2*1000000 #convert to µmol kg-1
#carb.output$HCO3 <- carb.output$HCO3*1000000 #convert to µmol kg-1
#carb.output$CO3 <- carb.output$CO3*1000000 #convert to µmol kg-1
#carb.output$DIC <- carb.output$DIC*1000000 #convert to µmol kg-1
carb.output <- carb.output[,-c(1,4,5,8,10:13,19)] #subset variables of interest

## for some reason, when I convert the carb chem parameters to µmol kg-1, the numbers get very large. Also pCO2 looks very strange...the numbers are either 0 or negative

# Bind SW chem information
carb.output <- cbind(SW.chem$SampleID, SW.chem$Date, SW.chem$tank, SW.chem$squaricle, carb.output) # combine sample info with seacarb output 
colnames(carb.output) <- c("SampleID", "Date", "Treatment", "Squarical", "Salinity", "Temperature", "pH", "CO2", "pCO2", "HCO3", "CO3", "DIC", "TA", "AragSat")

















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

# Arrange treatment order 
carb.output$Treatment <- factor(carb.output$Treatment, levels = c("C", "M", "H"))

# Salinity
compare_means(Salinity ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

sal_plot <- ggplot(carb.output, aes(x = Treatment, y = Salinity, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 37.4) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("Salinity (psu)")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
sal_plot

# Temperature
compare_means(Temperature ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

temp_plot <- ggplot(carb.output, aes(x = Treatment, y = Temperature, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 34) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("Temperature (°C)")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
temp_plot

# pH
compare_means(pH ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

pH_plot <- ggplot(carb.output, aes(x = Treatment, y = pH, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 8.2) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("pH (Total scale)")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
pH_plot

# CO2
compare_means(CO2 ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

CO2_plot <- ggplot(carb.output, aes(x = Treatment, y = CO2, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 54) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("CO2 (mol", " kg"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
CO2_plot

# pCO2
compare_means(pCO2 ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

pCO2_plot <- ggplot(carb.output, aes(x = Treatment, y = pCO2, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova") +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("pCO2 (mol", " kg"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
pCO2_plot

# HCO3
compare_means(HCO3 ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

HCO3_plot <- ggplot(carb.output, aes(x = Treatment, y = HCO3, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 2150) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("HCO3 (mol", " kg"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
HCO3_plot

# CO3
compare_means(CO3 ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

CO3_plot <- ggplot(carb.output, aes(x = Treatment, y = CO3, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 275) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("CO3 (mol", " kg"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
CO3_plot

# DIC
compare_means(DIC ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

DIC_plot <- ggplot(carb.output, aes(x = Treatment, y = DIC, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 2250) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("DIC (mol", " kg"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
DIC_plot

# TA
compare_means(TA ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

TA_plot <- ggplot(carb.output, aes(x = Treatment, y = TA, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 2450) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("Total alkalinity (mol", " kg"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
TA_plot

# Aragonite saturation
compare_means(AragSat ~ Treatment, data = carb.output, method = "anova")
my_comparisons <- list(c("C", "M"), c("C", "H"), c("M", "H"))

AragSat_plot <- ggplot(carb.output, aes(x = Treatment, y = AragSat, fill = Treatment)) +
  #geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 2450) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("Aragonite Saturation")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
AragSat_plot



all_plots<-plot_grid(salinity_plot, temp_plot, pH_plot, CO2_plot, pCO2_plot, HCO3_plot, CO3_plot, DIC_plot, TA_plot, AragSat_plot, 
                     nrow=2, ncol=5, label_y=1, labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"), label_size=18)
ggsave(all_plots, file="output/CarbChem/CarbChem_panel.png", width=25, height=10)


### Need to look into removing outliers and whats going on w/ units for pCO2 and aragonite saturation




