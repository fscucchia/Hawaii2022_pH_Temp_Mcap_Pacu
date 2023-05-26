## Title: TA calculations from Hawaii 2022 samples 
## Author: Jill Ashey 
## Date created: 20230520

## This script will calculate TA from the Hawaii 2022 samples. The code here is based on the `Total_Alkalinity_wParsing/R` [scripts](https://github.com/Putnam-Lab/Titrator/tree/main/Scripts) in the Putnam lab titrator github [repo](https://github.com/Putnam-Lab/Titrator/tree/main). 

# Notes: 
# - Created by Nyssa Silbiger 03/28/2014
# - modified 20210126 Sam Gurr - set diff directories and such for PPP Lab titrator (URI)
# - new acid bottle 20190626 by Sam Gurr - changed lines 
# - line 68 changed SampleID to Sample.ID - Sam Gurr 20180713
# - modified 20210221 Danielle Becker - set different mass file and Titratorfile to match code
# - modified 20210318 Danielle Becker - opened a new acid titrant, same batch #A16
# - new acid bottle 20220127 by Danielle Becker - new batch number A22, updated script calculation

## Load libraries & set working directory
## need to load an older version (v3.2) of seacarb for code to work
packageurl <- "https://cran.r-project.org/src/contrib/Archive/seacarb/seacarb_3.2.tar.gz"
install.packages(packageurl, repos = NULL, type = "source")
library(seacarb) #used to calculate TA - should now be version 3.2
library(tidyverse)

setwd("/Users/jillashey/Desktop/PutnamLab/Repositories/Hawaii2022_pH_Temp_Mcap_Pacu/")
main <- getwd()
getwd() # check path 


# This script calculates total alkalinity using potentiometric titrations. It uses a for loop to read in data exported as a titration file and calculate Total Alkalinity. At the end it exports your data as a .csv file. Comment the last line out if your don't want that.
# 
# ### Files needed
#  1. pHCalibration.csv in your "Data" folder
# Inside the Data folder You must have a subfolder for each data set. In each subfolder there is:
#  2. the mass file for your run  
#  3. a subfolder named "TodaysDate" (where all of your titration files are) directly exported from LabX.
# 
# Each day/run will get its own code. 

## 20220831

### Run 1 

## Paths and file info
path <- "Raw_data/TA/20220831" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run1_20220831.csv" # name of mass file 
titrationfile <- "20220831_Run1.csv" # name of last titration file run 
date <- "20220831" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV
png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
lines(c(4,7,10), predict(mod.pH))
R2<-summary(mod.pH)$r.squared
legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

## Create cumulative TA file here - this will be added to as titrations are analyzed
write.table(TA, "Raw_data/TA/Cumulative_TA_Output.csv", sep = ",", row.names = F)









## 20230516

### CRM run

## Paths and file info
path <- "Raw_data/TA/20230516" #the location of all your titration files, your folder of the day!
massfile <- "Mass_CRM_20230516.csv" # name of mass file 
titrationfile <- "20230516_CRM.csv" # name of last titration file run 
date <- "20230516" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV
png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
lines(c(4,7,10), predict(mod.pH))
R2<-summary(mod.pH)$r.squared
legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)






## 20230516

### Run 1

## Paths and file info
path <- "Raw_data/TA/20230516" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run1_20230516.csv" # name of mass file 
titrationfile <- "20230516_Run1.csv" # name of last titration file run 
date <- "20230516" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)




## 20230516

### Run 2

## Paths and file info
path <- "Raw_data/TA/20230516" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run2_20230516.csv" # name of mass file 
titrationfile <- "20230516_Run2.csv" # name of last titration file run 
date <- "20230516" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)

















## 20230516

### Run 3

## Paths and file info
path <- "Raw_data/TA/20230516" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run3_20230516.csv" # name of mass file 
titrationfile <- "20230516_Run3.csv" # name of last titration file run 
date <- "20230516" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)










## 20230522

### CRM run

## Paths and file info
path <- "Raw_data/TA/20230522" #the location of all your titration files, your folder of the day!
massfile <- "Mass_CRM_20230522.csv" # name of mass file 
titrationfile <- "20230522_CRM1.csv" # name of last titration file run 
date <- "20230522" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV
png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
lines(c(4,7,10), predict(mod.pH))
R2<-summary(mod.pH)$r.squared
legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)










## 20230522

### Run 1

## Paths and file info
path <- "Raw_data/TA/20230522" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run1_20230522.csv" # name of mass file 
titrationfile <- "20230522_Run1.csv" # name of last titration file run 
date <- "20230522" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)










## 20230522

### Run 2

## Paths and file info
path <- "Raw_data/TA/20230522" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run2_20230522.csv" # name of mass file 
titrationfile <- "20230522_Run2.csv" # name of last titration file run 
date <- "20230522" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)








## 20230522

### Run 3

## Paths and file info
path <- "Raw_data/TA/20230522" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run3_20230522.csv" # name of mass file 
titrationfile <- "20230522_Run3.csv" # name of last titration file run 
date <- "20230522" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)









## 20230523

### CRM run

## Paths and file info
path <- "Raw_data/TA/20230523" #the location of all your titration files, your folder of the day!
massfile <- "Mass_CRM_20230523.csv" # name of mass file 
titrationfile <- "20230523_CRM.csv" # name of last titration file run 
date <- "20230523" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV
png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
lines(c(4,7,10), predict(mod.pH))
R2<-summary(mod.pH)$r.squared
legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)









## 20230523

### Run 1

## Paths and file info
path <- "Raw_data/TA/20230523" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run1_20230523.csv" # name of mass file 
titrationfile <- "20230523_Run1.csv" # name of last titration file run 
date <- "20230523" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)






## 20230523

### Run 2

## Paths and file info
path <- "Raw_data/TA/20230523" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run2_20230523.csv" # name of mass file 
titrationfile <- "20230523_Run2.csv" # name of last titration file run 
date <- "20230523" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)







## 20230523

### Run 3

## Paths and file info
path <- "Raw_data/TA/20230523" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run3_20230523.csv" # name of mass file 
titrationfile <- "20230523_Run3.csv" # name of last titration file run 
date <- "20230523" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)








## 20230523

### Run 4

## Paths and file info
path <- "Raw_data/TA/20230523" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run4_20230523.csv" # name of mass file 
titrationfile <- "20230523_Run4.csv" # name of last titration file run 
date <- "20230523" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)







## 20230525

### CRM run

## Paths and file info
path <- "Raw_data/TA/20230525" #the location of all your titration files, your folder of the day!
massfile <- "Mass_CRM_20230525.csv" # name of mass file 
titrationfile <- "20230525_CRM.csv" # name of last titration file run 
date <- "20230525" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV
png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
lines(c(4,7,10), predict(mod.pH))
R2<-summary(mod.pH)$r.squared
legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)







## 20230525

### Run 1

## Paths and file info
path <- "Raw_data/TA/20230525" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run1_20230525.csv" # name of mass file 
titrationfile <- "20230525_Run1.csv" # name of last titration file run 
date <- "20230525" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)









## 20230525

### Run 2

## Paths and file info
path <- "Raw_data/TA/20230525" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run2_20230525.csv" # name of mass file 
titrationfile <- "20230525_Run2.csv" # name of last titration file run 
date <- "20230525" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)










## 20230525

### Run 3

## Paths and file info
path <- "Raw_data/TA/20230525" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run3_20230525.csv" # name of mass file 
titrationfile <- "20230525_Run3.csv" # name of last titration file run 
date <- "20230525" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)











## 20230525

### Run 4

## Paths and file info
path <- "Raw_data/TA/20230525" #the location of all your titration files, your folder of the day!
massfile <- "Mass_Run4_20230525.csv" # name of mass file 
titrationfile <- "20230525_Run4.csv" # name of last titration file run 
date <- "20230525" # date that data were run 

## Load mass data 
Mass<-read.csv(file.path(path,massfile), header=T, sep=",", na.string="NA", as.is=T) 
colnames(Mass) <- c("SampleID","Mass","Salinity")

#### pH Calibration #####
pHCal<-read.csv("Raw_data/TA/pHCalibration.csv") # read in the pH Calibration file

# Select calibration for correct date 
pHData <- pHCal[pHCal$Date==date,]

# Calculate pH 3 and 3.5 based on slope and intercept from pH 4, 7, and 10 calibrations
mod.pH<-lm(c(pHData$pH4, pHData$pH7, pHData$pH10)~c(4,7,10)) # linear model

# Print a plot of the relationship between pH and mV - DO NOT HAVE TO DO THIS IF ALREADY DONE FOR CRM run of the day
# png(paste0(path,"/",Sys.Date(),'pHmvplot.png'), height = 400, width = 400)
# plot(c(4,7,10), c(pHData$pH4, pHData$pH7, pHData$pH10), xlab = 'pH', ylab = 'mv')
# lines(c(4,7,10), predict(mod.pH))
# R2<-summary(mod.pH)$r.squared
# legend('topright', legend = bquote(R^2 == .(format(R2, digits = 3))), bty='n')
# dev.off()

# Select mV for pH=3 and pH=3.5 based on probe calibration
pH35 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3.5
pH3 <- mod.pH$coefficients[1] + mod.pH$coefficients[2]*3

#### Titrations #####
# Create empty matrix to put TA values in 
nrows<-nrow(Mass) #need file length/number of rows
TA <- data.frame(matrix(nrow = nrows, ncol = 4))
rownames(TA)<-Mass$Sample.ID1[1:nrows]
colnames(TA)<-c("SampleID",'TA','Mass','Salinity') # changed Sample.ID1 to SampleID in the TA data frame only

# Run a for-loop to bring in titration files and calculate TA
## Read in mega concatenated titration results file 
filename <- file.path(path, titrationfile)
AllData <- read.csv(filename, sep=",", na.string="NA", as.is=T, skip=4)[ ,1:5]

# Identify rows starting with scope in column 1 of the titration file
sample_name_positions <- c(1,grep("^Scope", AllData[,1]), nrow(AllData))

# Parse through all the data in the one file ###
sample_names<-Mass$SampleID

# Create a list with all the sample IDs
sample_names_list <- list()
for (item in 1:length(sample_names)){
  sample_names_list[[item]] <- sample_names[item]
}

# fill the list with the data from each sample
for (i in 1:nrows){
  sample_names_list[[i]]<-data.frame(AllData[sample_name_positions[i]:sample_name_positions[i+1],])
  colnames(sample_names_list[[i]])<-c("Volume","Time","mV","Temperature","dV/dt")
}

for(i in 1:nrows) {
  
  #  Data<-read.csv(file.names[i], header=F, sep=",", na.string="NA",as.is=T, skip=10)[ ,1:5] 
  
  # colnames(Data) <-  c("Volume","Time",	"mV",	"Temperature",	"dV/dt")
  
  Data<-sample_names_list[[i]]
  
  # everything was brought in as a character because of the second line, converts back to numeric
  
  Data$mV<-suppressWarnings(as.numeric(Data$mV)) ## supress the warnings since NA will be produced
  
  Data$Temperature<-suppressWarnings(as.numeric(Data$Temperature))
  
  Data$Volume<-suppressWarnings(as.numeric(Data$Volume))
  
  #name of the file without .csv
  
  #name<-unlist(strsplit(file.names[i], split='.', fixed=TRUE))[1]
  
  name<-sample_names[i]
  
  
  
  #calculates the index of values between pH 2 and 3.5 
  
  mV<-which(Data$mV<pH3 & Data$mV>pH35) 
  
  
  
  #CHANGE ONLY WHEN NEW BOTTLE OF ACID IS USED----------------------------------
  
  #density of your titrant: change every time acid is changed
  
  #Batch A16 changed on 20190731 by SJG, SIlbiger used same batch
  #d<-(-0.00000410*mean(Data$Temperature[mV], na.rm=T)^2-0.0001065*mean(Data$Temperature[mV], na.rm=T)+1.02884) #20190731 Batch A16
  
  #Danielle Becker updated script and changed acid to new batch #A22 on 20220127
  
  d<-(-0.00000400*mean(Data$Temperature[mV], na.rm=T)^2-0.0001116*mean(Data$Temperature[mV], na.rm=T)+1.02881) #20220127 Batch A22 DMBP
  
  
  #concentration of your titrant: CHANGE EVERYTIME ACID IS CHANGED 
  
  #c<-0.100010 ##Batch A16 first used by SJG on 20190731
  
  c<-0.100347 ##Batch A22 first used by DMBP on 20220127
  
  
  
  
  
  #------------------------------------------------------------------------------
  
  
  
  #Salinity of your samples, set salinity for all titrations using a junk or CRM (single value)
  s<-Mass[Mass$SampleID==name,3]
  #s<-Mass[name,2]
  
  #mass of sample in g: changed with every sample
  #mass<-Mass[name,1]
  mass<-Mass[Mass$SampleID==name,2]
  #sample.index<-Mass[Mass$Sample.ID1==name,3]# this is the order that the sample was run
  #-------------------------------------------------------------------
  #Calculate TA
  
  #at function is based on code in saecarb package by Steeve Comeau, Heloise Lavigne and Jean-Pierre Gattuso
  TA[i,1]<-name #exports the sample ID into output file, column 1
  TA[i,2]<-1000000*seacarb::at(S=s,T=mean(Data$Temperature[mV], na.rm=T), C=c, d=d, pHTris=NULL, ETris=NULL, weight=mass, E=Data$mV[mV], volume=Data$Volume[mV])
  TA[i,3]<-mass #exports the mass into the TA output file, column 3 
  TA[i,4]<-s #exports the salinity column into the output file, column 4
}

## TA calculated! 
# make sure the appropriate columns are numeric
TA[,2:3]<-sapply(TA[,2:3], as.numeric) 

# Add date column 
TA$Date <- date

#exports your data as a CSV file
write.table(TA,paste0(path,"/","TA_Output_",titrationfile),sep=",", row.names=FALSE)

#Cumulative TA
cumu.data <- read.csv("Raw_data/TA/Cumulative_TA_Output.csv", header=TRUE, sep=",")
update.data <- rbind(cumu.data, TA)

#check that your new data has been appended to the cumulative TA dataframe (added 20220623 by LZ)
tail(update.data)

#export data as csv file
write.table(update.data,"Raw_data/TA/Cumulative_TA_Output.csv",sep=",", row.names=FALSE)


