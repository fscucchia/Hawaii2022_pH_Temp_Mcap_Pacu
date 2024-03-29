## Title: Mcap 2022 respiration and photosynthesis analysis and plotting  
## Author: Jill Ashey 
## Date created: 20230605

#### This script will analyze and plot photosynthesis and respiration rates for M. capitata larvae from Hawaii 2022 pH and temperature experiment
#### Modified from KW, SG, AH & HP scripts 

## Install packages if you dont already have them in your library
if ("tidyverse" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyverse') 
if ("car" %in% rownames(installed.packages()) == 'FALSE') install.packages('car') 
if ("lme4" %in% rownames(installed.packages()) == 'FALSE') install.packages('lme4') 
if ("lmerTest" %in% rownames(installed.packages()) == 'FALSE') install.packages('lmerTest') 
if ("scales" %in% rownames(installed.packages()) == 'FALSE') install.packages('scales') 
if ("cowplot" %in% rownames(installed.packages()) == 'FALSE') install.packages('cowplot') 
if ("ggplot2" %in% rownames(installed.packages()) == 'FALSE') install.packages('ggplot2') 
if ("effects" %in% rownames(installed.packages()) == 'FALSE') install.packages('effects') 
if ("emmeans" %in% rownames(installed.packages()) == 'FALSE') install.packages('emmeans') 
if ("multcomp" %in% rownames(installed.packages()) == 'FALSE') install.packages('multcomp') 

#load packages
library("ggplot2")
library("tidyverse")
library('car')
library('lme4')
library('lmerTest')
library('scales')
library('cowplot')
library('effects')
library('emmeans')
library('multcomp')
library('ggsignif')
library('ggpubr')


# Set working directory 
setwd("/Users/jillashey/Desktop/PutnamLab/Repositories/Hawaii2022_pH_Temp_Mcap_Pacu/")

# Load data for P and R rates generated by Respirometry_Extraction.R script
PRdata <- read.csv("output/Respiration/oxygen_P_R_calc_Mcap2022.csv")

# Calculate a gross photosynthesis : respiration (P:R) ratio. This calculation will provide a ratio that will show the amount of oxygen produced compared to consumed, which is an indicator of energy availability. A ratio >1 indicates more photosynthesis than respiration and a ratio <1 indicates less photosynthesis than respiration.   
PRdata$ratio<-abs(PRdata$GP.nmol.org.min)/abs(PRdata$R.nmol.org.min) #calculate ratio with absolute values of gross photosynthesis and respiration

#Examine plot of data to look for outliers.  
plot(PRdata$ratio)

# Remove outliers detected by values of P:R ratio data
PRdata <- PRdata %>%
  filter(ratio < 8)
plot(PRdata$ratio) # outliers are removed - removed about 6 samples 

# Remove any respiration values that were above 0 (respiration is oxygen consumption, so we expect values less than 0 for negative rates)
plot(PRdata$R.nmol.org.min)
PRdata<-PRdata%>%filter(R.nmol.org.min < 0) 

# Remove any respiration values that were outliers
PRdata<-PRdata%>%filter(R.nmol.org.min > -0.15) #Outliers are removed and values are removed where respiration is >0. Respiration is oxygen consumption, so we expect values less than 0 for negative rates.  
plot(PRdata$R.nmol.org.min)

# Arrange treatment order 
PRdata$Treatment <- factor(PRdata$Treatment, levels = c("Control", "Mid", "High"))


# Plot Respiration data as a boxplot 
r_plot <- ggplot(PRdata, aes(x = Treatment, y = abs(R.nmol.org.min), fill = Treatment)) +
  geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("R (nmol ", O[2], " larva"^-1, "min"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
r_plot # 2 outliers in high treatment

# Plot Net Photosynthesis data as a boxplot. Net photosynthesis is excess oxygen production after subtracting the oxygen consumed (respiration).   
np_plot <- ggplot(PRdata, aes(x = Treatment, y = P.nmol.org.min, fill = Treatment)) +
  geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  geom_signif(comparisons = list(c("Mid", "High")), map_signif_level=TRUE) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("Net P (nmol ", O[2], " larva"^-1, "min"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
np_plot

# Plot Gross Photosynthesis data as a boxplot. Gross photosynthesis is net photosynthesis rates plus the oxygen consumed through respiration. 
gp_plot <- ggplot(PRdata, aes(x = Treatment, y = GP.nmol.org.min, fill = Treatment)) +
  geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("Gross P (nmol ", O[2], " larva"^-1, "min"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
gp_plot

# Plot P:R ratio
ratio_plot <- ggplot(PRdata, aes(x = Treatment, y = ratio, fill = Treatment)) +
  geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("P:R")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
ratio_plot




## Analyze data 
### Respiration 
# Build ANOVA model and examine results for analysis of respiration.  
Rmodel1<-aov(R.nmol.org.min~Treatment, data=PRdata)
summary(Rmodel1) # results are not significant 

# Check assumptions of model for residual normality and variance.    
qqPlot(residuals(Rmodel1)) #check normality assumption
leveneTest(residuals(Rmodel1)~Treatment, data=PRdata) #check for homogeneity of variance 
# Data meets normality assumptions and passes homogenity of variance

# Test with non-parametric test 
kruskal.test(R.nmol.org.min~Treatment, data=PRdata) # Results are also not significant

# Conduct post-hoc test - not really necessary here bc no significance in the first place 
TukeyHSD(Rmodel1)
#emm<-emmeans(Rmodel1, ~Treatment)
#cld(emm)


### Net photosynthesis 
# Build ANOVA model and examine results for analysis of net photosynthesis.  
NPmodel1<-aov(P.nmol.org.min~Treatment, data=PRdata)
summary(NPmodel1) # results are significant 

# Check assumptions of model for residual normality and variance.    
qqPlot(residuals(NPmodel1)) #check normality assumption
leveneTest(residuals(NPmodel1)~Treatment, data=PRdata) #check for homogeneity of variance 
# Data meets normality assumptions and passes homogenity of variance

# Test with non-parametric test 
kruskal.test(P.nmol.org.min~Treatment, data=PRdata) # Results are not significant - almost significant

# Conduct post-hoc test - not really necessary here bc no significance in the first place 
TukeyHSD(NPmodel1)
#emm<-emmeans(NPmodel1, ~Treatment)
#cld(emm)


### Gross photosynthesis 
# Build ANOVA model and examine results for analysis of gross photosynthesis.   
GPmodel1<-aov(GP.nmol.org.min~Treatment, data=PRdata)
summary(GPmodel1) # results are significant 

# Check assumptions of model for residual normality and variance.    
qqPlot(residuals(GPmodel1)) #check normality assumption
leveneTest(residuals(GPmodel1)~Treatment, data=PRdata) #check for homogeneity of variance 
# Data meets normality assumptions and passes homogenity of variance

# Test with non-parametric test 
kruskal.test(GP.nmol.org.min~Treatment, data=PRdata) # Results are also significant

# Conduct post-hoc test - not really necessary here bc no significance in the first place 
TukeyHSD(GPmodel1)
#emm<-emmeans(GPmodel1, ~Treatment)
#cld(emm)


### P:R ratio 
# Build ANOVA model and examine results for analysis of gross photosynthesis.   
PRmodel1<-aov(ratio~Treatment, data=PRdata)
summary(PRmodel1) # results are not significant 

# Check assumptions of model for residual normality and variance.    
qqPlot(residuals(PRmodel1)) #check normality assumption
leveneTest(residuals(PRmodel1)~Treatment, data=PRdata) #check for homogeneity of variance 
# Data meets normality assumptions and passes homogenity of variance

# Test with non-parametric test 
kruskal.test(ratio~Treatment, data=PRdata) # Results are also significant

# Conduct post-hoc test - not really necessary here bc no significance in the first place 
TukeyHSD(PRmodel1)
#emm<-emmeans(PRmodel1, ~Treatment)
#cld(emm)







## Analysis and plotting in a different way
# Respiration
compare_means(R.nmol.org.min ~ Treatment, data = PRdata, method = "anova")
my_comparisons <- list(c("Control", "Mid"), c("Control", "High"), c("Mid", "High"))

r_plot <- ggplot(PRdata, aes(x = Treatment, y = abs(R.nmol.org.min), fill = Treatment)) +
  geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 0.021) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("R (nmol ", O[2], " larva"^-1, "min"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
r_plot


# Net photosynthesis
compare_means(P.nmol.org.min ~ Treatment, data = PRdata, method = "anova")
my_comparisons <- list(c("Control", "Mid"), c("Control", "High"), c("Mid", "High"))

np_plot <- ggplot(PRdata, aes(x = Treatment, y = P.nmol.org.min, fill = Treatment)) +
  geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 0.03) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  #geom_signif(comparisons = list(c("Mid", "High")), map_signif_level=TRUE) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("Net P (nmol ", O[2], " larva"^-1, "min"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
np_plot


# Gross photosynthesis
compare_means(GP.nmol.org.min ~ Treatment, data = PRdata, method = "anova")
my_comparisons <- list(c("Control", "Mid"), c("Control", "High"), c("Mid", "High"))

gp_plot <- ggplot(PRdata, aes(x = Treatment, y = GP.nmol.org.min, fill = Treatment)) +
  geom_hline(yintercept=0, linetype="dashed", color="black", size=0.75)+
  geom_boxplot() +
  stat_compare_means(method = "anova", label.y = 0.04) +
  stat_compare_means(comparisons = my_comparisons, method = "t.test") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "pink")) +
  ylab(expression(bold(paste("Gross P (nmol ", O[2], " larva"^-1, "min"^-1, ")")))) +
  xlab("") +
  theme_classic() + 
  theme(axis.text = element_text(size=12, color="black"), 
        axis.title=element_text(size=12, color="black", face="bold")) +
  theme(legend.position = "none") 
gp_plot


# Save all plots
plots<-plot_grid(r_plot, np_plot, gp_plot, nrow=1, ncol=3, label_y=1, labels=c("A", "B", "C"), label_size=18)
ggsave(plots, file="output/Respiration/Respiration_panel.png", width=25, height=10)

