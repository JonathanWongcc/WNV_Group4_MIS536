#Load libraries 
library(rpart)      #need it for recursive partitioning And regression trees
library(rpart.plot) #need it to plot rpart binary tree
library(caret)      # need it for confusion matrix
library(ggplot2)   
library(forecast)   #For time series chart
library(tidyverse)  ## %>%
library(MASS)       ##for stepAIC glm
library(gains)      #for gain chart

#Load csv file
WNV <- read.csv("WNV_R.csv", header = TRUE)

summary(WNV)
#run correlation on numerical variables
wnvnum<-WNV[!is.na(WNV$Wards),sapply(WNV,is.numeric)] #only numerical number
round(cor(wnvnum),2)  #Display correlation with 2 decimal number

#data visualization
WNV$SEASON.YEAR <- factor(WNV$SEASON.YEAR)
WNV$RESULTS <- factor(WNV$RESULT)
WNV$SPECIES <- factor(WNV$SPECIES)
WNV$WEEK <- factor(WNV$WEEK)

s <- ggplot(data = WNV)
s + geom_histogram(binwidth = 10, aes(x=NUMBER.OF.MOSQUITOES,
                                      fill=SPECIES), colour="Black") + xlab("Census of Mosquitoes") + ylab("Species") +
  ggtitle("West Nile Virus (WNV) Mosquito Test RESULT") +
  theme(axis.title.x = element_text(colour = "DarkGreen", size = 30),
        axis.title.y = element_text(colour = "Red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "DarkBlue", size = 40, family = "Times New Roman"))

s + geom_bar(aes(x=SPECIES), fill="Light Green", colour="Black")
s + geom_histogram(binwidth = 7, aes(x=Census.Tracts,
                                     fill=RESULT), colour="Black")

#Time Series Chart
WNV.ts <- ts(WNV$NUMBER.OF.MOSQUITOES, start = c(2006, 1), end = c(2019, 3), freq = 12)
plot(WNV.ts, xlab = "SEASON.YEAR", ylab = "Census.Tracts of Mosquitoes", ylim = c(0, 75))


summary(WNV$WEEK)
hist(WNV$NUMBER.OF.MOSQUITOES)
pie(table(WNV$RESULT), main= "Test Result of WNV")
barplot(table(WNV$SPECIES), main = "Barplot of Species")
barplot(table(WNV$TRAP_TYPE), main = "Barplot of Trap Type")
plot(WNV$LATITUDE,WNV$LONGITUDE, main = "Test location")
plot(WNV$SEASON.YEAR,WNV$TEST.ID, main = "Test VS Year")