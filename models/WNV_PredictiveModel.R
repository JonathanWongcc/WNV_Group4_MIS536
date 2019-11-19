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

## Training and validation sets
WNV$isPos <- 1 * (WNV$RESULT == "positive") 
set.seed(1) 
wnv<- WNV[!is.na(WNV$Wards),-c(3,4,5,7,9,13)] 
dim(wnv)
set.seed(1)   #to consistence sample 
train.index <- sample(c(1:dim(wnv)[1]), dim(wnv)[1]*0.6)  #60:40 partition 
train <- wnv[train.index, ]     #60% training data
valid<- wnv[-train.index, ]    #40% validation data
head(train)


#Model 1 - Stepwise AIC regression
m1=glm(isPos~.,family=binomial,data=train)
summary(m1)
lmodel <- glm(isPos ~., data = train, family = binomial) %>% stepAIC(trace = FALSE)
summary(lmodel) #AIC: 6567.7

#Model 2: Manual variable selection Manual variable without Census Track and Historical Wards
options(digits = 4)
m2=glm(isPos~SEASON.YEAR+WEEK+TRAP_TYPE+NUMBER.OF.MOSQUITOES+LONGITUDE+SPECIES,family=binomial,data=train) 
summary(m2) #AIC: 6568

#Model 2: Manual variable selection Manual variable without Census Track,Species and Historical Wards
m3=glm(isPos~SEASON.YEAR+WEEK+TRAP_TYPE+NUMBER.OF.MOSQUITOES+LONGITUDE,family=binomial,data=train) 
summary(m3) #AIC: 6603.6


#Confusion Matrix for all three models
pred <- predict(lmodel, valid)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(valid$isPos))

pred <- predict(m2, valid)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(valid$isPos))

pred <- predict(m3, valid)
confusionMatrix(as.factor(ifelse(pred > 0.5, 1, 0)), as.factor(valid$isPos))


## Lift Chart
WNV$isPos <- 1 * (WNV$RESULT == "positive") # Convert Result from catogerial to 0 1 binary

pred <- predict(m2, valid)
gain <- gains(valid$isPos, pred, groups=100)

plot(c(0,gain$cume.pct.of.total*sum(valid$isPos))~
       c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid$isPos))~c(0, dim(valid)[1]), lty=2)


# Creates tree with max depth of 7 nodes and set the minimum number of records in a terminal node to 50, with all predictors
model <-rpart(isPos ~ ., data = train,  method = "class")
#control = rpart.control(maxdepth = 7,minbucket=50),
# "snipping" off the least important splits - pruning, best prune tree
pruned <- prune(model, cp = model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
# Count the number of leaves
length(pruned$frame$var[pruned$frame$var == "<leaf>"])
# Plot this rpart model, #type = 1 Label all nodes,#extra = 1 Display the number of observations #split.font - Font for the split labels. Default 2, bold
#varlen - less than 0 truncate variable names to the shortest length
prp(pruned, type = 1, extra = 1, split.font = 1, varlen = -10)  

# use predict() to make predictions for a valid data set.
model.pred <- predict(pruned, valid,type = "class")
# Plot confusion matrix of prediction tree VS actual
confusionMatrix(model.pred, as.factor(valid$isPos))
