## ADVANCED PREDICTIVE MODELLING ("BANK LOAN.csv")
data<-read.csv(file.choose(),header=TRUE)
str(data)
data$AGE<-factor(data$AGE)
str(data)

riskmodel<-glm(DEFAULTER~AGE+EMPLOY+ADDRESS+DEBTINC+CREDDEBT+OTHDEBT,
               family=binomial,data=data)
summary(riskmodel)
## variables EMPLOY+ADDRESS+DEBTINC+CREDDEBT are significant to the model as the p value <0.05

riskmodel<-glm(DEFAULTER~EMPLOY+ADDRESS+DEBTINC+CREDDEBT,
               family=binomial,data=data)

summary(riskmodel)

## odds ratio  is to measure the association between independent 
# variables and dependent variables
##  identify model coefficients using the coef function and estimate the 
# odds ratio by taking the antilog, the conf-int argument 
# calculates the confidence interval for the odds ratio of the model

(riskmodel)
exp(coef(riskmodel)) 
exp(confint(riskmodel))
cbind(coef(riskmodel),odds_ratio=exp(coef(riskmodel)),exp(confint(riskmodel))) 

## Interpretation: The odds ratio for CREDDEBT is approx.  1.77, which means for
# a one unit change in CREDDEBT, the odds of being a defaulter 
# will change by 1.77 folds

data$predprob<-round(fitted(riskmodel),2)
head(data,n=10)

classificationtable<-table(data$DEFAULTER,data$predprob > 0.3)
classificationtable

sensitivity<-(classificationtable[2,2]/(classificationtable[2,2]+classificationtable[2,1]))*100
sensitivity

specificity<-(classificationtable[1,1]/(classificationtable[1,1]+classificationtable[1,2]))*100
specificity

## Cut off value = 0.5, sensitivity 50.27322, specificity 92.6499
## Cut off value = 0.3, sensitivity 74.86339, specificity 80.27079
## Model is performing best with the cut off value of 0.3
## Sensitivity = (TP)/(TP+FN) measure of positive instances correctly predicted
## Specificity = (TN)/(TN+FP) measure of negative instances correctly predicted as negative
## Good model, high sensitivity and high specificity


## MODEL VALIDATION BLR
## HOLD OUT VALIDATION IN BLR
install.packages("caret")
library(caret)

data <- read.csv("BANK LOAN.csv", header=TRUE)
index<-createDataPartition(data$DEFAULTER, p=0.7, list=FALSE) 

dim(index)

traindata<-data[index,]
testdata<-data[-index,]

dim(traindata)

dim(testdata)

## Generate confusion matrix for train data
riskmodel<-glm(DEFAULTER~EMPLOY+ADDRESS+DEBTINC+CREDDEBT,
               family=binomial,data=traindata)

traindata$predprob<-predict(riskmodel,traindata,type='response')
traindata$predY<-ifelse(traindata$predprob>0.30,1,0)
traindata$predY<-factor(traindata$predY)
traindata$DEFAULTER<-factor(traindata$DEFAULTER)
confusionMatrix(traindata$predY,traindata$DEFAULTER,positive="1")

## Generate confusion matrix for train data
testdata$predprob<-predict(riskmodel,testdata,type='response')
testdata$predY<-ifelse(testdata$predprob>0.3,1,0)
testdata$predY<-factor(testdata$predY)
testdata$DEFAULTER<-factor(testdata$DEFAULTER)
confusionMatrix(testdata$predY,testdata$DEFAULTER,positive="1")

## K FOLD VALIDATION
library(caret)
kfolds<-trainControl(method="cv",number=4)

riskmodel<-train(as.factor(DEFAULTER)~EMPLOY+ADDRESS+
                   DEBTINC+CREDDEBT,data=data,method="glm",
                 family=binomial,trControl=kfolds)
riskmodel

library(caret)
data$pred<-riskmodel$finalModel$fitted.values
data$predY<-ifelse(data$pred>0.3,1,0)

data$predY<-factor(data$predY)
data$DEFAULTER<-factor(data$DEFAULTER)

confusionMatrix(data$predY,data$DEFAULTER,positive="1")



