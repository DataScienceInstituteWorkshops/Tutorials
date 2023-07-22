## MACHINE LEARNING 1
## Naive Bayes Algorithm

#Naive Bayes classifier assumes features are independent of each other. 
#Since that is rarely possible in real-life data, the classifier is called naive.
## dataset: "BANK LOAN.csv"
bankloan<-read.csv(file.choose())
str(bankloan)

bankloan$AGE<-factor(bankloan$AGE)

riskmodel<-glm(DEFAULTER~AGE+EMPLOY+ADDRESS+DEBTINC+CREDDEBT+OTHDEBT,
               family=binomial,data=bankloan)
summary(riskmodel)
riskmodel<-glm(DEFAULTER~EMPLOY+ADDRESS+DEBTINC+CREDDEBT,
               family=binomial,data=bankloan)

summary(riskmodel)

install.packages("ROCR")
library(ROCR) 

bankloan$predprob<-fitted(riskmodel)

pred<-prediction(bankloan$predprob,bankloan$DEFAULTER)

perf<-performance(pred,"tpr","fpr")

plot(perf)

abline(0,1)

auc<-performance(pred,"auc")

auc@y.values

## NAIVE BAYES ALGORITHM

install.packages("e1071")
library(e1071)

riskmodel2<-naiveBayes(DEFAULTER~AGE+EMPLOY+ADDRESS+DEBTINC+CREDDEBT+OTHDEBT,
                       data=bankloan)

riskmodel2
prednb<-predict(riskmodel2,bankloan,type='raw')
head(prednb)

pred<-prediction(prednb[,2],bankloan$DEFAULTER)
perf<-performance(pred,"tpr","fpr")
plot(perf)

abline(0,1)

auc<-performance(pred,"auc")
auc@y.values

## Laplace smoothing
# If a given class and feature value never occur together in the training data, 
# then the frequency-based probability estimate will be zero
# desirable to incorporate a small-sample correction, called pseudo-count, 
# in all probability estimates such that no probability is ever set to be exactly zero.
# regularising naive Bayes is called Laplace Smoothing 
# when the pseudo count is one
## dataset: Data for Laplace Smoothing.csv
data1<-read.csv(file.choose())

data1$X1<-as.factor(data1$X1)
str(data1)
model<-naiveBayes(Y~X1+X2+X3,data=data1)

laplacemodel<-naiveBayes(Y~X1+X2+X3,data=data1,laplace=2)

model
laplacemodel

## dataset: "New Data for Laplace Predictions.csv"
newdata1<-read.csv(file.choose())
newdata1

newdata1$X1<-as.factor(newdata1$X1)

prednew<-predict(laplacemodel,newdata1,type="raw")
## predicted probabilities

prednew1<-predict(laplacemodel,newdata1,type="raw",
                  threshold=0.1,eps=0.1)
## if predicted probability is less than the threshold 0.1,
## then replace it by epsilon 0.1
prednew

prednew1

## KNN Classifier
## dataset: "BANK LOAN KNN.csv"
bankloan<-read.csv(file.choose())
bankloan2<-subset(bankloan,select=c(-AGE,-SN,-DEFAULTER))
head(bankloan2)
bankloan3<-scale(bankloan2)
head(bankloan3)

install.packages("caret")
library(caret)

index<-createDataPartition(bankloan$SN,p=0.7,list=FALSE)
head(index)
traindata<-bankloan3[index,]
testdata<-bankloan3[-index,]

dim(traindata)

dim(testdata)
Ytrain<-bankloan$DEFAULTER[index]

Ytest<-bankloan$DEFAULTER[-index]
Ytrain

install.packages("class")
library(class)
model<-knn(traindata,testdata,k=20,cl=Ytrain)
table(Ytest,model)
class(model)

class(Ytest)

Ytest <- as.factor(Ytest)
class(Ytest)
library(caret)
confusionMatrix(Ytest,model)

## KNN for Regression
# KNN algorithm can also be extended to regression problems, 
# i.e. when the dependent variable is continuous
## Average value of the response variable for k neighbours 
# is calculated and assigned to the new case.




