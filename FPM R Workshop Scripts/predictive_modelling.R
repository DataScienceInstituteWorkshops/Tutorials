## PREDICTIVE MODELLING
## Multiple Linear Regression ("Performance Index.csv")
perindex<-read.csv(file.choose(),header=TRUE)
install.packages("GGally")
library(GGally)

ggpairs(perindex[,c("jpi","aptitude","tol","technical","general")],
        title="Scatter Plot Matrix",
        columnLabels=c("jpi","aptitude","tol","technical","general"))

jpimodel<-lm(jpi~aptitude+tol+technical+general, data=perindex)
jpimodel
summary(jpimodel)
## aptitude, technical and general are significant variables for the 
## model (p-value<0.05)
## tol is not significant (p-valu>0.05)
## drop tol, create new model with significant variables only

jpimodel_new<-lm(jpi~aptitude+technical+general,data=perindex)
summary(jpimodel_new)

## predicting jpi (Y), given data
perindex$pred_jpi<-fitted(jpimodel_new)

perindex$resi<-residuals(jpimodel_new)

## Predictions for New Dataset ("Performance Index new.csv")
perindex_new<-read.csv(file.choose(), header=TRUE)

perindex_new$pred<-predict(jpimodel_new,perindex_new)

head(perindex_new)
predict(jpimodel_new, perindex_new, interval="confidence")
jpimodel_new
## April 20, 2023
## Generation of standardized parameter estimate
install.packages("lm.beta")
library(lm.beta)

lm.beta(jpimodel_new)

## MLR with Categorical Dummy Variables in R
## "RESTAURANT SALES DATA.csv"
restaurantsales<-read.csv(file.choose(),header=TRUE)

str(restaurantsales)
unique(restaurantsales$LOCATION)
salesmodel<-lm(SALES~NOH+LOCATION,data=restaurantsales)
summary(salesmodel)

restaurantsales$LOCATION<-relevel(restaurantsales$LOCATION,ref="mall")
## Fitting model on data with reordered levels
salesmodel2<-lm(SALES~NOH+LOCATION, data=restaurantsales)

summary(salesmodel2)

## Detecting Multicollinearity 
perindex<-read.csv("Performance Index.csv", header=TRUE)
jpimodel<-lm(jpi~aptitude+tol+technical+general, data=perindex)
install.packages("car")
library(car)
vif(jpimodel)
## all the vif values are < 5, multicollinearity does not exist in the model
## in case vif>5, multicollinearity exists, drop the variable whose vif>5

## Residuals vs Predicted plot
perindex<-read.csv("Performance Index.csv",header=TRUE)
jpimodel<-lm(jpi~aptitude+tol+technical+general, data=perindex)
perindex$pred_jpi<-fitted(jpimodel)
perindex$resi<-residuals(jpimodel)
plot(perindex$pred_jpi,perindex$resi,col="blue")

qqnorm(perindex$resi,col="blue")

qqline(perindex$resi,col="blue")
## Null Hypothesis (H0):  Sample is drawn from Normal Population
## Alternate Hypothesis (H1): Not H0

shapiro.test(perindex$resi)
## p-value = 0.1588, > 0.05, Accept H0

## CROSS VALIDATION
## "Motor_Claims.csv"
motor<-read.csv(file.choose(),header=TRUE)
library(GGally)

ggpairs(motor[,c("vehage","CC","Length","Weight","claimamt")], title="Scatter Plot Matrix" , 
        columnLabels = c("vehage","CC","Length","Weight","claimamt"))

motor_model<-lm(claimamt~Length+CC+vehage+Weight, data=motor)
summary(motor_model)
library(car)
## check for multicollinearity
vif(motor_model)
## vif of weight is 6.55 > 5, multicollinearity exists, drop weight

motor_model1<-lm(claimamt~Length+CC+vehage,data=motor)
summary(motor_model1)
vif(motor_model1)
## all the vif values are < 5, multicollinearity does not exist

motor$res<-residuals(motor_model1)
RMSEmotor<-sqrt(mean(motor$res**2))
RMSEmotor
## RMSE = 11444.51 of the motor_model1

## 1. HOLD OUT CROSS VALIDATION
motor <- read.csv("Motor_Claims.csv", header=TRUE)

install.packages("caret")
library(caret)

index <- createDataPartition(motor$claimamt,p=0.8,list=FALSE) 

head(index)
dim(index)
traindata<-motor[index,]
testdata<-motor[-index,]

dim(traindata)
dim(testdata)

motor_trn_model<-lm(claimamt~Length+CC+vehage,data=traindata)

traindata$res<-residuals(motor_trn_model)
head(traindata)

RMSEtrain<-sqrt(mean(traindata$res**2))
RMSEtrain
## RMSE = 11467.4 of train data 
## RMSE = 11444.51 of the motor_model1

testdata$pred<-predict(motor_trn_model,testdata)

testdata$res<-(testdata$claimamt-testdata$pred)
RMSEtest<-sqrt(mean(testdata$res**2))
RMSEtest
## RMSE = 11422.27 of test data
## all the three RMSE values are close to each other, model is stable

## 2. K-Fold Cross Validation
kfolds<-trainControl(method="cv",number=4)
model<- train(claimamt~vehage+CC+Length,data=motor,method="lm",trControl=kfolds)
model

## 3. Repeated K-Fold Cross Validation
kfolds<-trainControl(method="repeatedcv",number=4,repeats=5)

model<- train(claimamt~vehage+CC+Length,data=motor,method="lm",trControl=kfolds)

model

## 4. LOOCV
kfolds<-trainControl(method="LOOCV")
model<- train(claimamt~vehage+CC+Length,data=motor,method="lm",trControl=kfolds)
model

## 5. Bootstrapping in R
kfolds<-trainControl(method="boot")
model<- train(claimamt~vehage+CC+Length,data=motor,method="lm",trControl=kfolds)
model


