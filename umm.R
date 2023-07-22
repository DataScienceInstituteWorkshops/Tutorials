## UNSUPERVISED MULTIVARIATE METHODS
## PRINCIPAL COMPONENT ANALYSIS ("Athleticsdata.csv")
data<-read.csv(file.choose(), header=TRUE)
athletics<-subset(data,select=c(-Country))

pc<-princomp(formula=~.,data=athletics,cor=T)

summary(pc)
## cor=T indicates that calculations should be done using the Correlation Matrix. It is equivalent to standardization. 
pc$loadings

data$performance<-pc$score[,1]
head(data)

## Scree Plot
plot(pc, type="lines")
plot(data$performance)
text(data$performance,label=data$Country,col="red",cex=0.5)
## Interpretation:Athletics from country Cook islands and Western Samoa are 
##performing low since, their score are highest.(lower the  score ,better is the performance).

bottom3<-head(data[order(-data$performance),],3)
bottom3

top3<-tail(data[order(-data$performance),],3)
top3

## Interpretation:
# USA, Britain and Italy are the top three performing countries.
# Cook Islands, Western Samoa and Mauritius are the bottom three countries.

round(cor(pc$scores))
## Interpretation:principal components are  uncorrelated

## PRINCIPAL COMPONENT REGRESSION ("pcrdata.csv")
## Remedial measure for Multicollinearity
salesdata<-read.csv(file.choose(),header=T)
predsales<-lm(SALES~AD+PRO+SALEXP+ADPRE+PROPRE,data=salesdata)
summary(predsales)

## checking for multicollinearity
install.packages("car")
library(car)

vif(predsales)
## Interpretation:
#VIF values are very high (>5, except for SALEEXP)  indicating severe multicolinearity problem.


salesdatapca<-subset(salesdata,select=c(-SRNO,-SALES))

pc<-princomp(formula=~.,data=salesdatapca, cor=T)
summary(pc)
install.packages("pls")
library(pls)
pcmodel<-pcr(SALES~AD+PRO+SALEXP+ADPRE+PROPRE,ncomp=3,data=salesdata,scale=TRUE)

salesdata$pred_pcr<-predict(pcmodel,salesdata,ncomp=3)
head(salesdata)

## Comparing Linear Regression Model and PCR model on Test data
## "pcrdata_test.csv"
salesdata_test<-read.csv(file.choose(),header=TRUE)

# Getting RMSE of linear regression model
salesdata_test$lmpredict<-predict(predsales,salesdata_test)
salesdata_test$lmres<-(salesdata_test$SALES-salesdata_test$lmpredict)
RMSE_lm<-sqrt(mean(salesdata_test$lmres)**2)
RMSE_lm

# Getting RMSE of PCR model
salesdata_test$pcrpredict<-predict(pcmodel,salesdata_test,ncomp=3)
salesdata_test$pcrres<-(salesdata_test$SALES-salesdata_test$pcrpredict)
RMSE_pcr<-sqrt(mean(salesdata_test$pcrres)**2)

head(salesdata_test)
RMSE_l
RMSE_pcr

## FACTOR ANALYSIS ("Athleticsdata.csv")
data<-read.csv(file.choose(), header=TRUE)
athletics<-subset(data,select=c(-Country))

fact<-factanal(athletics,2,rotation="varimax",scores="regression")
fact
## Interpretation:Two factors explain 92% of common variance.
## Factor 1 can be termed as ‘Stamina’ and factor 2 as ‘Speed’

data$stamina<-fact$score[,1]
data$speed<-fact$score[,2]
plot(data$stamina,data$speed)
text(data$stamina,data$speed,data$Country,cex=0.6, pos=4,col="red")

## Hierarchical Clustering("Town Insurance.csv")
townins<-read.csv(file.choose(),header=T)
townins2<-subset(townins,select=c(-Town))

# Calculating distance matrix (euclidean distance formula)
d<-dist(townins2, method="euclidean")
d
fit <- hclust(d, method="single")
## Plot dendrogram
plot(fit,label=townins$Town,col="blue")

townins$segment<-cutree(fit,k=3)
townins

## K Means clustering (Non Hierarchical)
## "RETAILERS DATA.csv"
custsales<-read.csv(file.choose(),header=T)
custsales_cl<-subset(custsales,select=c(-Custid,-region))

# Scale (standardize) all variables.(subtract mean and divide by standard deviation)
custsales_cl<-scale(custsales_cl)
## k=3
CL<-kmeans(custsales_cl,3)
CL 

custsales$segment <- CL$cluster
head(custsales)
aggregate(cbind(nsv,n_brands,n_bills,growth)~segment,data=custsales, FUN=mean)

## K-Means (Elbow Method)

install.packages("factoextra")
library(factoextra)
fviz_nbclust(custsales_cl, kmeans,method = "wss")

## Package fpc:Flexible Procedures for Clustering
## Performs K-means method for different values of ‘K’ and provides best value of K.
install.packages("fpc")
library(fpc)
CL1<-kmeansruns(custsales_cl,krange=2:10)
CL1$bestk

## K-Median Clustering in R
install.packages("flexclust")
library(flexclust)

kmedian<-kcca(custsales_cl,3,family=kccaFamily("kmedian"))
kmedian

custsales$seg_median <- kmedian@cluster
head(custsales)





