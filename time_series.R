## TIME SERIES (turnover_annual.csv)
turnover_annual <- read.csv(file.choose(),header=TRUE)
salesseries<-ts(turnover_annual$sales,start=1961,end=2017)
## Line plot
plot(salesseries,col="red",main="Sales Time Series (Simple Plot)")
## create a subset (window)
salesseries2<-window(salesseries,start=1990,end=2017)
plot(salesseries2, col="red", main="Sales Time Series (Subset)")

## Time Series which are not evenly spaced,R has two highly effective 
# packages for handling unevenly spaced time series, zoo and xts.

## "Sales Data for 3 Years.csv" (uneven time series data)
salesdata<-read.csv(file.choose(),header=TRUE)
salesseries<-ts(salesdata$Sales,start=c(2013,1), end=c(2015,12), frequency=12)
## decomposing the time series, trend, seasonality, random
decomp<-decompose(salesseries)  
plot(decomp)
decomp$seasonal
decomp$trend
decomp$random
seasadj <- salesseries - decomp$seasonal
plot(seasadj)

## Time Series Decomposition – Local Regression Method (LOESS)

decomp<-stl(salesseries,s.window="periodic")
plot(decomp)

## turnover_annual <- read.csv("turnover_annual.csv",header=TRUE)
## salesseries<-ts(turnover_annual$sales,start=1961,end=2017)

plot(salesseries,col="red",main="Sales Time Series (Simple Plot)")
## autocorrelation function
acf(salesseries,col="blue")

## Differencing (convert non stationary ts to stationary ts)
salesdiff<-diff(salesseries,difference=1)

plot(salesdiff,main="Sales Differenced Time Series")
acf(salesdiff,col="blue")

install.packages("forecast")
library(forecast)
ndiffs(salesseries)

salesdiff2<-diff(salesseries,differences=2)
plot(salesdiff2,col="red")
acf(salesdiff2,col="blue")

## Analytical Method – Dickey Fuller (DF) Test
install.packages("urca")
library(urca)

df<-ur.df(salesseries, lag=0)
summary(df)
df2 <- ur.df(salesdiff2,lags = 0)
summary(df2)

## Step 1: Stationarity Checking
turnover_annual <- read.csv("turnover_annual.csv",header=TRUE)
salesseries<-ts(turnover_annual$sales,start=1961,end=2017)
plot(salesseries,col="red",main="Sales Time Series (Simple Plot)")

df<-ur.df(salesseries, lag=0)
summary(df)
#install.packages("forecast")
#library(forecast)
ndiffs(salesseries)
## ndiffs() function determines how many time differencing needs to be done

salesdiff2<-diff(salesseries,differences=2)
plot(salesdiff2,col="red")
acf(salesdiff2,col="blue")

## Step 2: Model Identification

## Step 3: Parameter Estimation
salesmodel<-arima(salesseries,order=c(2,2,2))
coef(salesmodel)
AIC(salesmodel)
install.packages("forecast")
library(forecast)

auto.arima(salesseries,d=2,max.p=3,max.q=2,trace=TRUE,ic="aic")
salesmodel<-arima(salesseries,order=c(2,2,1))
coef(salesmodel)
AIC(salesmodel)

## Step 4: Diagnostic Checking
resi<-residuals(salesmodel)
Box.test(resi)
plot(resi,col="red")

## Step 5: Forecasting for next 3 years
predict(salesmodel,n.ahead=3)

