## Data Visualisation
## import the two data sets, telecom demo and telecom transaction
demographic<-read.csv(file.choose())
transaction<-read.csv(file.choose())
head(demographic)
head(transaction)
tcalls<-aggregate(Calls~CustID, data=transaction, FUN=sum)
head(tcalls)
working<-merge(demographic, tcalls, by=("CustID"), all=TRUE)
head(working)

## create new column- age_group (18-30, 30-45, >45)
working$age_group<-cut(working$Age, breaks=c(0,30,45,Inf), 
                       labels=c("18-30","30-45",">45"))
head(working)

## Create a simple bar chart using ggplot2 library
install.packages("ggplot2")
library(ggplot2)
ggplot(working , aes ( x = age_group)) + geom_bar() 
ggplot(working,aes(x=age_group,y=Calls))+
  geom_bar(stat="identity",fill="green")+
  labs(x="Age Groups",y="Total Calls",title="Bar Diagram")

## reorder the bars
ggplot(working,aes(reorder(age_group,Calls),Calls))+
  geom_bar(stat="identity",fill="red")+
  labs(x="Age Groups",y="Total Calls", title="Bar Diagram") 

ggplot(working,aes(x=Gender,y=Calls))+
  geom_bar(stat="identity",fill="maroon")+
  labs(x= "Gender" ,y= "Total Calls",title="Bar Diagram")

## Horizontal bar chart
ggplot(working,aes(x=age_group,y=Calls))+
  geom_bar(stat="identity",fill="orange")+
  labs(x="Age Groups",y="Total Calls",title="Bar Diagram")+
  coord_flip()

## Stacked bar chart
ggplot(working, aes(x=age_group))+
  geom_bar(aes(fill=Gender))+ 
  labs(x = "Age Group", y="No. of customers", title="Stacked bar chart")

## Pie Chart
pie_table<-table(working$age_group)
pie_table
pie_dataframe<-as.data.frame(pie_table)
names(pie_dataframe)[1]<-"age_group"
names(pie_dataframe)[2]<-"Count"
pie_dataframe
pct <- round(pie_dataframe$Count/sum(pie_dataframe$Count)*100)

pie_dataframe$group_percent <- paste0(pie_dataframe$Age_group," ",pct,"%")
pie(pie_dataframe$Count,labels = pie_dataframe$group_percent, 
    col=rainbow(length(pie_dataframe$group_percent)),
    main="Pie Chart of Age Groups")

## Box and Whisker Plot
boxplot(working$Calls, data= working, 
        main="BOX PLOT (Total Calls)", ylab= "Total Calls", col= "cadetblue3")

boxplot(Calls~age_group,data=working, 
        main="BOX PLOT – (Total Calls – Age Group)",
        xlab="Age Group",ylab="Total Calls", 
        col=c("orange","green","cadetblue"))

## Histogram
hist(working$Calls, breaks=12, main = "HISTOGRAM – Total Calls", 
     xlab = "Total Calls", ylab = "No. of Customers", col="darkorange")
## Density Plot
working_den<-density(working$Calls) 
plot(working_den, main="DENSITY PLOT - Calls",xlab="Calls")
polygon(working_den, col="yellowgreen")

## Stem and Leaf chart
stem(working$Calls)

## Pareto chart

install.packages("RColorBrewer")
library(RColorBrewer)
##col=brewer.pal(n,"palette")
install.packages("qcc")
library(qcc)

telecom1<-aggregate(Calls~age_group,data = working, FUN=sum)

pareto.chart(telecom1$Calls, xlab= "Age Groups" ,ylab= "Total Calls" ,           
             main = "PARETO CHART : Age Group",col=brewer.pal(3,"Blues"), 
             names.arg=telecom1$age_group)

install.packages("ggplot2")
library(ggplot2)
ggplot(working, aes(x=Calls))+ geom_histogram(binwidth=100, fill="darkorange")+
  labs(x="Total Calls", y="No. of customers", title="HISTOGRAM")

