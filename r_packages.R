## dplyr package
install.packages("dplyr")
library(dplyr)
data<-read.csv(file.choose())

## creating subset using filter and select
data1<-filter(data,Grade=="GR1",Location=="MUMBAI")
head(data1)
data2<-select(data,First_Name, Location, ba)
head(data2)
data3<-select(data,-(Last_Name:Location))
head(data3)

## arrange
a1<-arrange(data,desc(ba))
head(a1)
a2<-arrange(data,Grade,Location,ba)
head(a2)

## mutate, creating new column
m1<-mutate(data, total=ba+ms)
head(m1)
m2<-mutate(data, total=ba-ms)
head(m2)

## summarise and group_by
summarise(data,meanba=mean(ba,na.rm=TRUE),
          medianba=median(ba,na.rm=TRUE))
loc_wise<-group_by(data,Location)
summarise(loc_wise,count=n(),mean=mean(ba,na.rm=TRUE))

## pipe operator
data %>% 
  select(First_Name,Grade,Location) %>% 
  filter(Location=="MUMBAI")

sample_n(data,7)
sample_frac(data,0.2)

## Rank
data %>%
  mutate(Rank=dense_rank(desc(ba)))%>%
  arrange(Rank) %>%
  head()

## package data.table
install.packages("data.table")
library(data.table)
dt1<- data.table(ID=1:7000000,
                 Capacity=sample(100:1000,size=50,replace=F),
                 Code=sample(LETTERS[1:4],50,replace=T),
                 State=rep(c("Alabama","Indiana","Texas","Nevada")))
head(dt1, 4)
## creating subset using row indexing
subset1<-dt1[3:11,]
head(subset1)
subset2<-dt1[Code=="B" & State=="Texas"]
head(subset2)

## creating subset using columns
sub_columns<-dt1[,.(ID,Capacity)]
head(sub_columns, 3)

setkey(dt1,Code,State)
sub_key<-dt1[.("C","Alabama")]
head(sub_key, 2)

sub_key2<-dt1[.("C")]
head(sub_key2)
sub_key3 <- dt1[.(unique(Code),"Alabama")]
head(sub_key3)

## sorting
dt_order1<-dt1[order(Code,-State)]
head(dt_order1)
dt_order2<-dt1[order(-Code,-Capacity)]
head(dt_order2)

## Modifying data, add a column
dt1[,new_capacity:=Capacity+5]
head(dt1)
dt1[State=="Alabama",State:="Al"]
head(dt1)
## delete a column
dt1[,c("Capacity"):=NULL]
head(dt1,2)
## chaining of commands
dt1[,new_capacity:=Capacity + 5] [State=="Alabama",State:="Al"] [,"Capacity":=NULL]
head(dt1)
## renaming
setnames(dt1,old="new_capacity" , new = "New_Capacity")
head(dt1,2)
setkey(dt1, New_Capacity,State)

## aggregate function
DT_agg <- dt1[,sum(New_Capacity),by=State]
DT_agg
DT_agg <- dt1[,.(Totalcapacity=sum(New_Capacity)),by=State]
DT_agg

## Package tidyr
install.packages("tidyr")
library(tidyr)
stud_data<-read.csv(file.choose())

# gather(stud_data), convert wide format to long format
longformat<-gather(stud_data,Subjects,Marks,Maths,Economics,Statistics)
longformat
spread(longformat,Subjects,Marks,fill=0)

## separate function
empid<-c(101,102,103,104)
location<-c("Mumbai","Delhi","Delhi","Mumbai")
address<-c("4/Churchgate","12/Rohini","8/Pitampura", "21/Andheri")
date<-c("2016-10-09","2010-11-01","2009-09-23","1990-02-30")
empdata<-data.frame(empid,location,address,date)

empdata
sep_date<-separate(empdata,date,into=c("Year","Month","Date"))
sep_date
sep_address<-separate(empdata,address,into=c("sector","area"),sep="/", convert=TRUE)
sep_address
unite_date<-unite(sep_date,date,c(Year,Month,Date),sep="/")
unite_date

