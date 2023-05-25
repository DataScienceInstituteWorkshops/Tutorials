## Package data.table
install.packages("data.table")
library(data.table)
dt1<- data.table(ID=1:7000000,
                 Capacity=sample(100:1000,size=50,replace=F),
                 Code=sample(LETTERS[1:4],50,replace=T),
                 State=rep(c("Alabama","Indiana","Texas","Nevada")))
head(dt1, 4)
dt2<-dt1[Capacity>2500 & Capacity<5000]
head(dt2)
dt_order1<-dt1[order(Code,-State)]
head(dt_order1,4)
tail(dt_order1)
