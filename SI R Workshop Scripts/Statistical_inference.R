## Statistical Inference
## Parametric tests (sample is following normal distribution)
## Normality testing (normal distribution - bell shape curve)
## Q-Q norm plot (Normality Testing Data.csv)
data<-read.csv(file.choose())
qqnorm(data$csi, col="blue")
qqnorm(data$billamt, col="red")

## Shapiro-Wilk test (check p value)
## H0: Sample is drawn from normal population
shapiro.test(data$csi)
## p value=0.9038, >0.05, Accept null hypothesis
shapiro.test(data$billamt)
## p-value = 4.858e-06, <0.05, reject null hypothesis
## the sample is drawn from a population which is not normally distributed

## Kolmogorov-Smirnov test
install.packages("nortest")
library(nortest)
lillie.test(data$csi)
lillie.test(data$billamt)

## one sample t-test ("ONE SAMPLE t TEST.csv")
## H0: average time taken to complete mis report is = 90 minutes
data<-read.csv(file.choose(),header=TRUE)
t.test(data$Time, alternative="greater", mu=90)
## p value=0.04074, <0.05, reject null hypothesis, H1 is true
## Inference: mean time to complete mis report is > 90 minutes

## Independent sample t-test ("INDEPENDENT SAMPLES t TEST.csv")
## H0: mean of two groups are equal
data<-read.csv(file.choose(), header=TRUE)
t.test(data$time_g1,data$time_g2, alternative="two.sided",
       var.equal=TRUE)
## p-value = 0.8251, >0.05 accept null hypotheses
## Inference: mean time of the two groups is the same

## Next session April 18, 2023

## Paired sample t-test ("PAIRED t TEST.csv")
## H0: there is no difference in avg time before and after training
data<-read.csv(file.choose(),header=TRUE)
t.test(data$time_before,data$time_after,
       alternative="greater", paired=TRUE)
## p-value = 4.919e-07, p-value < 0.05, reject null hypotheses
## Inference: average time taken after training is less than avg time before training

## Correlation t-test ("Correlation test.csv")
## H0: there is no significant relation between aptitude and job prof
data<-read.csv(file.choose(), header=TRUE)
cor.test(data$aptitude, data$job_prof, alternative="two.sided",
         method="pearson")
## p-value = 0.008517, p-value <0.05, reject H0
## there is a relation between aptitude and job_prof
## cor 0.5144107, moderate positive correlation between the variables 


## F-test for equality of variance ("F test for 2 variances.csv")
## H0: variances of time are equal in 2 groups
data<-read.csv(file.choose(),header=TRUE)
var.test(data$time_g1,data$time_g2,alternative = "two.sided")
## p-value = 0.4524, p-value>0.05 Accept H0
## Inference: Variance of time is same for both the groups

## One way ANOVA ("One way anova.csv")
## H0: mean satisfaction index is same for the three departments
data<-read.csv(file.choose(),header=TRUE)
anovatable<-aov(formula=satindex~dept, data=data)
summary(anovatable)
## p-value=0.193, >0.05, accept H0
## Inference: there is no significant difference in satisfaction index among 
## the three departments

## Two way ANOVA
## H01: Average satisfaction index is equal for 3  departments.
## H02: Average satisfaction index is equal for 2 experience levels.
## H03: Interaction effect(dept*exp) is not significant on satisfaction index.
##data<-read.csv("Two Way Anova.csv", header=TRUE)
anovatable<-aov(formula=satindex~dept+exp+dept*exp,data=data)
summary(anovatable)

## Three way ANOVA ("Three Way Anova.csv")
data<-read.csv(file.choose(), header=TRUE)
anovatable<-aov(formula=growth~campaign*region*size,data=data)
summary(anovatable)
## p-value = 0.2196, >0.05 accept H0

par(mfrow=c(1,3))

boxplot(growth~campaign,data=data,col="blue")
boxplot(growth~region,data=data,col="blue")
boxplot(growth~size,data=data,col="blue")

par(mfrow=c(1,1))
interaction.plot(data$campaign, data$region, data$growth)

## April 19, 2023
## Non Parametric tests
## Mann Whitney test (t-test for independent samples)("Mann Whitney test.csv")
## H0: The two samples come from the same population
data<-read.csv(file.choose(), header=TRUE)
wilcox.test(formula=aptscore~Group,data=data)
## p-value = 0.7308, >0.05, Accept H0
## Aptscore is same for both groups, samples are coming from the same population

## Wilcoxon Signed Rank test (t-test for paired samples)
## "Wilcoxon Signed Rank test for paired  data.csv"
## null hypothesis H0: median (score) of paired samples is same.
data<-read.csv(file.choose(),header=TRUE)
wilcox.test(data$Before, data$After, paired=TRUE,
            alternative = "less")
## p-value = 0.001709, <0.05, Reject H0
## Training program is effective, score after training > score before training

## Kruskal Wallis test (ANOVA) ("Kruskal Wallis Test.csv")
## Null Hypothesis (H0):  The three samples are from the same population
data<-read.csv(file.choose(),header=TRUE)
kruskal.test(formula=aptscore~Group,data=data)
## p-value = 0.3278, > 0.05, Accept H0
## Aptitude score is same for all three groups
## all three groups are chosen from the same population

## Chi-square test ("chi square test of association.csv")
## Null Hypothesis (H0):  performance and source are not associated
data<-read.csv(file.choose(), header=TRUE)
unique(data$performance)
unique(data$source)
install.packages("gmodels")
library(gmodels)
CrossTable(data$performance, data$source, chisq=TRUE)
## p =  2.635987e-22 < 0.05, Reject H0
## Recruitment source and performance are associated




