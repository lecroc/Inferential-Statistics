### GSS Data Analysis for Statistical Inference project

# load libraries

library(dplyr)
library(ggplot2)
library(olsrr)

# load data

load('gss.Rdata')

# look at raw data

str(gss)

dim(gss)

sum(is.na(gss))

# select the columns i'm interested for my research question
# Remove "Other" as an option for race

mydata<-gss %>%
  select(race, coninc, region) %>%
  filter(race != "Other" & race !="White")

# look at data for questions

dim(mydata)

sum(is.na(mydata$race))
sum(is.na(mydata$coninc))
sum(is.na(mydata$region))

# get rid of N/As

completes<-complete.cases(mydata)
mydata<-mydata[completes,]

#  Check counts of respondents by region

mydatasum<-mydata %>%
  group_by(region, race) %>%
  summarise(n=n())

head(mydatasum, 12)

# ANOVA Conditions

# Independence within groups
# Independence between groups
# Near normal distributions (log transform?)
# Similar variance between groups (homoskedactic)

mydata$coninc<-log(mydata$coninc)

sum1<- mydata %>%
  group_by(region) %>%
  summarise(Q1=quantile(coninc, .25), Mean=mean(coninc), Median=median(coninc),
            Q3=quantile(coninc, .75), IQR=IQR(coninc), StDev=sd(coninc)) %>%
  mutate(Skew=ifelse(Mean>Median, "Right", "Left"))

head(sum1, 9)

p1<-ggplot(mydata, aes(x=factor(region), y=coninc, fill=factor(region)))+geom_boxplot()+ggtitle("Constant Income by Region")

p1

# Test for heteroscedasticity

model<-lm(coninc~region, data=mydata)

ols_test_f(model, rhs = T)

# Look at normality of Constant Income distributions by Region

aov<-aov(coninc~region, mydata)

summary(aov)

# AOV shows differences between groups move on to pair-wise tests

# Bonferroni correction

k<-nlevels(mydata$region)

bc<-k*(k-1)/2

bcsig<-.05/bc




