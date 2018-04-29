### GSS Data Analysis for Statistical Inference project

# load libraries

library(dplyr)
library(ggplot2)
library(olsrr)
library(tidyr)
library(gplots)

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
  filter(race != "Other" & race !="White") %>%
  select(region, coninc)

# look at data for questions

dim(mydata)

sum(is.na(mydata$coninc))
sum(is.na(mydata$region))

# get rid of N/As

completes<-complete.cases(mydata)
mydata<-mydata[completes,]


#  Check counts of respondents by region

mydatasum<-mydata %>%
  group_by(region) %>%
  summarise(n=n())

head(mydatasum, 9)

# ANOVA Conditions

# Independence within groups
# Independence between groups
# Near normal distributions (log transform?)
# Similar variance between groups (homoskedactic)

# EDA with mydata

sum1<- mydata %>%
  group_by(region) %>%
  summarise(Q1=quantile(coninc, .25), Mean=mean(coninc), Median=median(coninc),
            Q3=quantile(coninc, .75), IQR=IQR(coninc), StDev=sd(coninc)) %>%
  mutate(Skew=ifelse(Mean>Median, "Right", "Left"))

head(sum1, 9)

p1<-ggplot(mydata, aes(x=factor(region), y=coninc, fill=factor(region)))+
  geom_boxplot()+ggtitle("Constant Income by Region")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  xlab("Region")+ylab("Constant Annual Income")
p1

# Look at normality of Constant Income distributions by Region 

NE<-mydata %>% filter(region=="New England")
MA<-mydata %>% filter(region=="Middle Atlantic")
ENC<-mydata %>% filter(region=="E. Nor. Central")
WNC<-mydata %>% filter(region=="W. Nor. Central")
SA<-mydata %>% filter(region=="South Atlantic")
ESC<-mydata %>% filter(region=="E. Sou. Central")
WSC<-mydata %>% filter(region=="W. Sou. Central")
MT<-mydata %>% filter(region=="Mountain")
PC<-mydata %>% filter(region=="Pacific")

png(file='plot1.png')

par(mfrow=c(3,3))

qqnorm(NE$coninc, main = "New England")
qqline(NE$coninc)

qqnorm(MA$coninc, main = "Mid Atlantic")
qqline(MA$coninc)

qqnorm(ENC$coninc, main = "East North Central")
qqline(ENC$coninc)

qqnorm(WNC$coninc, main = "West North Central")
qqline(WNC$coninc)

qqnorm(SA$coninc, main = "South Atlantic")
qqline(SA$coninc)

qqnorm(ESC$coninc, main = "East South Central")
qqline(ESC$coninc)

qqnorm(WSC$coninc, main = "West South Central")
qqline(WSC$coninc)

qqnorm(MT$coninc, main = "Mountain")
qqline(MT$coninc)

qqnorm(PC$coninc, main = "Pacific")
qqline(PC$coninc)

dev.off()

par(mfrow=c(1,1))

# Test for heteroscedasticity

model<-lm(coninc~region, data=mydata)

ols_test_f(model, rhs = T)
ols_test_score(model, rhs = T)


# log transform

mydata$coninc<-log(mydata$coninc)

# EDA after log transform

sum2<- mydata %>%
  group_by(region) %>%
  summarise(Q1=quantile(coninc, .25), Mean=mean(coninc), Median=median(coninc),
            Q3=quantile(coninc, .75), IQR=IQR(coninc), StDev=sd(coninc)) %>%
  mutate(Skew=ifelse(Mean>Median, "Right", "Left"))

head(sum2, 9)

p2<-ggplot(mydata, aes(x=factor(region), y=coninc, fill=factor(region)))+
  geom_boxplot()+ggtitle("Natural Log of Constant Income by Region")+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  xlab("Region")+ylab("Log of Constant Annual Income")

p2

# Look at normality of Constant Income distributions by Region after log transform

NE<-mydata %>% filter(region=="New England")
MA<-mydata %>% filter(region=="Middle Atlantic")
ENC<-mydata %>% filter(region=="E. Nor. Central")
WNC<-mydata %>% filter(region=="W. Nor. Central")
SA<-mydata %>% filter(region=="South Atlantic")
ESC<-mydata %>% filter(region=="E. Sou. Central")
WSC<-mydata %>% filter(region=="W. Sou. Central")
MT<-mydata %>% filter(region=="Mountain")
PC<-mydata %>% filter(region=="Pacific")

png(file='plot2.png')

par(mfrow=c(3,3))

qqnorm(NE$coninc, main = "New England")
qqline(NE$coninc)

qqnorm(MA$coninc, main = "Mid Atlantic")
qqline(MA$coninc)

qqnorm(ENC$coninc, main = "East North Central")
qqline(ENC$coninc)

qqnorm(WNC$coninc, main = "West North Central")
qqline(WNC$coninc)

qqnorm(SA$coninc, main = "South Atlantic")
qqline(SA$coninc)

qqnorm(ESC$coninc, main = "East South Central")
qqline(ESC$coninc)

qqnorm(WSC$coninc, main = "West South Central")
qqline(WSC$coninc)

qqnorm(MT$coninc, main = "Mountain")
qqline(MT$coninc)

qqnorm(PC$coninc, main = "Pacific")
qqline(PC$coninc)

dev.off()

par(mfrow=c(1,1))

# Test for heteroscedasticity

model<-lm(coninc~region, data=mydata)

ols_test_f(model, rhs = T)
ols_test_score(model, rhs = T)

# ANOVA on log transformed data

aov<-aov(coninc~region, mydata)

summary(aov)

drop1(aov,~.,test="F")

TukeyHSD(aov)

# AOV shows differences between groups move on to pair wise t-tests

# Bonferroni correction

k<-nlevels(mydata$region)

bc<-k*(k-1)/2

bcsig<-.05/bc

bcsig

# t tests

ttable<-pairwise.t.test(mydata$coninc, mydata$region, p.adj="bonf")

ttable

