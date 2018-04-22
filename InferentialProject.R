### GSS Data Analysis for Statistical Inference project

# load libraries

library(dplyr)
library(ggplot2)
library(olsrr)
library(tidyr)

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

par(mfrow=c(1,1))

# Test for heteroscedasticity

model<-lm(coninc~region, data=mydata)

ols_test_f(model, rhs = T)
ols_test_score(model, rhs = T)

# ANOVA on log transformed data

aov<-aov(coninc~region, mydata)

summary(aov)

# AOV shows differences between groups move on to pair-wise tests

# Bonferroni correction

k<-nlevels(mydata$region)

bc<-k*(k-1)/2

bcsig<-.05/bc

bcsig

# t tests

# NE V MA
t1<-t.test(NE$coninc, MA$coninc)
t1p<-t1$p.value
t1ci<-t1$conf.int

# NE V ENC
t2<-t.test(NE$coninc, ENC$coninc)
t2p<-t2$p.value
t2ci<-t2$conf.int

# NE V WNC
t3<-t.test(NE$coninc, WNC$coninc)
t3p<-t3$p.value
t3ci<-t3$conf.int

# NE V SA
t4<-t.test(NE$coninc, SA$coninc)
t4p<-t4$p.value
t4ci<-t4$conf.int

# NE V ESC
t5<-t.test(NE$coninc, ESC$coninc)
t5p<-t5$p.value
t5ci<-t5$conf.int

# NE V WSC
t6<-t.test(NE$coninc, WSC$coninc)
t6p<-t6$p.value
t6ci<-t6$conf.int

# NE V MT
t7<-t.test(NE$coninc, MT$coninc)
t7p<-t7$p.value
t7ci<-t7$conf.int

# NE V PC
t8<-t.test(NE$coninc, PC$coninc)
t8p<-t8$p.value
t8ci<-t8$conf.int

# MA V ENC
t9<-t.test(MA$coninc, ENC$coninc)
t9p<-t9$p.value
t9ci<-t9$conf.int

# MA V WNC
t10<-t.test(MA$coninc, WNC$coninc)
t10p<-t10$p.value
t10ci<-t10$conf.int

# MA V SA
t11<-t.test(MA$coninc, SA$coninc)
t11p<-t11$p.value
t11ci<-t11$conf.int

# MA V ESC
t12<-t.test(MA$coninc, ESC$coninc)
t12p<-t12$p.value
t12ci<-t12$conf.int

# MA V WSC
t13<-t.test(MA$coninc, WSC$coninc)
t13p<-t13$p.value
t13ci<-t13$conf.int

# MA V MT
t14<-t.test(MA$coninc, MT$coninc)
t14p<-t14$p.value
t14ci<-t14$conf.int

# MA V PC
t15<-t.test(MA$coninc, PC$coninc)
t15p<-t15$p.value
t15ci<-t15$conf.int

# ENC V WNC
t16<-t.test(ENC$coninc, WNC$coninc)
t16p<-t16$p.value
t16ci<-t16$conf.int

# ENC V SA
t17<-t.test(ENC$coninc, SA$coninc)
t17p<-t17$p.value
t17ci<-t17$conf.int

# ENC V ESC
t18<-t.test(ENC$coninc, ESC$coninc)
t18p<-t18$p.value
t18ci<-t18$conf.int

# ENC V WSC
t19<-t.test(ENC$coninc, WSC$coninc)
t19p<-t19$p.value
t19ci<-t19$conf.int

# ENC V MT
t20<-t.test(ENC$coninc, MT$coninc)
t20p<-t20$p.value
t20ci<-t20$conf.int

# ENC V PC
t21<-t.test(ENC$coninc, PC$coninc)
t21p<-t21$p.value
t21ci<-t21$conf.int

# WNC V SA
t22<-t.test(WNC$coninc, SA$coninc)
t22p<-t22$p.value
t22ci<-t22$conf.int

# WNC V ESC
t23<-t.test(WNC$coninc, ESC$coninc)
t23p<-t23$p.value
t23ci<-t23$conf.int

# WNC V WSC
t24<-t.test(WNC$coninc, WSC$coninc)
t24p<-t24$p.value
t24ci<-t24$conf.int

# WNC V MT
t25<-t.test(WNC$coninc, MT$coninc)
t25p<-t25$p.value
t25ci<-t25$conf.int

# WNC V PC
t26<-t.test(WNC$coninc, PC$coninc)
t26p<-t26$p.value
t26ci<-t26$conf.int

# SA V ESC
t27<-t.test(SA$coninc, ESC$coninc)
t27p<-t27$p.value
t27ci<-t27$conf.int

# SA V WSC
t28<-t.test(SA$coninc, WSC$coninc)
t28p<-t28$p.value
t28ci<-t28$conf.int

# SA V MT
t29<-t.test(SA$coninc, MT$coninc)
t29p<-t29$p.value
t29ci<-t29$conf.int

# SA V PC
t30<-t.test(SA$coninc, PC$coninc)
t30p<-t30$p.value
t30ci<-t30$conf.int

# ESC V WSC
t31<-t.test(ESC$coninc, WSC$coninc)
t31p<-t31$p.value
t31ci<-t31$conf.int

# ESC V MT
t32<-t.test(ESC$coninc, MT$coninc)
t32p<-t32$p.value
t32ci<-t32$conf.int

# ESC V PC
t33<-t.test(ESC$coninc, PC$coninc)
t33p<-t33$p.value
t33ci<-t33$conf.int

# WSC V MT
t34<-t.test(WSC$coninc, MT$coninc)
t34p<-t34$p.value
t34ci<-t34$conf.int

# WSC V PC
t35<-t.test(WSC$coninc, PC$coninc)
t35p<-t35$p.value
t35ci<-t35$conf.int

# MT V PC
t36<-t.test(MT$coninc, PC$coninc)
t36p<-t36$p.value
t36ci<-t36$conf.int

# Build a table of results

# P-values

pval<-c(t1p, t2p, t3p, t4p, t5p, t6p, t7p, t8p,
         t9p, t10p, t11p, t12p, t13p, t14p, t15p,
         t16p, t17p, t18p, t19p, t20p, t21p, t22p,
         t23p, t24p, t25p, t26p, t27p, t28p, t29p,
         t30p, t31p, t32p, t33p, t34p, t35p, t36p)

# Region Combination

combo<-c("NE V MA", "NE V ENC", "NE V WNC", "NE V SA",
          "NE V ESC", "NE V WSC", "NE V MT", 'NE V PC',
          "MA V ENC", "MA V WNC", "MA V SA", "MA V ESC",
          "MA V WSC", "MA V MT", "MA V PC", "ENC V WNC",
          "ENC V SA", "ENC V ESC", "ENC V WSC", "ENC V MT",
          "ENC V PC", "WNC V SA", "WNC V ESC", "WNC V WSC",
          "WNC V MT", "WNC V PC", "SA V ESC", "SA V WSC",
          "SA V MT", "SA V PC", "ESC V WSC", "ESC V MT",
          "ESC V PC", "WSC V MT", "WSC V PC", "MT V PC")

# Lower confidence interval

lowerci<-c(t1ci[1],t2ci[1],t3ci[1],t4ci[1],t5ci[1],t6ci[1],
           t7ci[1],t8ci[1],t9ci[1],t10ci[1],t11ci[1],t12ci[1],
           t13ci[1],t14ci[1],t15ci[1],t16ci[1],t17ci[1],t18ci[1],
           t19ci[1],t20ci[1],t21ci[1],t22ci[1],t23ci[1],t24ci[1],
           t25ci[1],t26ci[1],t27ci[1],t28ci[1],t29ci[1],t30ci[1],
           t31ci[1],t32ci[1],t33ci[1],t34ci[1],t35ci[1],t36ci[1])

# Upper confidence interval

upperci<-c(t1ci[2],t2ci[2],t3ci[2],t4ci[2],t5ci[2],t6ci[2],
           t7ci[2],t8ci[2],t9ci[2],t10ci[2],t11ci[2],t12ci[2],
           t13ci[2],t14ci[2],t15ci[2],t16ci[2],t17ci[2],t18ci[2],
           t19ci[2],t20ci[2],t21ci[2],t22ci[2],t23ci[2],t24ci[2],
           t25ci[2],t26ci[2],t27ci[2],t28ci[2],t29ci[2],t30ci[2],
           t31ci[2],t32ci[2],t33ci[2],t34ci[2],t35ci[2],t36ci[2])

# 1st region mean

mean1<-c(mean(NE$coninc), mean(NE$coninc), mean(NE$coninc), mean(NE$coninc),
         mean(NE$coninc), mean(NE$coninc), mean(NE$coninc), mean(NE$coninc),
         mean(MA$coninc), mean(MA$coninc), mean(MA$coninc), mean(MA$coninc),
         mean(MA$coninc), mean(MA$coninc), mean(MA$coninc), mean(ENC$coninc),
         mean(ENC$coninc), mean(ENC$coninc), mean(ENC$coninc), mean(ENC$coninc),
         mean(ENC$coninc), mean(WNC$coninc), mean(WNC$coninc), mean(WNC$coninc),
         mean(WNC$coninc), mean(WNC$coninc), mean(SA$coninc), mean(SA$coninc),
         mean(SA$coninc), mean(SA$coninc), mean(ESC$coninc), mean(ESC$coninc),
         mean(ESC$coninc), mean(WSC$coninc), mean(WSC$coninc), mean(MT$coninc))

# 2nd region mean

mean2<-c(mean(MA$coninc), mean(ENC$coninc), mean(WNC$coninc), mean(SA$coninc),
         mean(ESC$coninc), mean(WSC$coninc), mean(MT$coninc), mean(PC$coninc),
         mean(ENC$coninc), mean(WNC$coninc), mean(SA$coninc), mean(ESC$coninc),
         mean(WSC$coninc), mean(MT$coninc), mean(PC$coninc), mean(WNC$coninc),
         mean(SA$coninc), mean(ESC$coninc), mean(WSC$coninc), mean(MT$coninc),
         mean(PC$coninc), mean(SA$coninc), mean(ESC$coninc), mean(WSC$coninc),
         mean(MT$coninc), mean(PC$coninc), mean(ESC$coninc), mean(WSC$coninc),
         mean(MT$coninc), mean(PC$coninc), mean(WSC$coninc), mean(MT$coninc),
         mean(PC$coninc), mean(MT$coninc), mean(PC$coninc), mean(PC$coninc))

# Combine vectors into table

ttable<-as.data.frame(cbind(combo, mean1, mean2, pval, lowerci, upperci))

# Convert factors to continuous variables

ttable$mean1<-as.numeric(as.character(ttable$mean1))
ttable$mean2<-as.numeric(as.character(ttable$mean2))
ttable$pval<-as.numeric(as.character(ttable$pval))
ttable$lowerci<-as.numeric(as.character(ttable$lowerci))
ttable$upperci<-as.numeric(as.character(ttable$upperci))

# Add significance indicators for Bonferoni corrected pvalues and confidence intervals

ttable$CIsig<-ifelse(((ttable$lowerci<0 & ttable$upperci<0) | (ttable$lowerci>0 & ttable$upperci>0)), "Yes", "No")
ttable$Bsig<-ifelse(pval<bcsig, "Yes","No")


           