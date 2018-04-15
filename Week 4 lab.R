# load libraries

library(statsr)
library(dplyr)
library(ggplot2)

data(atheism)

us12<-atheism %>%
  filter(nationality=="United States" & year==2012)
  
x1<-sum(us12$response=="atheist")
n<-nrow(us12)
p<-x1/n
j<-(p*(1-p)/n)
SE<-sqrt(j)
SE
ME=1.96*SE
ME

inference(y = response, data = us12, statistic = "proportion", type = "ci",
          method = "theoretical", success = "atheist")



spain<-atheism %>%
  filter(nationality=="Spain")

inference(y=response, x=year, data=spain, statistic = "proportion", type="ht",
          method = "theoretical", success="atheist", alternative = "twosided")

USA<-atheism %>%
  filter(nationality=="United States")

inference(y=response, x=year, data=USA, statistic = "proportion", type="ht",
          method = "theoretical", success="atheist", alternative = "twosided")
