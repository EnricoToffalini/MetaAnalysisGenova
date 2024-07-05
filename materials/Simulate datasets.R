
#######################################

rm(list=ls())
library(MASS)
library(metafor)
library(psych)
source("R code utils/utils.R")

#######################################

# simulate data with z scores

set.seed(0)

ns = c(70, 357, 119, 450, 46, 680)
rs = c(.10, .36, .40, .31, .60, .22)
ages = c(45, 30, 30, 40, 24, 48)
for(i in 1:length(ns)){
  d = data.frame(id=1:ns[i], age=round(runif(ns[i],ages[i]-6,ages[i]+6)),
                 mvrnorm(n=ns[i], mu=c(0,0),Sigma=matrix(c(1,rs[i],rs[i],1),2,2)))
  colnames(d)[3:4] = c("SensSeek","RiskDriv")
  d[,"SensSeek"] = round(d[,"SensSeek"],4)
  d[,"RiskDriv"] = round(d[,"RiskDriv"],4)
  assign(paste0("d",i),d)
  print(cor(d[,3],d[,4]))
}
dTot = rbind(d1,d2,d3,d4,d5,d6)
cor.test(dTot[,3],dTot[,4])

#######################################

# transform some z scores into sum scores

d2$SensSeek = z2SumScore(d2$SensSeek,nItems=100,0,1)
d4$SensSeek = z2SumScore(d4$SensSeek,nItems=10,0,2)
d5$SensSeek = z2SumScore(d5$SensSeek,nItems=10,0,2)
d6$SensSeek = z2SumScore(d6$SensSeek,nItems=10,1,6)

d2$RiskDriv = z2SumScore(d2$RiskDriv,nItems=20,0,1)
d3$RiskDriv = z2SumScore(d3$RiskDriv,nItems=20,0,1)
d4$RiskDriv = z2SumScore(d4$RiskDriv,nItems=10,1,5)
d5$RiskDriv = z2SumScore(d5$RiskDriv,nItems=5,1,5)
d6$RiskDriv = z2SumScore(d6$RiskDriv,nItems=5,1,5)

#######################################

# export
save(d1,d2,d3,d4,d5,d6,file="Exercise Simulated dataset/datasets.RData")
for(i in 1:length(ns)){
  write.table(get(paste0("d",i)),file=paste0("Exercise Simulated dataset/Study",i,".csv"),row.names=F,sep=",")
}

#######################################

# try meta-analyses

dx = data.frame(eff=rep(NA,length(ns)),vi=NA,age=NA)
for(i in 1:nrow(dx)){
  dx$eff[i] = fisherz(cor(get(paste0("d",i))[,3], get(paste0("d",i))[,4]))
  dx$vi[i] = 1/(nrow(get(paste0("d",i))-3))
  dx$age[i] = mean(get(paste0("d",i))[,"age"])
}
fit1 = rma(eff,vi,data=dx,method="EE"); summary(fit1)
fit2 = rma(eff,vi,data=dx); summary(fit2)
fit3 = rma(eff,vi,mods=~age,data=dx); summary(fit3)
anova(fit2,fit3,refit=T)

regplot(fit3)

#######################################

