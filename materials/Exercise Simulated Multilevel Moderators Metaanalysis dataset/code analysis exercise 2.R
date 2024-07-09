
##############################

rm(list=ls())
library(metafor)
library(clubSandwich)
library(emmeans)

##############################

df = read.csv("datasetMLMA.csv", header=T)

dx = escalc(measure="SMD", m2i=M_Clinical, m1i=M_Controls, 
            sd2i=SD_Clinical, sd1i=SD_Controls, 
            n2i=N_Clinical, n1i=N_Controls, data=df)

dx$directionSign = ifelse(dx$direction=="positive", +1, -1)
dx$cohensd = dx$directionSign * dx$yi

##############################

V = impute_covariance_matrix(dx$vi, cluster=dx$idStudy, r=0.5)

mod = rma.mv(cohensd, V, data=dx, random =~ 1|idStudy/idEff) 
mod

mod1 = rma.mv(cohensd, V, data=dx, mods=~1+meanAge+taskType, random =~ 1|idStudy/idEff) 
mod1

regplot(mod1,mod="taskType")

emp = emmprep(mod1)
emmeans(emp, specs="taskType")

mod2 = rma.mv(cohensd, V, data=dx, mods=~meanAge*taskModality+taskType*taskModality, random =~ 1|idStudy/idEff) 
mod2
emp = emmprep(mod2,at=list(meanAge=seq(7,17,1)))
ggd = data.frame(emmeans(emp,specs="meanAge",by="taskModality"))
ggd

library(ggplot2)
ggplot(ggd,aes(x=meanAge,y=emmean,group=taskModality,linetype=taskModality,color=taskModality,fill=taskModality))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=asymp.LCL,ymax=asymp.UCL),alpha=.25,color=NA)+
  ylab("Cohen's d")+
  theme(text=element_text(size=20))


##############################


