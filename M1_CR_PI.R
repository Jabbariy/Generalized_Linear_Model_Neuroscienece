library(aod)
library (readxl)
library(ggplot2)
library(lme4)
library(nlme)
library(emmeans)
library(lsmeans)
library(multcomp)

#setwd()
#data<- read_excel("M1_CR_PI.xlsx")
data$subject <- factor(data$subject)
data$group <- factor(data$group, levels=c("PD", "P", "D", "N"))
data$block <- factor(data$block, levels=c("1st Block", "2nd Block"))
data$success <- factor(data$success)
data$trial <- as.integer(data$trial)
data$pair <- with(data,interaction(group,block,sep=""))
data$computerhr<-as.numeric(data$computerhr)



##########Success############
# 1st block #training block 
data1 <- split(data,data$block=='2nd Block')$'FALSE' 
logisticR1 <- glmer(success ~ group + computerhr + trial + (1 | trial) , data = data1, family = "binomial",  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(logisticR1)

confint(logisticR1) #CIs of the log odds
exp(cbind(OR = coef(logisticR1), confint(logisticR1))) ## odds ratios and 95% CI
OR=coef(logisticR1)
OR
exp(2.638301)
exp( 0.1064695) 
exp(-1.360722)
exp(-0.8417015)
exp(-0.06756583)
exp(-0.2380936)

pwc<-lsmeans :: lsmeans(logisticR1, pairwise~group,adjust="tukey")
pwc
confint(lsmeans :: lsmeans(logisticR1, pairwise~group,adjust="tukey"))

ORpwc=coef(pwc)
ORpwc

# 2nd block #transfer block
data2 <- split(data,data$block=='2nd Block')$'TRUE' 
logisticR2 <- glmer(success ~ group + computerhr + trial + (1|trial), data = data2, family = "binomial",  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(logisticR2)

OR2=coef(logisticR2)
OR2
exp(3.474085)
exp(0.1916401)
exp(0.9130864)
exp(0.5607509)


confint(logisticR2) #CIs of the log odds
exp(cbind(OR = coef(logisticR2), confint(logisticR2))) ## odds ratios and 95% CI



lsmeans :: lsmeans(logisticR2, pairwise~group,adjust="tukey")
confint(lsmeans :: lsmeans(logisticR2, pairwise~group,adjust="tukey"))

ORpwc=coef(pwc)
ORpwc


########Route Retracing##########
# 1st block #training block
block1RR <- nlme::lme(RR ~ group+ trial , random = ~1| trial  , data = data1)
summary(block1RR)
intervals(block1RR, which = "fixed")

lsmeans :: lsmeans(block1RR, pairwise~group,adjust="tukey")
confint(lsmeans :: lsmeans(block1RR, pairwise~group,adjust="tukey"))

#2nd block #transfer block
block2RR <- nlme::lme(RR ~ group+trial, random = ~1| trial,  data = data2)
summary(block2RR)
intervals(block2RR, which = "fixed")

lsmeans :: lsmeans(block2RR, pairwise~group,adjust="tukey")
confint(lsmeans :: lsmeans(block2RR, pairwise~group,adjust="tukey"))




########Route Retracing of only successful trials##########
data1RR <- split(data, data$success == 1)$'TRUE' 
data1RR1 <- split(data1RR,data1RR$block=='1st Block')$'TRUE' 
# 1st group
block1RRsuccess <- nlme::lme(RR ~ group+ trial , random = ~1| trial  , data = data1RR1)
summary(block1RRsuccess)
intervals(block1RRsuccess, which = "fixed")

lsmeans :: lsmeans(block1RRsuccess, pairwise~group,adjust="tukey")
confint(lsmeans :: lsmeans(block1RR, pairwise~group,adjust="tukey"))

#2nd group
data2RR <- split(data, data$success == 1)$'TRUE' 
data2RR2 <- split(data2RR,data2RR$block=='2nd Block')$'TRUE' 
block2RRsuccess <- nlme::lme(RR ~ group+trial, random = ~1| trial,  data = data2RR2)
summary(block2RRsuccess)
intervals(block2RR, which = "fixed")

lsmeans :: lsmeans(block2RR, pairwise~group,adjust="tukey")
confint(lsmeans :: lsmeans(block2RR, pairwise~group,adjust="tukey"))










