library(aod)
library (readxl)
library(ggplot2)

setwd("/Users/yaz/Desktop/M1")
data<- read_excel("M1_data.xlsx")
data$subject <- as.integer(data$subject)
data$group <- factor(data$group)
data$block <- factor(data$block)
data$success <- factor(data$success)
data$RR <-as.integer(data$RR)

# overall model with all terms
logisticR <- glm(success ~ subject+group*block, data = data, family = "binomial")
summary(logisticR)
confint(logisticR) #CIs of the log odds
exp(cbind(OR = coef(logisticR), confint(logisticR))) ## odds ratios and 95% CI


# only looking at block 1 with PD group as reference
data1 <- split(data,data$block==2)$'FALSE' 
logisticR1 <- glm(success ~ subject+group, data = data1, family = "binomial")
summary(logisticR1)
confint(logisticR1) #CIs of the log odds
exp(cbind(OR = coef(logisticR1), confint(logisticR1))) ## odds ratios and 95% CI


# only looking at block 2 with PD group as reference
data2 <- split(data,data$block==2)$'TRUE' 
logisticR2 <- glm(success ~ subject+group, data = data2, family = "binomial")
summary(logisticR2)
confint(logisticR2) #CIs of the log odds
exp(cbind(OR = coef(logisticR2), confint(logisticR2))) ## odds ratios and 95% CI

# only looking at block 2 with N group as reference
data3 <- data2
data3$group <- factor(data3$group,levels=c(4,1,2,3))
logisticR3 <- glm(success ~ subject+group, data = data3, family = "binomial")
summary(logisticR3)
confint(logisticR3) #CIs of the log odds
exp(cbind(OR = coef(logisticR3), confint(logisticR3))) ## odds ratios and 95% CI


RRglm <- glm(RR ~ subject+group*block, data = data)
summary(RRglm)
confint.lm(RRglm)

data1RR <- split(data,data$block==1)$'TRUE' 
block1RRglm <- glm(RR ~ subject+group, data = data1RR)
summary(block1RRglm)
confint(block1RRglm) 

data2RR <- split(data,data$block==2)$'TRUE' 
block2RRglm <- glm(RR ~ subject+group, data = data2RR)
summary(block2RRglm)
confint(block2RRglm) 
