#To remove allstored data
rm(list=ls())

#LAB 4: Variable selection

#Task 1: Salary and GPA
#Import .csv data into R
setwd("C:\\Users\\Acer\\Downloads")
dt <- read.csv("VO2Data.csv")
str(dt)

dt$Gender <- as.factor(dt$Gender)

y <- dt$V02
x1 <- dt$Weight
x2 <- dt$Age
x3 <- ifelse(dt$Gender == "Male",1,0)
x4 <- dt$HeartRate
x5 <- dt$BodyTemp
x6 <- dt$Height

#1. find r for qn and qn   
cor(y,x1)
cor(y,x2)
cor(y,x4)

cor(x1,x2)
cor(x1,x4)
cor(x2,x4)

#2.find VIF
full <- lm(y~x1+x2+x3+x4)
library(car)
vif(full)

# vif of x1
m0 <- lm(x1~x2+x3+x4)
summary(m0)
# vif =1/(1-R2)
1/(1-0.1555) # vif x1 = 1.184

#vifof x2
m1<- lm(x2~x1+x3+x4)
summary(m1)
1/(1-0.05297)

#3. Forward seiection
null <- lm(y~x1)
full <- lm(y~x1+x2+x3+x4)
#Forward seiection
step(null, scope = list(lower=null, upper=full),
     direction = "forward")

#3. FW step by step
#step 1.
f1 <-lm(y~x1)
summary(f1)
anova(f1)

f2 <-lm(y~x2)
summary(f2)
anova(f2)

f3 <-lm(y~x3)
summary(f3)
anova(f3)

f4 <-lm(y~x4)
summary(f4)
anova(f4)

#step2:
altf5 <- lm(y~x3+x1)
summary(altf5)
anova(altf5)

altf6 <- lm(y~x3+x2)
summary(altf6)
anova(altf6)

altf7 <- lm(y~x3+x4)
summary(altf7)
anova(altf7)

#step3.
altf8 <- lm(y~x3+x1+x2)
summary(altf8)
anova(altf8)

altf9 <- lm(y~x3+x1+x4)
summary(altf9)
anova(altf9)

#backward elimination
fulll <- lm(y~x1+x2+x3+x4)
step(fulll,direction = "backward")

#BE step by step
#step1 backward
fulll <- lm(y~x1+x2+x3+x4)
#step2
#-x4 :y=x1+x2+x3
alt1 <- lm(y~x1+x2+x3)
anova(alt1,fulll)

#-x3 :y=x1+x2+x4
alt2 <- lm(y~x1+x2+x4)
anova(alt2,fulll)

#-x2:y=x1+x3+x4
alt3 <- lm(y~x1+x3+x4)
anova(alt3,fulll)

#-x1:y=x2+x3+x4
alt4 <- lm(y~x2+x3+x4)
anova(alt4,fulll)

#step3.
fullll <- lm(y~x1+x2+x3)
#-x3:y=x1+x2
alt5 <- lm(y~x1+x2)
anova(alt5,fullll)
#-x2:y=x1+x3
alt6 <- lm(y~x1+x3)
anova(alt6,fullll)
#-x1:y=x2+x3
alt7 <- lm(y~x2+x3)
anova(alt7,fullll)

#step4
fulllll <- lm(y~x1+x3)
#-x3:y=x1
alt8 <- lm(y~x1)
anova(alt8,fulllll)

#-x1:y=x3
alt9 <- lm(y~x3)
summary(fulllll)

