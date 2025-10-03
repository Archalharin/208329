#To remove allstored data
rm(list=ls())

#Import .csv data into R
setwd("C:\\Users\\Acer\\Downloads")
dt1 <- read.csv("TheRocketData (1).csv")
str(dt1)
dt2 <- read.csv("TheAthleteData.csv")
str(dt2)
x <- dt1$Sher.Strength
y <- dt1$Age.of.Propellant

plot(x,y)
#1. fit wish simple liner regression
mlin <- lm(y~x)
summary(mlin)
anova(mlin)
# check model assumptions
resmlin <- resid(mlin)

par(mfrow=c(2,2)) #set plots layout
plot(mlin)

#test model assumptions
#test ความแปรปรวนควที่หรือไม่
library("lmtest")
bptest(mlin)

var.test(resmlin[1:10],resmlin[11:20])

#normality test
shapiro.test(resmlin)

#Correlation test
dwtest(mlin)

#fit wish sqrt of y
msq <- lm(sqrt(y)~x)
summary(msq)
anova(msq)

resmsq <- resid(msq)
par(mfrow=c(2,2)) #set plots layout
plot(msq)

bptest(msq)
var.test(resmsq[1:10],resmsq[11:20])

#normality test
shapiro.test(resmsq)

#Correlation test
dwtest(msq)

#extra: delete obs no.5 and 6
xx <- x[-c(5,6)]
yy <- y[-c(5,6)]

f1 <- lm(yy~xx)
bptest(f1)

x1 <- dt2$Fat.Intake
y1 <- dt2$Cholesterol

mlin <- lm(y1~x1)
summary(mlin)
anova(mlin)

# check model assumptions
resmlin <- resid(mlin)

par(mfrow=c(2,2)) #set plots layout
plot(mlin)

#normality test
shapiro.test(resmlin)

#Correlation test
dwtest(mlin)

fitqua <- lm(y1~x1+I(x1^2))
summary(fitqua)
anova(fitqua)

resfitqua <- resid(fitqua)
par(mfrow=c(2,2)) #set plots layout
plot(fitqua)

bptest(fitqua)
var.test(resmsq[1:10],resmsq[11:20])

#normality test
shapiro.test(resfitqua)

#Correlation test
dwtest(fitqua)