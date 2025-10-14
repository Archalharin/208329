rm(list=ls())

setwd("C:\\Users\\Acer\\Downloads")
dt <-read.csv("ScoreAndEQ (1).csv",header=TRUE)
str(dt)

y =dt$Y
x1 = ifelse(dt$Method == "A",1,0)
x2 = ifelse(dt$Method == "B",1,0)
x3 = ifelse(dt$Method == "C",1,0)
x4 = dt$Pretest
x5 = dt$EQ

#Descripive statistics
plot(y,x4)
boxplot(y~x5)
plot(x4,x5,xlab="x4",ylab="x5", col="red")
hist(y, main="Hisrogarm of Y",xlab="Y", col="pink")
#Correlation analysis
cor(x4,y)
cor(x5,y)
cor(x4,x5)
cor.test(x4,y,alternative =  "two.sided")
full <- lm(y~x1+x2+x3+x4+x5)
anova(full)
library(car)
vif(full)

#simple regression
fit <- lm(y~x4)
summary(fit)
anova(fit)
#Multiple regression
#forward seiection
null <-lm(y~1)
step(null, scope=list(lower=null, upper=full), direction = "forward")
#backward seiection
step(full, direction = "backward")
fit2 <- lm(y~x2+x3+x4+x5)
anova(fit2)

par(mfrow=c(2,2))
plot(fit2)