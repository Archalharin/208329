#Lab2 Simple liner regression
#660510632

#Task 1:
#change \ to \\
setwd("C:\\Users\\Acer\\Downloads")
#read data flie
dt1 <- read.csv("TheRocketData.csv", header = TRUE)

x <- dt1$Age.of.Propellant
y <- dt1$Sher.Strength

#1
plot(x,y)

#2.
cor(x,y) #r
cor.test(x,y,alternative =  "two.sided") # 95% CI
cor.test(x,y,alternative =  "two.sided",
         conf.level = 0.9)
#3.
# ~ <- Alt126
fit <- lm(y~x)
summary(fit)
anova(fit)

confint.lm(fit,level = 0.9)
#4.
#90% CI of data
confint.lm(fit,level = 0.9)
#95% CI of data
confint.lm(fit,level = 0.95)
#99% CI of data
confint.lm(fit,level = 0.99)

#7-8.
xnew <- data.frame(x=10)
yhat <- predict.lm(fit,newdata = xnew)
yhat

#7. 95 CI ของ E(Y|x=10)
predict.lm(fit, newdata = xnew, interval = "confidence")

#95 CI ของ y|x=10
predict.lm(fit, newdata = xnew, interval = "prediction")

#9.

