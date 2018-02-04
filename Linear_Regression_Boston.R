##############################################
#################Linear Regression############
##############################################

library(MASS)
library(ISLR)
library(dplyr)
library(tidyr)
library(car)

tbl_df(Boston)
glimpse(Boston)
fix(Boston)
names(Boston)

##Simple linear model
lm.fit <- lm(Boston$medv~Boston$lstat)
summary(lm.fit)
names(lm.fit)

plot(Boston$lstat,Boston$medv)
abline(lm.fit, col = "red", pch = 2)

##Partition
par(mfrow = c(2,2))
plot(lm.fit)

#Residual plot
plot(predict(lm.fit),residuals(lm.fit))
#Studentized residuals 
plot(predict(lm.fit),rstudent(lm.fit))
#Leverage statictic function
plot(hatvalues(lm.fit))


##############################################
##########Multiple Linear Regression##########
##############################################

lm.fit <- lm(medv~., data=Boston)
summary(lm.fit)

?summary.lm
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

##Variance inflation factor
vif(lm.fit)

lm.fit <- lm(medv~.-age, data=Boston)
summary(lm.fit)

##Interaction terms
#lstat*age includes lstat,age and lstat*age

lm.fit1 <- update(lm.fit, medv~lstat*age, data=Boston)
summary(lm.fit1)


##Non-linear transformations

lm.fit2 <- lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.fit2)

lm.fit <- lm(medv~lstat, data=Boston)
anova(lm.fit,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit6 <- lm(medv~log(rm), data = Boston)
lm.fit5 <- lm(medv~rm, data=Boston)

par(mfrow=c(2,2))
plot(lm.fit5)
plot(lm.fit6)


