##Predicting Boston Housing prices##


rm(list=ls())

Loadlibraries <- function()
{
  library(MASS)
  library(tidyr)
  library(dplyr)
  library(readxl)
  print("Libraries are loaded")
}


Loadlibraries()
getwd()
setwd("E:\\Predective Modeling\\Class 3")


##Loading the data

Boston1 <- read_excel("data/Boston Housing.xlsx")
glimpse(Boston1)
tbl_df(Boston1)
names(Boston1)

##Train and test datasets
train <- Boston1[Boston1$Validation==0,]
nrow(train)
validation <- Boston1[Boston1$Validation==1,]
nrow(validation)


##Model build using 3 variables
lm.b <- lm(MEDV~CRIM+CHAS+RM, data = train)
summary(lm.b)
df.residual(lm.b)



##Stepwise to reduce variables if possible
lm.b.step <- stepAIC(lm.b, direction = "both")
summary(lm.b.step)



##Collinearity check
library(car)
vif(lm.b)
pairs(train[,c("CRIM","CHAS","RM")])

##Predict values in training dataset
train$predict <- predict(lm.b)
train$error <- residuals(lm.b)
View(train)
plot(train$predict,train$error)

summary(lm.b)

##Predict values in validation
validation$predict <- predict(lm.b, newdata=validation)
validation$error <- validation$predict - validation$MEDV
plot(validation$predict,validation$error)

par(mfrow=c(2,2))
hist(train$error)
hist(validation$error)

##Correlation

a <- cor(train$predict,train$MEDV)
b <- cor(validation$predict,validation$MEDV)
b
a


##Min Max accuracy and MAPE

val <- validation[,c("predict","MEDV")]
val

min_max_acc <- mean(apply(val,1,min)/apply(val,1,max))
min_max_acc
MAPE <- mean(abs(val$predict-val$MEDV)/val$MEDV)
MAPE  #mean absolute pect deviation


##Mean square error
mse.train <- summary(lm.b)$sigma
mse.train
mse.test <- sqrt(sum((val$predict-val$MEDV)^2)/ (nrow(validation)-2))
mse.test
