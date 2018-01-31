###########################################
##### Linear regression on Real estate data
###########################################

# 1. Importing data
library(car)
library(MASS)
library(dplyr)
library(tidyr)
setwd("E:\\Study\\R Projects\\day3")
house_prices <- read.csv(file.choose(), sep = "|")
names(house_prices)
str(house_prices)
#tbl_df(house_prices)
#glimpse(house_prices)
attach(house_prices)


# 2. Simple linear regression model Price v/s Sqaure feet
lm.1 <- lm(Price ~ SqFt, data=house_prices)
summary(lm.1)
co <- cor(house_prices[c(-7,-8)])
#pairs(house_prices[c(-7,-8)])

plot(SqFt, Price, main="Scatter plot", xlab="Square feet", ylab="Price")
abline(lm.1,col="red",lwd=3)

# 3 Prepare data and split

# 3.1 Create dummy variables

house_prices$brick_d<-ifelse(house_prices$Brick=="Yes",1,0)
house_prices$east<-ifelse(house_prices$Neighborhood=="East",1,0)
house_prices$north<-ifelse(house_prices$Neighborhood=="North",1,0)

# 3.2 Split your dataset

set.seed(110)
sub <- sample(nrow(house_prices), floor(nrow(house_prices) * 0.6))
training_data <- house_prices[sub,]
validation_data <- house_prices[-sub,]
sub
set.seed(100)
# 4 Build multiple regression model

# 4.1 Build model with all variables
lm.fit1 <- lm(Price ~ SqFt+Bathrooms+Bedrooms+Offers+
             north+east+brick_d, data=training_data)

summary(lm.fit1)

# 4.2 Using stepwise, we reduce variables if possible

lm.fit1.step <- stepAIC(lm.fit1)
summary(lm.fit1.step)


# 5 Check for multicollinearity

vif(lm.fit1)

# 6 Predict values on training and validation data sets

# 6.1 Predict values on training set
training_data$predict.price <- predict(lm.fit1)
training_data$error <- residuals(lm.fit1)
attach(training_data)
plot(predict.price, error)

# 6.2 Predict values on validation set
validation_data$predict.price <- predict(lm.fit1,newdata=validation_data)
validation_data$error <- validation_data$predict.price - validation_data$Price

# 6.3 Check residual plots
hist(training_data$error)
hist(validation_data$error)

# 6.4 Correlation
a<-cor(training_data$Price,training_data$predict.price)
b<-cor(validation_data$Price,validation_data$predict.price)
a*a
b*b

