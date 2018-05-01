

load_lb <- function()
{
  library(readxl)
  library(tidyr)
  library(dplyr)
  library(caret)
  library(rpart)
  library(tree)
  library(MASS)
  library(mice)
  require(xgboost)
  require(data.table)
  require(Matrix)
}

load_lb()

library(rpart.plot)

library(AmesHousing)
data <- AmesHousing::make_ames()

nrow(data)
ncol(data)


set.seed(123)

init <- createDataPartition(data$Sale_Price, p = 0.7, list = FALSE)
train <- data[init,]
test <- data[-init,]

m1 <- rpart(Sale_Price~.,data=train,method = "anova")
rpart.plot(m1)
plotcp(m1)
m1$cptable
  
grid <- expand.grid(
  minsplit = seq(5,20,1),
  maxdepth = seq(8,15,1)
)
head(grid)
nrow(grid)

# Model creation and saving

models <- list()

for (i in 1:nrow(grid))
{
  minsp <- grid$minsplit[i]
  maxdp <- grid$maxdepth[i]
  
  # train model and store in list
  models[[i]] <- rpart(
    Sale_Price~.,
    data = train,
    method = "anova",
    control = list(minsplit=minsp,maxdepth=maxdp)
  )
}

# Get optimal cp

get_cp <- function(x)
{
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min,"cp"]
}

# Get optimal error
get_min_error <- function(x)
{
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min,"xerror"]
}


grid %>%
  mutate(
      cp = purrr::map_dbl(models,get_cp),
      error = purrr::map_dbl(models,get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5,wt = error)

## rpart
opt_tree <- rpart(
  Sale_Price~.,
  data = train,
  method = "anova",
  control = list(minsplit=11, maxdepth=8, cp=0.01)
)

pred <- predict(opt_tree,newdata = test)
RMSE(pred,test$Sale_Price)


Rsq <- function(x,y)
{
  RSS <- sum((x-y)^2)
  TSS <- sum((x-mean(x))^2)
  return((1-RSS/TSS))
}

Rsq(test$Sale_Price,pred)

### Bagging

# Specify 10-fold cross validation
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv <- train(
  Sale_Price ~ .,
  data = train,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

pred1 <- predict(bagged_cv,test)
Rsq(test$Sale_Price,pred1)
varImp(bagged_cv)

## Basic random forest

ggplot(data,aes(x=sqrt(Sale_Price))) +
  geom_density()

ncol(data)

#Creating grid - random forest
modelLookup(model='ranger')
grid <- expand.grid(mtry = c(13,14,15,16,17,18,19,20),
                    splitrule = "variance",
                    min.node.size = c(3,5,10))
rf_model <- train(Sale_Price~.,
                  data = train,
                  method = "ranger",
                  trControl = ctrl,
                  tuneGrid = grid)
print(rf_model)
rf_model
pred2 <- predict(rf_model,newdata = test)
Rsq(test$Sale_Price,pred2)

ggplot(test,aes(x=pred2,y=test$Sale_Price)) +
  geom_point()
## we can see outliers at the end


# Linera regression

set.seed(123)
modelLookup(model="lm")
lm_model <- train(Sale_Price~.,
                  data = train,
                  method = "lm",
                  trControl = ctrl)
pred3 <- predict(lm_model,newdata = test)
Rsq(test$Sale_Price,pred3)

model_list <- list(lm=lm_model, rf=rf_model)
resample = resamples(model_list)
summary(resample)

bwplot(resample, metric="RMSE")


# GBM
modelLookup(model="gbm")
gbmTuningGrid = expand.grid(interaction.depth = 5, 
                            n.trees = 200, 
                            shrinkage = 0.1,
                            n.minobsinnode = 20)
model_gbm2 = train(Sale_Price ~ ., 
                   data = train,
                   method = "gbm",
                   trControl = ctrl,
                   tuneGrid = gbmTuningGrid)

model_gbm2
pred5 <- predict(model_gbm2, newdata = test)
Rsq(test$Sale_Price, pred5)


# Xgboost
modelLookup(model="xgbLinear")
xgbTuningGrid = expand.grid(nrounds = c(50, 100), 
                            lambda = seq(0.1, 0.5, 0.1), 
                            alpha = seq(0.1, 0.5, 0.1),
                            eta = c(0.3, 0.4))
model_xgb4 = train(Sale_Price ~ ., 
                   data = train,
                   method = "xgbLinear",
                   trControl = ctrl,
                   tuneGrid = xgbTuningGrid)
model_xgb4
pred6 <- predict(model_xgb4, newdata = test)
Rsq(test$Sale_Price,pred6)

colnames(data)

## Check for any outliers

ggplot(data, aes(x=Garage_Area,y=Sale_Price)) +
  geom_point()
# Removing the 'garage area' more than 1250

data <- data[data$Garage_Area<1250,]


col_class <- c()
for (i in 1:length(colnames(data)))
{
  col_class[i] <- class(data[[i]])
  i=i+1
}

num_col <- colnames(data[col_class=="numeric"])
fct_col <- colnames(data[!col_class=="numeric"])

## ElasticNet 

glmnetTuningGrid = expand.grid(alpha = seq(0, 1, 0.2),
                               lambda = seq(0, 1, 0.2))
model_glmnet1 = train(Sale_Price ~ ., 
                      data = train,
                      method = "glmnet",
                      trControl = ctrl,
                      tuneGrid = glmnetTuningGrid)
model_glmnet1
pred7 <- predict(model_glmnet1, newdata = test)
Rsq(test$Sale_Price, pred7)



## Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcome <- "Sale_Price"
predictiors <- names(data)[!names(data) %in% outcome]

# error in y part of rfe - changed to [[]]
pred_profile <- rfe(train[,predictiors],train[[outcome]],
                    rfeControl = control)
pred_profile

## gave top 5 variables

