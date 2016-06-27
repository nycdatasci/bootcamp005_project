setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 5 test")

library(hash)
library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)
library(doParallel)
registerDoParallel(2)
library(Matrix)

#read in train data with features as data.table

train <- fread("train_with_mde_feature.csv")

#create keys for the week, Client, and Product

setkeyv(train, c("Semana", 'Cliente_ID','Producto_ID'))

#add the weekly purchase frequency for client product pairs

train[ , PurchaseCount := .N, by = list(Semana, Cliente_ID, Producto_ID)]

#remove pred feature as it causes leakage

train <- select(train,-11)

#xgboost model 

#split the train set into train and test

test <- train[Semana == 9, ]
train <- train[Semana < 9, ]

#impute all the NA's in the lag features as 0

test[is.na(test)] <- 0
train[is.na(train)] <- 0



train.y <- train$Demanda_uni_equil
test.y <- test$Demanda_uni_equil

test$Demanda_uni_equil <- NULL

#check the amount of memroy being used

memory.size()

#increase the memory limit do to size of the data 

memory.limit(size = 20000)

#make the train model matrix

train.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = train)

dtrain <- xgb.DMatrix(data = train.model, label = train.y)
watchlist <- list(train=dtrain)
rm(train.model,train)
gc()

depths = c(17, 20, 22, 24, 25, 27, 29, 30)
results = as.numeric(1:length(depths))

for (depth_cv in 1:length(depths)) {
  set.seed(1234)
  param <- list(  objective           = "reg:linear",
                  booster             = "gbtree",
                  eval_metric         = "rmse",
                  eta                 = 0.2,
                  max_depth           = depths[depth_cv]
  )
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 10,
                      verbose             = 1,
                      watchlist           = watchlist,
                      maximize            = FALSE
  )
  
  test$Demanda_uni_equil <- -1
  test.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = test)
  
  gc()
  
  preds <- predict(clf, test.model)
  test.y <- as.numeric(test.y)
  
  gc()
  
  preds[preds < 0] = 0.1
  result = rmsle(test.y, preds)
  
  results[depth_cv] = result
  to_csv = data.frame(nrounds = depths, results = results)
  write.csv(to_csv, "results_with_lag_feature_weightpieces_frequency.csv", row.names = F)
}
