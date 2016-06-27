setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 5 test")

library(hash)
library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)
library(twitteR)
library(doParallel)
registerDoParallel(2)
library(slackr)
library(Matrix)
slackr_setup(channel = "@hcozart", username = "i_bimbobot", icon_emoji = "",
             incoming_webhook_url = "datasqawd.slack.com", api_token = "xoxb-51493476675-ZkuheKfwDSdeEtTok0NRPcG6",
             echo = FALSE)

train <- fread("train_lag_wtpcs_freq_count.csv")

#create a new count column for cummulative sum

train[, count2 := 1]

#order by week

train = train[order(Semana)]

#create a running sum for client product pairs

train[, Cum.Sum := cumsum(count2), by=list(Cliente_ID,Producto_ID)]

#remove count2

train <- select(train,c(-17))


#xgboost model

test <- train[Semana == 9, ]
train <- train[Semana < 9, ]

test[is.na(test)] <- 0
train[is.na(train)] <- 0


train.y <- train$Demanda_uni_equil

test.y <- test$Demanda_uni_equil
test$Demanda_uni_equil <- NULL

memory.size()

train.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = train)

gc()

dtrain <- xgb.DMatrix(data = train.model, label = train.y)
watchlist <- list(train=dtrain)
rm(train.model,train)

gc()


depths = c(21, 26, 27, 28, 29, 30)
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
  message = paste0("Hey Hayes ;), for depth ", depths[depth_cv], ", your rmsle was: ", result)
  slackr(message)
  to_csv = data.frame(nrounds = depths, results = results)
  write.csv(to_csv, "results_with_lag_feature_weightpieces_cumsumnew.csv", row.names = F)
}
