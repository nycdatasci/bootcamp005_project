setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 5 test")


library(hash)
library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)
library(doParallel)
registerDoParallel(2)
library(slackr)
library(Matrix)


slackr_setup(channel = "@hcozart", username = "i_bimbobot", icon_emoji = "",
             incoming_webhook_url = "datasqawd.slack.com", api_token = "xoxb-51493476675-ZkuheKfwDSdeEtTok0NRPcG6",
             echo = FALSE)

week6 <- fread("week6lag8weeksum.csv")
week7 <- fread("week7lag8weeksum.csv")
week8 <- fread("week8lag8weeksum.csv")
week9 <- fread("week9lag8weeksum.csv")


#rbind the last 4 weeks to make the train set

train <- rbind(week6,week7,week8,week9)

rm(week6,week7,week8,week9)

#Add my total prior demand feature.

str(train)

# mean all the new lag features

train[, PriorDemand := (Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Lag6 +Lag7 +Lag8)/8]

str(train)

#remove extra lag weeks

train <- select(train,c(-11,-12,-13,-14,-15))

str(train)

#need to cbind old lag features
setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 5 test/lags")

week6 <- fread("week6lag8week.csv")
week7 <- fread("week7lag8week.csv")
week8 <- fread("week8lag8week.csv")
week9 <- fread("week9lag8week.csv")

#rbind the last 4 weeks to make the old train set

train2 <- rbind(week6,week7,week8,week9)

rm(week6,week7,week8,week9)

#make sure the columns are a match

str(train2)
str(train)

#copy the columns

NLag1 = train2[,Lag1]

NLag2 = train2[,Lag2]
  
NLag3 = train2[,Lag3]

train[,Lag1:= NLag1]

train[,Lag2:= NLag2]

train[,Lag3:= NLag3]

str(train)

rm(NLag1, NLag2, NLag3, train2)

#xgboost model

test <- train[Semana == 9, ]
train <- train[Semana < 9, ]

train.y <- train$Demanda_uni_equil

test.y <- test$Demanda_uni_equil
test$Demanda_uni_equil <- NULL

memory.size()

gc()

memory.limit(size = 20000)

train.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = train)

gc()

dtrain <- xgb.DMatrix(data = train.model, label = train.y)
watchlist <- list(train=dtrain)
rm(train.model,train)

gc()


depths = c(21, 22, 23, 24, 25)
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
  write.csv(to_csv, "results_with_lag_feature_priordemandtotmean.csv", row.names = F)
}
