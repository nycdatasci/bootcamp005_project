library(hash)
library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)
library(Matrix)
#library(twitteR)
library(doMC)
registerDoMC(2)
library(slackr)
# setup_twitter_oauth('KUPiq7JfGdmRCipxX4R34Wnin', '3AyMcGxAJWODP9LKRVgSxWTpoSkILnwVawZqqhZ7H7XWgZQVoH',
#                     '137572534-ApnOtpw6SjVe7LpBhstUgKxM4FoDQDChIcyPsNiN',
#                     'fHwEf55VJMtWR22nN3zu7SLV4AqDA8DCmCbAMhfhH1cI2')
slackr_setup(channel = "@kes367", username = "i_bimbobot", icon_emoji = "",
             incoming_webhook_url = "datasqawd.slack.com", api_token = "xoxb-51493476675-ZkuheKfwDSdeEtTok0NRPcG6",
             echo = FALSE)
setwd("~/GitHub-kszela24/Faceback/Szela/GrupoBimbo")
train <- fread("train_with_kmeans_too.csv")
train$NombreCliente <- as.factor(train$NombreCliente)
#
test <- train[Semana == 9, ]
train <- train[Semana < 9, ]

#Sample train and test for feature importance graph
# train <- train[,.SD[sample(.N,200000)], by = kmns5]
# test <- test[,.SD[sample(.N,2000)], by = kmns5]

#Remove the weeks, as the week column in test will be constant.
train$Semana <- NULL
test$Semana <- NULL
# test[is.na(test)] <- 0
# train[is.na(train)] <- 0

#Save the target variables in separate vectors so that we can remove the original
#data.tables for extra memory space.
train.y <- train$Demanda_uni_equil
test.y <- test$Demanda_uni_equil
test.y <- as.numeric(test.y)
test$Demanda_uni_equil <- NULL

#Create the sparse model matrxi from the train data table.
train.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = as.data.frame(train))

#Create the DMatrix from the sparse model matrix.
dtrain <- xgb.DMatrix(data = train.model, label = train.y)
watchlist <- list(train=dtrain)
rm(train)
#Keep train.model for variable importance plots, if wanted.
rm(train.model)
gc()

#Set the test target to -1 for the test sparse model matrix creation.
test$Demanda_uni_equil <- -1.
test.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = test)
rm(test)
gc()

#rounds_cv = c(13, 15, 17, 20, 23, 25, 27, 30)
depths = c(20)
results = as.numeric(1:length(depths))

#Run the model for each depth in depths.  Write results to csv, and send to slack channel
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
  
  preds <- predict(clf, test.model)
  
  preds[preds < 0] = 0.1
  result = rmsle(test.y, preds)
  
  results[depth_cv] = result
  message = paste0("For eta = 0.2, nrounds = 10, depth = ", depths[depth_cv], ", with 3 week lag, wtpcs, kmns scaled, name, and freq count your rmsle was: ", result)
  slackr(message)
  to_csv = data.frame(nrounds = depths, results = results)
  write.csv(to_csv, "results_with_lag_feature_3weeks_freq_count_wtpcs_name_kmns_scaled_9_rounds.csv", row.names = F)
  rm(preds)
  gc()
}


#Plot the variable importance charts.
# importance_matrix <- xgb.importance(dimnames(train.model)[[2]], model = clf)
# xgb.plot.importance(importance_matrix)
