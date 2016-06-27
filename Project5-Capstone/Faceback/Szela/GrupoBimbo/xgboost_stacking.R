library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)
library(twitteR)
library(doMC)
registerDoMC(2)
library(slackr)
# setup_twitter_oauth('KUPiq7JfGdmRCipxX4R34Wnin', '3AyMcGxAJWODP9LKRVgSxWTpoSkILnwVawZqqhZ7H7XWgZQVoH',
#                     '137572534-ApnOtpw6SjVe7LpBhstUgKxM4FoDQDChIcyPsNiN',
#                     'fHwEf55VJMtWR22nN3zu7SLV4AqDA8DCmCbAMhfhH1cI2')
slackr_setup(channel = "@kes367", username = "i_bimbobot", icon_emoji = "",
             incoming_webhook_url = "datasqawd.slack.com", api_token = "xoxb-51493476675-ZkuheKfwDSdeEtTok0NRPcG6",
             echo = FALSE)
setwd("~/Downloads/GrupoBimbo")
train <- fread("train_Lag5weeks_wtpcs_freqcount_with_kmeans_too.csv")
test_not_cv <- fread("test_Lag5weeks_wtpcs_freqcount_with_kmeans_too.csv")

train$id <- 1:length(train$Semana)
train$NombreCliente <- as.factor(train$NombreCliente)
test_not_cv$NombreCliente <- as.factor(test_not_cv$NombreCliente)
test_not_cv.id <- test_not_cv$id
test_not_cv$id <- NULL
test_not_cv$Semana <- NULL

for (i in 8:9) {
  test.cv <- train[Semana == i, ]
  train.cv <- train[Semana != i, ]
  
  test.cv$Semana <- NULL
  train.cv$Semana <- NULL
  
  train.y <- train.cv$Demanda_uni_equil
  train.cv$id <- NULL
  test.id <- test.cv$id
  test.cv$id <- NULL
  test.y <- test.cv$Demanda_uni_equil
  test.cv$Demanda_uni_equil <- NULL
  
  train.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = train.cv)
  
  dtrain <- xgb.DMatrix(data = train.model, label = train.y)
  rm(train.model)
  gc()
  
  watchlist <- list(train=dtrain)
  
  rm(train.cv)
  gc()
  
  test.cv$Demanda_uni_equil <- -1.
  test.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = test.cv)
  rm(test.cv)
  gc()
  
  set.seed(9999)
  param <- list(  objective           = "reg:linear",
                  booster             = "gbtree",
                  eval_metric         = "rmse",
                  eta                 = 0.2,
                  max_depth           = 21
  )
  message = paste0("Training model for week ", i, "!")
  slackr(message)
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 9,
                      verbose             = 1,
                      watchlist           = watchlist,
                      maximize            = FALSE
  )
  rm(dtrain)
  gc()
  
  preds.cv <- predict(clf, test.model)
  rm(test.model)
  gc()
  
  preds.cv[preds.cv < 0] = 0.1
  
  result = rmsle(test.y, preds.cv)
  message = paste0("For eta = 0.2, nrounds = 9, depth = 23, preds on week ", i, ", with weight/pieces, client name, lag 5 weeks, and freq count your rmsle was: ", result)
  slackr(message)
  
  for_csv = data.frame(id = test.id, Demanda_uni_equil = preds.cv)
  to_csv_test_week = paste0("week_", i, "_predictions_from_stack_lag5weeks_wtpcs_freq_count_seed9999_21d.csv")
  write.csv(for_csv, to_csv_test_week, row.names = F)
  rm(for_csv)
  rm(to_csv_test_week)
  rm(preds.cv)
  gc()
  
  test_not_cv$Demanda_uni_equil <- -1
  test.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = test_not_cv)
  
  preds <- predict(clf, test.model)
  rm(test.model)
  rm(clf)
  gc()
  
  preds[preds < 0] = 0.1
  
  for_csv = data.frame(id = test_not_cv.id, Demanda_uni_equil = preds)
  to_csv = paste0("predictions_on_test_week_out_", i,"_lag5weeks_wtpcs_freq_count_seed9999_21d.csv")
  write.csv(for_csv, to_csv, row.names = F)
  rm(for_csv)
  rm(to_csv)
  rm(preds)
  gc()
  
  message = paste0("Finished stacking week ", i, "!")
  slackr(message)
}
slackr('Finished total stacking!')