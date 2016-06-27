library(hash)
library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)
setwd("~/Downloads/GrupoBimbo")
#library(twitteR)
library(slackr)
slackr_setup(channel = "@kes367", username = "i_bimbobot", icon_emoji = "",
             incoming_webhook_url = "datasqawd.slack.com", api_token = "xoxb-51493476675-ZkuheKfwDSdeEtTok0NRPcG6",
             echo = FALSE)
# setup_twitter_oauth('KUPiq7JfGdmRCipxX4R34Wnin', '3AyMcGxAJWODP9LKRVgSxWTpoSkILnwVawZqqhZ7H7XWgZQVoH',
#                     '137572534-ApnOtpw6SjVe7LpBhstUgKxM4FoDQDChIcyPsNiN',
#                     'fHwEf55VJMtWR22nN3zu7SLV4AqDA8DCmCbAMhfhH1cI2')
train <- fread("train_with_lag_weightpieces_stacked_preds.csv")
test <- fread("test_with_lag_weightpieces_stacked_preds.csv")
test <- test[order(test$id),]
#train <- train[, .(Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID, Demanda_uni_equil, Lag1, Lag2, Lag3, Pred),]

train.y <- train$Demanda_uni_equil
test.id <- test$id
test$id <- NULL

test[is.na(test)] <- 0
train[is.na(train)] <- 0

train.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = as.data.frame(train))

dtrain <- xgb.DMatrix(data = train.model, label = train.y)
watchlist <- list(train=dtrain)
rm(train.model)
rm(train)
gc()

test$Demanda_uni_equil <- -1.
test.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = test)

rm(test)
gc()

set.seed(1010)
param <- list(  objective           = "reg:linear",
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.2,
                max_depth           = 17
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

to_csv = data.frame(id = test.id, Demanda_uni_equil = preds)
to_csv = to_csv[order(to_csv$id),]
write.csv(to_csv, "submission_with_stacked_feature_lagweightpieces_nrounds_10_depth17_seed1010.csv", row.names = F)
slackr("Finished with naive xgboost.")
