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
setwd("~/Downloads/GrupoBimbo")
train <- fread("train_with_lag.csv")
test <- fread("./lags/week9lag8week.csv")

train <- train[Semana < 9, ]
train[is.na(train)] <- 0

test$id <- 1:length(test$Demanda_uni_equil)
test <- test[, .(Producto_ID, Cliente_ID, Semana, Agencia_ID, Canal_ID,
                      Ruta_SAK, Demanda_uni_equil, Lag1, Lag2, Lag3, id)]
test.id <- test$id
test$id <- NULL


train.y <- train$Demanda_uni_equil
test.y <- test$Demanda_uni_equil
test.y <- as.numeric(test.y)
test$Demanda_uni_equil <- NULL

train.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = as.data.frame(train))

dtrain <- xgb.DMatrix(data = train.model, label = train.y)
watchlist <- list(train=dtrain)
rm(train)
rm(train.model)
gc()

test$Demanda_uni_equil <- -1.
test.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = test)
rm(test)
gc()

set.seed(1234)
param <- list(  objective           = "reg:linear",
                booster             = "gbtree",
                eval_metric         = "rmse",
                eta                 = 0.2,
                max_depth           = 25
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

rmsle(test.y, preds)
save_file <- data.table(id = test.id, Demanda_uni_equil = test.y, preds = preds)
write.csv(save_file, "./lags/from_cv.csv", row.names = F)