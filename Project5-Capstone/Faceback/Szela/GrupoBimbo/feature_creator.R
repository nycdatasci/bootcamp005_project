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
train <- fread("train_5week_lag.csv")
test <- fread("test_5week_lag.csv")
town <- fread("town_state_with_town_id.csv")

test.id <- test$id
test$id <- NULL
train.y <- train$Demanda_uni_equil
train$Demanda_uni_equil <- NULL
train$separator <- rep(1, length(train$Cliente_ID))
test$separator <- rep(0, length(test$Cliente_ID))

data_set <- rbind(train, test)
data_set$separator2 <- 1:length(data_set$Producto_ID)
data_set[is.na(data_set)] <- 0
#data_set$wtpcs <- data_set$weight / data_set$pieces

town = town[ , .(Agencia_ID, town_id)]
data_set = merge(data_set, town, by = "Agencia_ID", all.x = TRUE)

client_by_town <-  data_set %>% 
  select(town_id, Cliente_ID)

freq_client <- client_by_town %>% 
  group_by(town_id, Cliente_ID) %>%
  summarise(count = n())

data_set = merge(data_set, freq_client, by = c("town_id", "Cliente_ID"), all.x = TRUE)

train <- data_set[separator == 1]
test <- data_set[separator == 0]

test <- test[order(separator2)]
train <- train[order(separator2)]

test$separator <- NULL
test$separator2 <- NULL
test$id <- test.id

train$separator <- NULL
train$separator2 <- NULL
train$Demanda_uni_equil <- train.y

write.csv(train, "train_lag_5weeks_freq_count.csv", row.names = F)
write.csv(test, "test_lag_5weeks_freq_count.csv", row.names = F)
