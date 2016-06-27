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
library(stats)
# setup_twitter_oauth('KUPiq7JfGdmRCipxX4R34Wnin', '3AyMcGxAJWODP9LKRVgSxWTpoSkILnwVawZqqhZ7H7XWgZQVoH',
#                     '137572534-ApnOtpw6SjVe7LpBhstUgKxM4FoDQDChIcyPsNiN',
#                     'fHwEf55VJMtWR22nN3zu7SLV4AqDA8DCmCbAMhfhH1cI2')
slackr_setup(channel = "@kes367", username = "i_bimbobot", icon_emoji = "",
             incoming_webhook_url = "datasqawd.slack.com", api_token = "xoxb-51493476675-ZkuheKfwDSdeEtTok0NRPcG6",
             echo = FALSE)
setwd("~/GitHub-kszela24/Faceback/Szela/GrupoBimbo")
train <- fread("train_lag_wtpcs_freq_count.csv")
test <- fread("test_lag_wtpcs_freq_count.csv")

train_clustering <- train[, .(Lag1, Lag2, Lag3, weight, pieces, wtpcs, count)]
test_clustering <- test[, .(Lag1, Lag2, Lag3, weight, pieces, wtpcs, count)]

dataset <- rbind(train_clustering, test_clustering)
dataset$id <- 1:length(dataset$Lag1)
dataset.id <- dataset$id

rm(test_clustering)
rm(train_clustering)
gc()

dataset$weight <- as.numeric(dataset$weight)
dataset$pieces <- as.numeric(dataset$pieces)
dataset$count <- as.numeric(dataset$count)
dataset$id <- NULL

range01 <- function(x){(x-min(x))/(max(x)-min(x))}


dataset$Lag1 <- scale(dataset$Lag1, center = TRUE, scale = TRUE)
dataset$Lag2 <- scale(dataset$Lag2, center = TRUE, scale = TRUE)
dataset$Lag3 <- scale(dataset$Lag3, center = TRUE, scale = TRUE)
# dataset$Lag4 <- range01(dataset$Lag4)
# dataset$Lag5 <- range01(dataset$Lag5)
dataset$weight <- scale(dataset$weight, center = TRUE, scale = TRUE)
dataset$pieces <- scale(dataset$pieces, center = TRUE, scale = TRUE)
dataset$wtpcs <- scale(dataset$wtpcs, center = TRUE, scale = TRUE)
dataset$count <- scale(dataset$count, center = TRUE, scale = TRUE)

kmns <- kmeans(dataset, 2, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
kmns_2 <- kmns$cluster
kmns <- kmeans(dataset, 3, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
kmns_3 <- kmns$cluster
kmns <- kmeans(dataset, 4, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
kmns_4 <- kmns$cluster
kmns <- kmeans(dataset, 5, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
kmns_5 <- kmns$cluster
kmns <- kmeans(dataset, 6, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
kmns_6 <- kmns$cluster
kmns <- kmeans(dataset, 7, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
kmns_7 <- kmns$cluster
kmns <- kmeans(dataset, 8, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
kmns_8 <- kmns$cluster
kmns <- kmeans(dataset, 9, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
kmns_9 <- kmns$cluster
kmns <- kmeans(dataset, 10, iter.max = 100, nstart = 1, algorithm = "Hartigan-Wong")
kmns_10 <- kmns$cluster
slackr("Kmeans clustering finished.")

dataset$kmns2 <- kmns_2
dataset$kmns3 <- kmns_3
dataset$kmns4 <- kmns_4
dataset$kmns5 <- kmns_5
dataset$kmns6 <- kmns_6
dataset$kmns7 <- kmns_7
dataset$kmns8 <- kmns_8
dataset$kmns9 <- kmns_9
dataset$kmns10 <- kmns_10

dataset$id <- dataset.id


train_new_feats <- dataset[id <= length(train$town_id)]
test_new_feats <- dataset[id > length(train$town_id)]

test$kmns2 <- test_new_feats$kmns2
test$kmns3 <- test_new_feats$kmns3
test$kmns4 <- test_new_feats$kmns4
test$kmns5 <- test_new_feats$kmns5
test$kmns6 <- test_new_feats$kmns6
test$kmns7 <- test_new_feats$kmns7
test$kmns8 <- test_new_feats$kmns8
test$kmns9 <- test_new_feats$kmns9
test$kmns10 <- test_new_feats$kmns10

train$kmns2 <- train_new_feats$kmns2
train$kmns3 <- train_new_feats$kmns3
train$kmns4 <- train_new_feats$kmns4
train$kmns5 <- train_new_feats$kmns5
train$kmns6 <- train_new_feats$kmns6
train$kmns7 <- train_new_feats$kmns7
train$kmns8 <- train_new_feats$kmns8
train$kmns9 <- train_new_feats$kmns9
train$kmns10 <- train_new_feats$kmns10

test <- test[order(id)]

write.csv(test, "test_lag3weeks_wtpcs_freqcount_with_kmeans_scaled.csv", row.names = F)
write.csv(train, "train_lag3weeks_wtpcs_freqcount_with_kmeans_scaled.csv", row.names = F)

