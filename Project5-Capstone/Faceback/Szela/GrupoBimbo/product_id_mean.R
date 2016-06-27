library(hash)
library(data.table)
library(foreach)
library(dplyr)
setwd("~/Downloads/GrupoBimbo")
train <- fread("train.csv")
test <- fread("test.csv")

train_id_72 = train[Producto_ID == 72,]

plot(train_id_72$Agencia_ID, train_id_72$Demanda_uni_equil)
unique(train_id_72[, Agencia_ID])
train <- train[Semana == 9,]
train <- train[order(Producto_ID)]
#test <- test[order(Producto_ID)]
impute_mean = mean(train$Demanda_uni_equil)
#test_chunks <- split(test, test$Producto_ID)
train <- split(train, train$Producto_ID)

hash_keys = character(length = length(train))
hash_values = 1:length(train)
for (i in 1:length(train)) {
  train_chunk = train[[i]]
  mean_demand = mean(train_chunk$Demanda_uni_equil)
  product_id = as.character(train_chunk$Producto_ID[1])
  hash_keys[i] = product_id
  hash_values[i] = mean_demand
}


lookup_table = hash(hash_keys, hash_values)

submission = 1:length(test$Producto_ID)

for (i in 1:length(test$Producto_ID)) {
  if (i %% 100000 == 0) {
    print(i)
  }
  if (has.key(as.character(test$Producto_ID[i]), lookup_table)){
    submission[i] = lookup_table[[as.character(test$Producto_ID[i])]]
  } else {
    submission[i] = impute_mean
  }
}

#rmsle(test$Demanda_uni_equil, submission)

to_csv = data.frame(id = test$id, Demanda_uni_equil = submission)
write.csv(to_csv, "submission.csv", row.names = F)

