library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)

setwd("~/Downloads/GrupoBimbo")
train <- fread("train.csv")
client_table <- fread("cliente_tabla 2.csv")

client_table$Cliente_ID = as.numeric(client_table$Cliente_ID)
client_table$NombreCliente = as.character(client_table$NombreCliente)
train$Producto_ID = as.numeric(train$Producto_ID)
train$Agencia_ID = as.numeric(train$Agencia_ID)
train$Ruta_SAK = as.numeric(train$Ruta_SAK)
train$Cliente_ID = as.numeric(train$Cliente_ID)
train$Canal_ID = as.numeric(train$Canal_ID)
train$Demanda_uni_equil = as.numeric(train$Demanda_uni_equil)

setkey(client_table, Cliente_ID, NombreCliente)
setkey(train, Producto_ID, Cliente_ID, Agencia_ID, Ruta_SAK)

train <- merge(train, client_table, all.x = TRUE, by = "Cliente_ID")

train_train <- train[Semana != 9]

train_test <- train[Semana == 9]

setkey(train_test, Producto_ID, Cliente_ID, Agencia_ID, Ruta_SAK, NombreCliente)
setkey(train_train, Producto_ID, Cliente_ID, Agencia_ID, Ruta_SAK, NombreCliente)

actual <- train_test$Demanda_uni_equil
train_test$Demanda_uni_equil <- NULL

median <- train_train[, median(Demanda_uni_equil)]

median_Prod <- train_train[, median(Demanda_uni_equil), by = Producto_ID]
setnames(median_Prod,"V1","M2")

median_Client_Prod <- train_train[, median(Demanda_uni_equil),by = .(Producto_ID,Cliente_ID)]
setnames(median_Client_Prod,"V1","M3")

median_Nombre_Prod <- train_train[, median(Demanda_uni_equil), by = .(Producto_ID, NombreCliente)]
setnames(median_Nombre_Prod, "V1", "M4")
setkey(median_Nombre_Prod, Producto_ID, NombreCliente)

# median_Agencia_Prod <- train_train[, median(Demanda_uni_equil), by = .(Producto_ID, Agencia_ID)]
# setnames(median_Agencia_Prod, "V1", "M4")
# 
# median_Ruta_Prod <- train_train[, median(Demanda_uni_equil), by = .(Producto_ID, Ruta_SAK)]
# setnames(median_Ruta_Prod, "V1", "M5")
# 
# median_Agencia_Ruta_Prod <- train_train[, median(Demanda_uni_equil), by = .(Producto_ID, Ruta_SAK, Agencia_ID)]
# setnames(median_Agencia_Ruta_Prod, "V1", "M6")

submit <- merge(train_test, median_Client_Prod, all.x = TRUE)
#submit$M4 <- merge(train_test, median_Agencia_Prod, all.x = TRUE, by = c("Producto_ID", "Agencia_ID"))$M4
#submit$M5 <- merge(train_test, median_Ruta_Prod, all.x = TRUE, by = c("Producto_ID", "Ruta_SAK"))$M5
submit$M2 <- merge(train_test, median_Prod, by = "Producto_ID", all.x = TRUE)$M2
submit$M4 <- merge(train_test, median_Nombre_Prod, all.x = TRUE)$M4
#submit$M6 <- merge(train_test, median_Agencia_Ruta_Prod, by = c("Producto_ID", "Agencia_ID", "Ruta_SAK"), all.x = TRUE)$M6

submit$Pred <- submit$M3
submit[is.na(M3)]$Pred <- submit[is.na(M3)]$M4
submit[is.na(Pred)]$Pred <- submit[is.na(Pred)]$M2
submit[is.na(Pred)]$Pred <- median

submit$Pred <- submit$M3
submit[is.na(M3)]$Pred <- submit[is.na(M3)]$M2
submit[is.na(Pred)]$Pred <- median


rmsle(actual, submit$Pred)
