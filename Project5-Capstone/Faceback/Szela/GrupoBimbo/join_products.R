library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)

setwd("~/Downloads/GrupoBimbo")
train <- fread("train_lag_5weeks_freq_count.csv")
test <- fread("test_lag_5weeks_freq_count.csv")
client_table <- fread("cliente_tabla.csv")
product_table <- fread("preprocessed_products.csv")

client_table$NombreCliente <- as.factor(client_table$NombreCliente)
freqs <- table(client_table$NombreCliente)
freqs <- as.data.table(freqs)
freqs <- freqs[order(-freqs$N)]

new_client_table <- client_table[NombreCliente == "ABARROTES"]
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "OXXO"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "SUPER"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "MINI SUPER"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "COMODIN"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "FARMACIA"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "CAFE"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "DEPOSITO"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "MODELORAMA"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "7 ELEVEN"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "HOTEL"])
new_client_table <- rbind(new_client_table, client_table[NombreCliente == "CREMERIA"])
new_client_table$NombreCliente <- as.character(new_client_table$NombreCliente)
new_client_table$NombreCliente <- as.factor(new_client_table$NombreCliente)
new_client_table$Cliente_ID <- as.numeric(new_client_table$Cliente_ID)
setkey(new_client_table, Cliente_ID)
setkey(train, Cliente_ID)
setkey(test, Cliente_ID)

train <- merge(train, new_client_table, all.x = TRUE)
test <- merge(test, new_client_table, all.x = TRUE)
train[is.na(train$NombreCliente)]$NombreCliente = "Unknown"
test[is.na(test$NombreCliente)]$NombreCliente = "Unknown"

product.id <- product_table$ID
product.brand <- product_table$brand
product_id_brand <- data.table(Producto_ID = product.id, brand = product.brand)

product_id_brand$Producto_ID = as.numeric(product_id_brand$Producto_ID)
product_table$Producto_ID = as.numeric(product_table$ID)
product_table[is.na(product_table$pieces)]$pieces = 1
product_table[is.na(product_table$weight)]$weight = 0
new_product_table = data.table(weight = product_table$weight, 
                               pieces = product_table$pieces,
                               Producto_ID = product_table$Producto_ID)

new_product_table$weight = as.numeric(new_product_table$weight)
new_product_table$pieces = as.numeric(new_product_table$pieces)

# client_table$NombreCliente = as.factor(client_table$NombreCliente)
# client_table$Cliente_ID = as.numeric(client_table$Cliente_ID)

test$Producto_ID = as.numeric(test$Producto_ID)
train$Producto_ID = as.numeric(train$Producto_ID)
# train$Agencia_ID = as.numeric(train$Agencia_ID)
# train$Ruta_SAK = as.numeric(train$Ruta_SAK)
train$Cliente_ID = as.numeric(train$Cliente_ID)
# train$Canal_ID = as.numeric(train$Canal_ID)
# train$Demanda_uni_equil = as.numeric(train$Demanda_uni_equil)
test$Cliente_ID = as.numeric(test$Cliente_ID)
product_id_brand$brand = as.factor(product_id_brand$brand)
product_id_brand$brand = as.numeric(product_id_brand$brand)

setkey(train, Cliente_ID)
setkey(test, Cliente_ID)
setkey(client_table, Cliente_ID)
setkey(train, Producto_ID)
setkey(test, Producto_ID)
setkey(product_id_brand, Producto_ID)
setkey(new_product_table, Producto_ID)

# testing <- merge(train, client_table_real, all.x = TRUE, by = "Cliente_ID")
# client_table_real <- client_table[unique(client_table$Cliente_ID)]
train <- merge(train, new_product_table, all.x = TRUE, by = "Producto_ID")
test <- merge(test, new_product_table, all.x = TRUE, by = "Producto_ID")

train$wtpcs <- train$weight / train$pieces
test$wtpcs <- test$weight / test$pieces

# train <- merge(train, product_id_brand, all.x = TRUE)
# test <- merge(test, product_id_brand, all.x = TRUE)

write.csv(train, "train_with_lag5weeks_and_weightpieces_clientname_freq.csv", row.names = F)
write.csv(test, "test_with_lag5weeks_and_weightpieces_clientname_freq.csv", row.names = F)
