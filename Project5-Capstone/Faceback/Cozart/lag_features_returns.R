library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)

setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 5 test")
train <- fread("train.csv", select = c('Semana', 'Cliente_ID', 'Producto_ID', 'Demanda_uni_equil','Dev_uni_proxima'),
               colClasses=c(Semana = "numeric",
                            Cliente_ID="numeric", 
                            Producto_ID="numeric", 
                            Demanda_uni_equil="numeric",
                            Dev_uni_proxima="numeric"))
test <- fread("test.csv")
setkey(test, Producto_ID, Cliente_ID, Semana)
setkey(train, Producto_ID, Cliente_ID, Semana)

Client_Prod_Week <- train[, mean(Dev_uni_proxima),by = .(Producto_ID, Cliente_ID, Semana)]
Client_Prod_Week$Dev_uni_proxima <- Client_Prod_Week$V1
Client_Prod_Week$V1 <- NULL
rm(train)
gc()
train_to_save <- fread("train.csv")
train_to_save <- train_to_save[, .(Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID, Demanda_uni_equil),]
setkey(train_to_save, Producto_ID, Cliente_ID)

Client_Prod_Week_9 <- Client_Prod_Week[Semana == 9]
Client_Prod_Week_9$Semana <- NULL
Client_Prod_Week_9$Week9 <- Client_Prod_Week_9$Dev_uni_proxima
Client_Prod_Week_9$Dev_uni_proxima <- NULL
Client_Prod_Week_8 <- Client_Prod_Week[Semana == 8]
Client_Prod_Week_8$Semana <- NULL
Client_Prod_Week_8$Week8 <- Client_Prod_Week_8$Dev_uni_proxima
Client_Prod_Week_8$Dev_uni_proxima <- NULL
Client_Prod_Week_7 <- Client_Prod_Week[Semana == 7]
Client_Prod_Week_7$Semana <- NULL
Client_Prod_Week_7$Week7 <- Client_Prod_Week_7$Dev_uni_proxima
Client_Prod_Week_7$Dev_uni_proxima <- NULL
Client_Prod_Week_6 <- Client_Prod_Week[Semana == 6]
Client_Prod_Week_6$Semana <- NULL
Client_Prod_Week_6$Week6 <- Client_Prod_Week_6$Dev_uni_proxima
Client_Prod_Week_6$Dev_uni_proxima <- NULL
Client_Prod_Week_5 <- Client_Prod_Week[Semana == 5]
Client_Prod_Week_5$Semana <- NULL
Client_Prod_Week_5$Week5 <- Client_Prod_Week_5$Dev_uni_proxima
Client_Prod_Week_5$Dev_uni_proxima <- NULL
Client_Prod_Week_4 <- Client_Prod_Week[Semana == 4]
Client_Prod_Week_4$Semana <- NULL
Client_Prod_Week_4$Week4 <- Client_Prod_Week_4$Dev_uni_proxima
Client_Prod_Week_4$Dev_uni_proxima <- NULL
Client_Prod_Week_3 <- Client_Prod_Week[Semana == 3]
Client_Prod_Week_3$Semana <- NULL
Client_Prod_Week_3$Week3 <- Client_Prod_Week_3$Dev_uni_proxima
Client_Prod_Week_3$Dev_uni_proxima <- NULL

write.csv(Client_Prod_Week, "client_product_mean_per_week_returns.csv", row.names = F)
rm(Client_Prod_Week)
gc()

test_week_10 <- test[Semana == 10]
setkey(test_week_10, Producto_ID, Cliente_ID)
test_week_11 <- test[Semana == 11]
setkey(test_week_11, Producto_ID, Cliente_ID)

train_week_9 <- train_to_save[Semana == 9]
setkey(train_week_9, Producto_ID, Cliente_ID)
train_week_8 <- train_to_save[Semana == 8]
setkey(train_week_8, Producto_ID, Cliente_ID)
train_week_7 <- train_to_save[Semana == 7]
setkey(train_week_7, Producto_ID, Cliente_ID)
train_week_6 <- train_to_save[Semana == 6]
setkey(train_week_6, Producto_ID, Cliente_ID)
train_week_5 <- train_to_save[Semana == 5]
setkey(train_week_5, Producto_ID, Cliente_ID)
train_week_4 <- train_to_save[Semana == 4]
setkey(train_week_4, Producto_ID, Cliente_ID)
train_week_3 <- train_to_save[Semana == 3]
setkey(train_week_3, Producto_ID, Cliente_ID)

train_wk <- train_week_9$Producto_ID

week11lag1week <- merge(test_week_11, Client_Prod_Week_9, all.x = TRUE)
names(week11lag1week)[names(week11lag1week) == "Week9"] = "Lag1"
week11lag2week <- merge(week11lag1week, Client_Prod_Week_9, all.x = TRUE)
rm(week11lag1week)
gc()
week11lag3week <- merge(week11lag2week, Client_Prod_Week_8, all.x = TRUE)
rm(week11lag2week)
gc()
week11lag4week <- merge(week11lag3week, Client_Prod_Week_7, all.x = TRUE)
rm(week11lag3week)
gc()
week11lag5week <- merge(week11lag4week, Client_Prod_Week_6, all.x = TRUE)
rm(week11lag4week)
gc()
week11lag6week <- merge(week11lag5week, Client_Prod_Week_5, all.x = TRUE)
rm(week11lag5week)
gc()
week11lag7week <- merge(week11lag6week, Client_Prod_Week_4, all.x = TRUE)
rm(week11lag6week)
gc()
week11lag8week <- merge(week11lag7week, Client_Prod_Week_3, all.x = TRUE)
rm(week11lag7week)
gc()

write.csv(week11lag8week, "week11lag8weekreturns.csv", row.names = F)
for (i in 6:1) {
  name_old = paste0("Week", i + 2)
  name_new = paste0("Lag", 9 - i)
  names(week11lag8week)[names(week11lag8week) == name_old] = name_new
}
rm(week11lag8week)
gc()

week10lag1week <- merge(test_week_10, Client_Prod_Week_9, all.x = TRUE)
week10lag2week <- merge(week10lag1week, Client_Prod_Week_8, all.x = TRUE)
rm(week10lag1week)
gc()
week10lag3week <- merge(week10lag2week, Client_Prod_Week_7, all.x = TRUE)
rm(week10lag2week)
gc()
week10lag4week <- merge(week10lag3week, Client_Prod_Week_6, all.x = TRUE)
rm(week10lag3week)
gc()
week10lag5week <- merge(week10lag4week, Client_Prod_Week_5, all.x = TRUE)
rm(week10lag4week)
gc()
week10lag6week <- merge(week10lag5week, Client_Prod_Week_4, all.x = TRUE)
rm(week10lag5week)
gc()
week10lag7week <- merge(week10lag6week, Client_Prod_Week_3, all.x = TRUE)
rm(week10lag6week)
gc()
week10lag7week$Week2 <- rep(0, length(week10lag7week$Week3))
week10lag7week[is.na(week10lag7week)] = 0
for (i in 7:0) {
  name_old = paste0("Week", i + 2)
  name_new = paste0("Lag", 8 - i)
  names(week10lag7week)[names(week10lag7week) == name_old] = name_new
}
write.csv(week10lag7week, "week10lag8weekreturns.csv", row.names = F)
rm(week10lag7week)
gc()

week9lag1week <- merge(train_week_9, Client_Prod_Week_8, all.x = TRUE)
week9lag2week <- merge(week9lag1week, Client_Prod_Week_7, all.x = TRUE)
rm(week9lag1week)
gc()
week9lag3week <- merge(week9lag2week, Client_Prod_Week_6, all.x = TRUE)
rm(week9lag2week)
gc()
week9lag4week <- merge(week9lag3week, Client_Prod_Week_5, all.x = TRUE)
rm(week9lag3week)
gc()
week9lag5week <- merge(week9lag4week, Client_Prod_Week_4, all.x = TRUE)
rm(week9lag4week)
gc()
week9lag6week <- merge(week9lag5week, Client_Prod_Week_3, all.x = TRUE)
rm(week9lag5week)
gc()
week9lag6week$Week2 <- rep(0, length(week9lag6week$Week3))
week9lag6week$Week1 <- rep(0, length(week9lag6week$Week3))
week9lag6week[is.na(week9lag6week)] = 0
for (i in 7:0) {
  name_old = paste0("Week", i + 1)
  name_new = paste0("Lag", 8 - i)
  names(week9lag6week)[names(week9lag6week) == name_old] = name_new
}
write.csv(week9lag6week, "week9lag8weekreturns.csv", row.names = F)
rm(week9lag6week)
gc()



week8lag1week <- merge(train_week_8, Client_Prod_Week_7, all.x = TRUE)
week8lag2week <- merge(week8lag1week, Client_Prod_Week_6, all.x = TRUE)
rm(week8lag1week)
gc()
week8lag3week <- merge(week8lag2week, Client_Prod_Week_5, all.x = TRUE)
rm(week8lag2week)
gc()
week8lag4week <- merge(week8lag3week, Client_Prod_Week_4, all.x = TRUE)
rm(week8lag3week)
gc()
week8lag5week <- merge(week8lag4week, Client_Prod_Week_3, all.x = TRUE)
rm(week8lag4week)
gc()
week8lag5week$Week2 <- rep(0, length(week8lag5week$Week3))
week8lag5week$Week1 <- rep(0, length(week8lag5week$Week3))
week8lag5week$Week0 <- rep(0, length(week8lag5week$Week3))
week8lag5week[is.na(week8lag5week)] = 0
for (i in 7:0) {
  name_old = paste0("Week", i)
  name_new = paste0("Lag", 8 - i)
  names(week8lag5week)[names(week8lag5week) == name_old] = name_new
}
write.csv(week8lag5week, "week8lag8weekreturns.csv", row.names = F)
rm(week8lag5week)
gc()

week7lag1week <- merge(train_week_7, Client_Prod_Week_6, all.x = TRUE)
week7lag2week <- merge(week7lag1week, Client_Prod_Week_5, all.x = TRUE)
rm(week7lag1week)
gc()
week7lag3week <- merge(week7lag2week, Client_Prod_Week_4, all.x = TRUE)
rm(week7lag2week)
gc()
week7lag4week <- merge(week7lag3week, Client_Prod_Week_3, all.x = TRUE)
rm(week7lag3week)
gc()
week7lag4week$Week2 <- rep(0, length(week7lag4week$Week3))
week7lag4week$Lag6 <- rep(0, length(week7lag4week$Week3))
week7lag4week$Lag7 <- rep(0, length(week7lag4week$Week3))
week7lag4week$Lag8 <- rep(0, length(week7lag4week$Week3))
week7lag4week[is.na(week7lag4week)] = 0
for (i in 7:3) {
  name_old = paste0("Week", i - 1)
  name_new = paste0("Lag", 8 - i)
  names(week7lag4week)[names(week7lag4week) == name_old] = name_new
}
write.csv(week7lag4week, "week7lag8weekreturns.csv", row.names = F)
rm(week7lag4week)
gc()


week6lag1week <- merge(train_week_6, Client_Prod_Week_5, all.x = TRUE)
week6lag2week <- merge(week6lag1week, Client_Prod_Week_4, all.x = TRUE)
rm(week6lag1week)
gc()
week6lag3week <- merge(week6lag2week, Client_Prod_Week_3, all.x = TRUE)
rm(week6lag2week)
gc()
week6lag3week$Lag4 <- rep(0, length(week6lag3week$Week3))
week6lag3week$Lag5 <- rep(0, length(week6lag3week$Week3))
week6lag3week$Lag6 <- rep(0, length(week6lag3week$Week3))
week6lag3week$Lag7 <- rep(0, length(week6lag3week$Week3))
week6lag3week$Lag8 <- rep(0, length(week6lag3week$Week3))
week6lag3week[is.na(week6lag3week)] = 0
for (i in 7:5) {
  name_old = paste0("Week", i - 2)
  name_new = paste0("Lag", 8 - i)
  names(week6lag3week)[names(week6lag3week) == name_old] = name_new
}
write.csv(week6lag3week, "week6lag8weekreturns.csv", row.names = F)
rm(week6lag3week)
gc()


week5lag1week <- merge(train_week_5, Client_Prod_Week_4, all.x = TRUE)
week5lag2week <- merge(week5lag1week, Client_Prod_Week_3, all.x = TRUE)
rm(week5lag1week)
gc()
week5lag2week$Lag3 <- rep(0, length(week5lag2week$Week3))
week5lag2week$Lag4 <- rep(0, length(week5lag2week$Week3))
week5lag2week$Lag5 <- rep(0, length(week5lag2week$Week3))
week5lag2week$Lag6 <- rep(0, length(week5lag2week$Week3))
week5lag2week$Lag7 <- rep(0, length(week5lag2week$Week3))
week5lag2week$Lag8 <- rep(0, length(week5lag2week$Week3))
week5lag2week[is.na(week5lag2week)] = 0
for (i in 7:6) {
  name_old = paste0("Week", i - 3)
  name_new = paste0("Lag", 8 - i)
  names(week5lag2week)[names(week5lag2week) == name_old] = name_new
}
write.csv(week5lag2week, "week5lag8weekreturns.csv", row.names = F)
rm(week5lag2week)
gc()


week4lag1week <- merge(train_week_4, Client_Prod_Week_3, all.x = TRUE)
week4lag1week$Lag2 <- rep(0, length(week4lag1week$Week3))
week4lag1week$Lag3 <- rep(0, length(week4lag1week$Week3))
week4lag1week$Lag4 <- rep(0, length(week4lag1week$Week3))
week4lag1week$Lag5 <- rep(0, length(week4lag1week$Week3))
week4lag1week$Lag6 <- rep(0, length(week4lag1week$Week3))
week4lag1week$Lag7 <- rep(0, length(week4lag1week$Week3))
week4lag1week$Lag8 <- rep(0, length(week4lag1week$Week3))
week4lag1week[is.na(week4lag1week)] = 0
names(week4lag1week)[names(week4lag1week) == "Week3"] = "Lag1"
write.csv(week4lag1week, "week4lag8weekreturns.csv", row.names = F)
rm(week4lag1week)
gc()



week3lag8week <- train_week_3
week3lag8week$Lag1 <- rep(0, length(week3lag8week$Dev_uni_proxima))
week3lag8week$Lag2 <- rep(0, length(week3lag8week$Dev_uni_proxima))
week3lag8week$Lag3 <- rep(0, length(week3lag8week$Dev_uni_proxima))
week3lag8week$Lag4 <- rep(0, length(week3lag8week$Dev_uni_proxima))
week3lag8week$Lag5 <- rep(0, length(week3lag8week$Dev_uni_proxima))
week3lag8week$Lag6 <- rep(0, length(week3lag8week$Dev_uni_proxima))
week3lag8week$Lag7 <- rep(0, length(week3lag8week$Dev_uni_proxima))
week3lag8week$Lag8 <- rep(0, length(week3lag8week$Dev_uni_proxima))
write.csv(week3lag8week, "week3lag8weekreturns.csv", row.names = F)
rm(week3lag8week)
gc()

