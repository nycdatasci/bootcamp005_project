library(dplyr)
library(Matrix)
library(data.table)
library(Metrics)
setwd("~/Downloads/GrupoBimbo")

train_week = fread("./lags/week9lag8week.csv")
train_week$id = 1:length(train_week$Producto_ID)

train_cv_preds = fread("./lags/from_cv.csv")

train_lags = train_week[, .(Lag1, Lag2, Lag3, Lag4, Lag5, Lag6)]

y_avg = rowMeans(train_lags)

x_avg = rep(1, length(x_avg)) * ((1 + 2 + 3 + 4 + 5 + 6) / 6)

beta_numerator = ((train_lags$Lag1 - y_avg) * (6 - x_avg)) + 
  ((train_lags$Lag2 - y_avg) * (5 - x_avg)) + 
  ((train_lags$Lag3 - y_avg) * (4 - x_avg)) + 
  ((train_lags$Lag4 - y_avg) * (3 - x_avg)) + 
  ((train_lags$Lag5 - y_avg) * (2 - x_avg)) + 
  ((train_lags$Lag6 - y_avg) * (1 - x_avg))
  
beta_denominator = ((6 - x_avg)^2) + 
  ((5 - x_avg)^2) + 
  ((4 - x_avg)^2) + 
  ((3 - x_avg)^2) + 
  ((2 - x_avg)^2) + 
  ((1 - x_avg)^2)
  
beta_1 = beta_numerator / beta_denominator
beta_0 = y_avg - (beta_1 * x_avg)

preds_from_reg = beta_0 + (beta_1 * 7)
preds <- train_cv_preds$preds
preds_from_reg_temp = preds_from_reg
preds_from_reg[preds_from_reg == 0] = preds[preds_from_reg == 0]
preds_from_reg[preds_from_reg < 0] = 0

actual <- train_cv_preds$Demanda_uni_equil
rmsle(actual, preds)
rmsle(actual, preds_from_reg)
rmsle(actual, (preds * 0.96) + (preds_from_reg * 0.04))

