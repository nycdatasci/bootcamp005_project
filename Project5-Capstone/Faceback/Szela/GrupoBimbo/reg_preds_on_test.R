library(dplyr)
library(Matrix)
library(data.table)
library(Metrics)
setwd("~/Downloads/GrupoBimbo")

train_week = fread("./lags/week10lag8week.csv")

train.id <- train_week$id

train_lags = train_week[, .(Lag1, Lag2, Lag3, Lag4, Lag5, Lag6, Lag7)]

y_avg = rowMeans(train_lags)

x_avg = rep(1, length(y_avg)) * ((1 + 2 + 3 + 4 + 5 + 6 + 7) / 7)

beta_numerator = ((train_lags$Lag1 - y_avg) * (7 - x_avg)) + 
  ((train_lags$Lag2 - y_avg) * (6 - x_avg)) + 
  ((train_lags$Lag3 - y_avg) * (5 - x_avg)) + 
  ((train_lags$Lag4 - y_avg) * (4 - x_avg)) + 
  ((train_lags$Lag5 - y_avg) * (3 - x_avg)) + 
  ((train_lags$Lag6 - y_avg) * (2 - x_avg)) + 
  ((train_lags$Lag7 - y_avg) * (1 - x_avg))

beta_denominator = ((6 - x_avg)^2) + 
  ((5 - x_avg)^2) + 
  ((4 - x_avg)^2) + 
  ((3 - x_avg)^2) + 
  ((2 - x_avg)^2) + 
  ((1 - x_avg)^2) + 
  ((7 - x_avg)^2)

beta_1 = beta_numerator / beta_denominator
beta_0 = y_avg - (beta_1 * x_avg)

preds_from_reg = beta_0 + (beta_1 * 8)

week_10_preds_reg = data.table(id = train.id, preds = preds_from_reg)



train_week = fread("./lags/week11lag8week.csv")

train.id <- train_week$id

train_lags = train_week[, .(Lag1, Lag2, Lag3, Lag4, Lag5, Lag6, Lag7, Lag8)]

y_avg = rowMeans(train_lags)

x_avg = rep(1, length(y_avg)) * ((1 + 2 + 3 + 4 + 5 + 6 + 7 + 8) / 8)

beta_numerator = ((train_lags$Lag1 - y_avg) * (8 - x_avg)) + 
  ((train_lags$Lag2 - y_avg) * (7 - x_avg)) + 
  ((train_lags$Lag3 - y_avg) * (6 - x_avg)) + 
  ((train_lags$Lag4 - y_avg) * (5 - x_avg)) + 
  ((train_lags$Lag5 - y_avg) * (4 - x_avg)) + 
  ((train_lags$Lag6 - y_avg) * (3 - x_avg)) + 
  ((train_lags$Lag7 - y_avg) * (2 - x_avg)) + 
  ((train_lags$Lag8 - y_avg) * (1 - x_avg))

beta_denominator = ((6 - x_avg)^2) + 
  ((5 - x_avg)^2) + 
  ((4 - x_avg)^2) + 
  ((3 - x_avg)^2) + 
  ((2 - x_avg)^2) + 
  ((1 - x_avg)^2) + 
  ((7 - x_avg)^2) + 
  ((8 - x_avg)^2)

beta_1 = beta_numerator / beta_denominator
beta_0 = y_avg - (beta_1 * x_avg)

preds_from_reg = beta_0 + (beta_1 * 9)

week_11_preds_reg = data.table(id = train.id, preds = preds_from_reg)

preds_from_reg = rbind(week_10_preds_reg, week_11_preds_reg)

preds_from_reg = preds_from_reg[order(id)]
preds_from_reg = preds_from_reg$preds

best_preds = fread("avg_preds_from_stack_3_seeds_lag_weightpieces.csv")

preds <- best_preds$Demanda_uni_equil
preds_from_reg[preds_from_reg == 0] = preds[preds_from_reg == 0]
preds_from_reg[preds_from_reg < 0] = 0

submission <- data.table(id = best_preds$id, Demanda_uni_equil = ((preds * 0.96) + (preds_from_reg * 0.04)))
write.csv(submission, "best_preds_and_preds_from_reg_96_04.csv", row.names = F)


