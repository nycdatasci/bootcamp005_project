library(data.table)
library(Metrics)
library(foreach)
library(dplyr)
library(xgboost)

preds_0 = fread("./stack_seed0_lag_weightpieces/predictions_on_test_week_out_6_lag_weightpieces.csv")

for (i in 7:9) {
  to_csv = paste0("./stack_seed0_lag_weightpieces/predictions_on_test_week_out_", i,"_lag_weightpieces.csv")
  new_preds = fread(to_csv)
  preds_0 = cbind(preds_0, new_preds$Demanda_uni_equil)
}

preds_1111 = fread("./stack_seed1111_lag_weightpieces/predictions_on_test_week_out_6_lag_weightpieces_seed1111.csv")

for (i in 7:9) {
  to_csv = paste0("./stack_seed1111_lag_weightpieces/predictions_on_test_week_out_", i,"_lag_weightpieces_seed1111.csv")
  new_preds = fread(to_csv)
  preds_1111 = cbind(preds_1111, new_preds$Demanda_uni_equil)
}

preds_1234 = fread("./stack_seed1234_lag_weightpieces/predictions_on_test_week_out_6_lag_weightpieces_seed1234.csv")

for (i in 7:9) {
  to_csv = paste0("./stack_seed1234_lag_weightpieces/predictions_on_test_week_out_", i,"_lag_weightpieces_seed1234.csv")
  new_preds = fread(to_csv)
  preds_1234 = cbind(preds_1234, new_preds$Demanda_uni_equil)
}

preds.id = preds_0$id
preds_0$id <- NULL
preds_1111$id <- NULL
preds_1234$id <- NULL


preds_avg_0 <- rowMeans(preds_0)
preds_avg_1111 <- rowMeans(preds_1111)
preds_avg_1234 <- rowMeans(preds_1234)

test <- fread("test_with_lag_and_weightpieces.csv")
test$stack0 <- preds_avg_0
test$stack1111 <- preds_avg_1111
test$stack1234 <- preds_avg_1234

write.csv(test, "test_with_lag_weightpieces_stacked_preds.csv", row.names = F)

preds_0 = fread("./stack_seed0_lag_weightpieces/week_6_predictions_from_stack_lag_weightpieces.csv")

for (i in 7:9) {
  to_csv = paste0("./stack_seed0_lag_weightpieces/week_", i,"_predictions_from_stack_lag_weightpieces.csv")
  new_preds = fread(to_csv)
  preds_0 = rbind(preds_0, new_preds)
}

preds_1111 = fread("./stack_seed1111_lag_weightpieces/week_6_predictions_from_stack_lag_weightpieces_seed1111.csv")

for (i in 7:9) {
  to_csv = paste0("./stack_seed1111_lag_weightpieces/week_", i,"_predictions_from_stack_lag_weightpieces_seed1111.csv")
  new_preds = fread(to_csv)
  preds_1111 = rbind(preds_1111, new_preds)
}

preds_1234 = fread("./stack_seed1234_lag_weightpieces/week_6_predictions_from_stack_lag_weightpieces_seed1234.csv")

for (i in 7:9) {
  to_csv = paste0("./stack_seed1234_lag_weightpieces/week_", i,"_predictions_from_stack_lag_weightpieces_seed1234.csv")
  new_preds = fread(to_csv)
  preds_1234 = rbind(preds_1234, new_preds)
}

rm(new_preds)
gc()

train <- fread("train_with_lag_and_weightpieces.csv")
train$id <- 1:length(train$Semana)

preds_0 <- preds_0[order(id)]
preds_1111 <- preds_1111[order(id)]
preds_1234 <- preds_1234[order(id)]

train$stack0 <- preds_0$Demanda_uni_equil
train$stack1111 <- preds_1111$Demanda_uni_equil
train$stack1234 <- preds_1234$Demanda_uni_equil

train$id <- NULL

write.csv(train, "train_with_lag_weightpieces_stacked_preds.csv", row.names = F)

