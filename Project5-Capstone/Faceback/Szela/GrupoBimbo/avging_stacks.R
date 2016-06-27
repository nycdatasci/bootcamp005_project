preds = fread("predictions_on_test_week_out_6_lag_wtpcs_freq_count_seed1234_eta_005.csv")

for (i in 7:9) {
  to_csv = paste0("predictions_on_test_week_out_", i,"_lag_wtpcs_freq_count_seed1234_eta_005.csv")
  new_preds = fread(to_csv)
  preds = cbind(preds, new_preds$Demanda_uni_equil)
}

for (i in 6:9) {
  to_csv = paste0("./stack_seed1111_lag_weightpieces/predictions_on_test_week_out_", i,"_lag_weightpieces_seed1111.csv")
  new_preds = fread(to_csv)
  preds = cbind(preds, new_preds$Demanda_uni_equil)
}

for (i in 6:9) {
  to_csv = paste0("./stack_seed1234_lag_weightpieces/predictions_on_test_week_out_", i,"_lag_weightpieces_seed1234.csv")
  new_preds = fread(to_csv)
  preds = cbind(preds, new_preds$Demanda_uni_equil)
}

preds.id = preds$id
preds$id <- NULL

preds_avg <- rowMeans(preds)

preds <- data.table(id = preds.id, Demanda_uni_equil = preds_avg)
preds <- preds[order(preds.id)]

write.csv(preds, "avg_preds_from_stack_1234_seed_lag_wtpcs_freq_count_eta_005.csv", row.names = F)
