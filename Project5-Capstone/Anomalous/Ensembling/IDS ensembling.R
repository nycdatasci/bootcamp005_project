logit_lasso = readRDS('./logit_lasso_unshuffled.rds')
logit = readRDS('./logit_unshuffled.rds')
nb = readRDS('./bayes_test_pred_unshuffled.rds')
nb = as.numeric(nb)-1

rf = read.csv('../Radhey/Random_Forest_Test.csv')
xg = read.csv('../Radhey/XgBoost_Test.csv')
ab = read.csv('../Radhey/AdaBoost_Test.csv')

merged = cbind(
  #as.double(logit_lasso), 
  #as.double(logit), 
  as.double(rf[,2]), 
  #as.double(xg[,2]), 
  as.double(ab[,2]),
  nb)
#colnames(merged) = c('logit_lasso','logit','rf','xg','ab','nb')


prediction = numeric()
for (i in 1:nrow(merged)) {
  avg = mean(merged[i,])
  prediction[i] = ifelse(avg>0.5,1,0)
}


#Define performance function
performance = function(target, pred) {
  contingency = table(truth = target, prediction = pred)
  cat('Accuracy: ',sum(diag(contingency))/sum(contingency),'\n')
  cat('True positive rate: ',contingency[2,2]/sum(contingency[2,]),'\n')
  cat('False positive rate: ',contingency[1,2]/sum(contingency[1,]),'\n')
  return(contingency)
}
KDD.test$outcome.response <- ifelse(KDD.test[,42] == 'normal',0,1)
performance(KDD.test$outcome.response,prediction)
