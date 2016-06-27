#################################################
####       Network Intrusion Detection       ####
####             Naive Bayes                 ####
#################################################

library(e1071)
# library(caret)
# library(klaR)

#---------------------- Define functions ---------------------------#
#Define performance function
performance = function(target, pred) {
  contingency = table(truth = target, prediction = pred)
  cat('Accuracy: ',sum(diag(contingency))/sum(contingency),'\n')
  cat('True positive rate: ',contingency[2,2]/sum(contingency[2,]),'\n')
  cat('False positive rate: ',contingency[1,2]/sum(contingency[1,]),'\n')
  return(contingency)
}

#Factorize columns
factor.bin = function(df) {
  bin.col = data.frame(matrix(ncol = 2, nrow = ncol(df)))
  colnames(bin.col) = c('col','unique')
  for (i in 1:ncol(df)) {
    bin.col[i,1]=i
    bin.col[i,2]=length(unique(df[,i]))
    # bin.col[i,3]=max(df[,i])
    # bin.col[i,4]=min(df[,i])
  }
  bin.col = bin.col[bin.col$unique==2,1]
  for (i in bin.col){
    df[,i] = as.factor(df[,i])
  }
  for (i in c('protocol_type','service','flag')) {
    df[,i] = as.factor(df[,i])
  }
  return(df)
}


#---------------------- Load data for unshuffled ---------------------------#
colnames(KDD.train)[42:43]=c('outcome','outcome.response')
KDD.train[43] <- ifelse(KDD.train$outcome == 'normal',0,1)
bayes_train = factor.bin(KDD.train[, -c(20,42)])

colnames(KDD.test)[42]=c('outcome','outcome.response')
KDD.test[43] <- ifelse(KDD.test$outcome == 'normal',0,1)
bayes_test = factor.bin(KDD.test[, -c(20,42)])


#---------------------- Load data for shuffled ---------------------------#
colnames(KDD.shuffle)[42:43]=c('outcome','outcome.response')
KDD.shuffle[43] <- ifelse(KDD.shuffle$outcome == 'normal',0,1)
bayes_train = factor.bin(KDD.shuffle[shuffle.train, -c(20,42)])
bayes_test = factor.bin(KDD.shuffle[-shuffle.train, -c(20,42)])


#---------------------- Split test and train into predictor and labels ---------------------------#
# bayes_train_labels = as.factor(bayes_train[,41])
# bayes_train = bayes_train[,-41]
# bayes_test_labels = as.factor(bayes_test[,41])
# bayes_test = bayes_test[,-41]

#---------------------- Reduce data size for quick testing ---------------------------#
# set.seed(10)
# index = sample(1:nrow(bayes_train),0.1*nrow(bayes_train)) #Temp
# bayes_train=bayes_train[index,]
# bayes_train_labels=bayes_train_labels[index]

#---------------------- Tune and train model ---------------------------#
tune.control = tune.control(sampling="cross",
                            sampling.aggregate=mean, 
                            cross=10)

obj = tune(naiveBayes, 
           outcome.response~.,
           data = bayes_train,
           ranges = list(laplace = 0:1),
           tunecontrol = tune.control)
# obj.unshuffled = obj
#Unshuffled, 10-CV: laplace = 1; class error: 0.1071023 
# obj.shuffled = obj
#Shuffled, 10-CV: laplace = 1; class error: 0.1204306 

#---------------------- Performance on unshuffled ---------------------------#
# Unshuffled Train
best.nb.unshuffled = obj.unshuffled$best.model
bayes_train_pred_unshuffled = predict(best.nb.unshuffled, bayes_train[,1:40])
performance(bayes_train_pred_unshuffled, bayes_train[,41])
# Accuracy:  0.8934851 
# True positive rate:  0.8773453 
# False positive rate:  0.09187921 
# prediction
# truth     0     1
# 0 59995  6070
# 1  7348 52560

# Unshuffled Test
bayes_test_pred_unshuffled = predict(best.nb.unshuffled, bayes_test[,1:40])
performance(bayes_test_pred_unshuffled, bayes_test[,41])
# Accuracy:  0.7779799 
# True positive rate:  0.9175379 
# False positive rate:  0.3213608 
# prediction
# truth    0    1
# 0 8937 4232
# 1  773 8601

#---------------------- Performance on shuffled ---------------------------#
# Shuffled Train
best.nb.shuffled = obj.shuffled$best.model
bayes_train_pred_shuffled = predict(best.nb.shuffled, bayes_train[,1:40])
performance(bayes_train_pred_shuffled, bayes_train[,41])
# Accuracy:  0.8796409 
# True positive rate:  0.879679 
# False positive rate:  0.1203936 
# prediction
# truth     0     1
# 0 58193  7965
# 1  7197 52618

# Shuffled Test
bayes_test_pred_shuffled = predict(best.nb.shuffled, bayes_test[,1:40])
performance(bayes_test_pred_shuffled, bayes_test[,41])
# Accuracy:  0.8741516 
# True positive rate:  0.865292 
# False positive rate:  0.1173921 
# prediction
# truth     0     1
# 0 10180  1354
# 1  1483  9526


#---------------------- Naive Bayes using e1071 without CV---------------------------#
# #Train model
# bayes_classifier = naiveBayes(bayes_train, bayes_train_labels)
# bayes_classifier
# 
# #Performance on train
# bayes_train_pred = predict(bayes_classifier, bayes_train)
# performance(bayes_train_pred, bayes_train_labels)
# 
# #Performance on test
# bayes_test_pred = predict(bayes_classifier, bayes_test)
# performance(bayes_test_pred, bayes_test_labels)


#---------------------- Naive Bayes using caret ---------------------------#
# # train a naive bayes model
# train_control <- trainControl(method="cv", number=3)
# model <- train(outcome.response~., 
#                data=bayes_train,
#                trControl=train_control,
#                method="nb")
# print(model)
# # make predictions
# predictions <- predict(model, bayes_test[,-41])
# confusionMatrix(predictions, bayes_test[,41])
