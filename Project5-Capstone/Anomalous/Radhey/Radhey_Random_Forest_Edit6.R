#Define accuracy function
accuracy = function(target, pred) {
  contingency = table(truth = target, prediction = pred)
  cat(sum(diag(contingency))/sum(contingency),'\n')
  return(contingency)
}


#Define performance function
performance = function(target, pred) {
  contingency = table(truth = target, prediction = pred)
  cat('Accuracy: ',sum(diag(contingency))/sum(contingency),'\n')
  cat('True positive: ',contingency[2,2]/sum(contingency[2,]),'\n')
  cat('False positive: ',contingency[1,2]/sum(contingency[1,]),'\n')
  return(contingency)
}
##############################################################
# Random Trees !!!
#############################################################


library(randomForest)

###########################################
####Random Forest ########
#############################################

set.seed(0)


rf.KDD = randomForest(outcome.response ~ ., data = KDD.train.random,ntree=100, importance = TRUE)
rf.KDD2 = randomForest(outcome.response ~ ., data = KDD.train.random,ntree=500, importance = TRUE)
importance(rf.KDD)
varImpPlot(rf.KDD)

importance(rf.KDD2)
varImpPlot(rf.KDD2)
new.KDD.train$outcome.response=as.factor(new.KDD.train$outcome.response)


set.seed(0)
oob.err = numeric(37)
temp=1

for (mtry in 4:40) {
  fit = randomForest(outcome.response ~ ., data = new.KDD.train, mtry=mtry)
  oob.err[temp] = fit$err.rate[500]
  temp=temp+1
  cat("We're performing iteration", mtry, "\n")
}
#Have to stop the simulation at 35 iteration so we get 35-4+1=32 ie mtry=32 upto 
plot(4:32, oob.err[4:32], pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Error Rate ",
     main = "Random Forest OOB Error Rates\nby # of Variables")



best_rf_123_var = randomForest(outcome.response ~ ., data = new.KDD.train, mtry=15)
oob.err_123= fit$err.rate[500]
best_rf_123_var$err.rate


importance_without_shuffle=importance(best_rf_123_var,sort=TRUE)
save(importance_without_shuffle,file="Train_Without_Shuffle_Importance.Rda")
write.csv(importance_without_shuffle,file ="Train_Without_Shuffle_Importance.csv")
#varImpPlot(best_rf_123_var)
varImpPlot(best_rf_123_var,sort=TRUE,main="Training Data (without Shuffle) Importance Plot")
plot(best_rf_123_var)


###########################################

#Finding the output !!!

#######################################
y_test_target_KDD_test=new.KDD.test[,122]

#Addding columns "num_outbound_cmds" because it was present in old testing model
new.KDD.test_123=new.KDD.test
new.KDD.test_123$num_outbound_cmds=0

y_test_predict_KDD_test = predict(best_rf_123_var, newdata =new.KDD.test_123[,-122])

y_test_target_KDD_test = new.KDD.test_123[,122]

cat("Random Forest KDD Test Performance")
performance(y_test_target_KDD_test,y_test_predict_KDD_test)

########################################
#Random Forest with new shuffle data
############################################


set.seed(0)
oob.err = numeric(13)
temp=1
new.KDD.train.shuffle$outcome.response=as.factor(new.KDD.train.shuffle$outcome.response)
for (mtry in 8:20) {
  fit = randomForest(outcome.response ~ ., data = new.KDD.train.shuffle, mtry=mtry)
  oob.err[temp] = fit$err.rate[500]
  temp=temp+1
  cat("We're performing iteration", mtry, "\n")
}
#Have to stop the simulation at 35 iteration so we get 35-4+1=32 ie mtry=32 upto 
plot(8:20, oob.err[1:13], pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Error Rate ",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#best_rf=randomForest(outcome.response ~ ., data = KDD.train.random, mtry=13)
varImp(fit)
varImpPlot(fit)

fit_best=randomForest(outcome.response ~ ., data = new.KDD.train.shuffle, mtry=18)

varImp(fit_best)
varImpPlot(fit_best,main = "KDD Shuffle Training Data Importance Plot")
yhat_best_shuffle = predict(fit_best, newdata =new.KDD.test.shuffle [,-122])


y.test.target.shuffle=new.KDD.test.shuffle[,122]
#accuracy(yhat_shuffle,yhat_best_shuffle)



cat("Shuffled Testing Error\n")
performance(y.test.target.shuffle,yhat_best_shuffle)


cat("Training Error")
yhat_best_shuffle_train = predict(fit_best, newdata =new.KDD.train.shuffle [,-122])
yhat_shuffle_train=new.KDD.train.shuffle[,122]
cat("Traing Error\n")
performance(yhat_shuffle_train,yhat_best_shuffle_train)




##########################################
#################XGBoost
#############################################
library(xgboost)
library(caret)
library(Matrix)

# Let us do the 80/20 split for training and test data
set.seed(1234)


#new.KDD.train.shuffle$outcome.response=as.numeric(new.KDD.train.shuffle$outcome.response)-1
KDD.train.ind = sample(1:nrow(new.KDD.train.shuffle), 8*nrow(new.KDD.train.shuffle)/10) #Training indices.
new.KDD.train_0.8 = new.KDD.train.shuffle[KDD.train.ind, ] # Train Dataset
new.KDD.train_0.2= new.KDD.train.shuffle[-KDD.train.ind, ]   # Test Dataset



train.y=new.KDD.train_0.8[,122]


train.model <- sparse.model.matrix(outcome.response ~ ., data = new.KDD.train_0.8)

dtrain <- xgb.DMatrix(data = train.model, label = train.y)


watchlist <- list(train=dtrain)

#Creating the params and training the model.
#Initial parameters are copied from a Santander Kaggle script, going to do a grid search eventually
#with cross validation to find the best parameters.
#We are using a binary:logistic objective since this is a binary classification problem.
#We are using gbtree because it generally provides better results than gblinear.
#Changed from 560 nrounds to 200 nrounds because the AUC seemed to level off at that stage and the 
#model may likely be overfitting (all preliminary).

#Set seed to 1234 for reproducibility (DONT FORGET TO DO THIS FOR YOUR OWN MODELS).
#Preparing for the training of the xgboost model.




set.seed(1234)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "error",
                eta                 = 0.02,
                max_depth           = 5,
                subsample           = 0.70,
                colsample_bytree    = 0.70
)

#history <- xgb.cv(params=param,data = dtrain,nround=50, nthread = 2, nfold = 5,prediction = TRUE, stratified=TRUE)

#print(history)


clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 200,
                    nfolds              = 5,
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)
#history <- xgb.cv(data = dtrain, nround=200, nthread = 2, nfold = 5, metrics=list("error","auc"),
#                  max.depth =5, eta = list(0.02,0.01,0.1,0.3), objective = "binary:logistic")

#Creating a sparse model matrix so that we can predict using the xgboost model we just trained.
y.target_0.2=new.KDD.train_0.2[,122]
test=new.KDD.train_0.2
test$outcome.response <- -1

test <- sparse.model.matrix(outcome.response ~ ., data = test)

preds <- predict(clf, test)

# plot(preds)
# library(pROC)
# auc = roc(y.target_0.2, preds)
# plot(auc, print.thres=TRUE)
y.predict_0.2=ifelse(preds > 0.5, 1,0)

#Prediction out of fold 20 percent of training data 
performance(y.target_0.2,y.predict_0.2)


# Let us predict full test data uisng this model
y.target=new.KDD.test.shuffle[,122]
test=new.KDD.test.s
test$outcome.response <- -1

test <- sparse.model.matrix(outcome.response ~ ., data = test)

preds.test <- predict(clf, test)

# auc = roc(y.target, preds)
# 
# plot(auc, print.thres=TRUE)
# plot(preds)
y.predict.unshuffle.test=ifelse(preds.test > 0.5, 1,0)
#Prediction out of fold 20 percent of training data 
#plot(preds)
performance(y.target,y.predict.unshuffle.test)


######################################################
##Adaboost
########################################################

library(ada)


# Let us do the 80/20 split for training and test data
set.seed(1234)


#new.KDD.train.shuffle$outcome.response=as.numeric(new.KDD.train.shuffle$outcome.response)-1
KDD.train.ind = sample(1:nrow(new.KDD.train.s), 8*nrow(new.KDD.train)/10) #Training indices.
new.KDD.train_0.8 = new.KDD.train[KDD.train.ind, ] # Train Dataset
new.KDD.train_0.2= new.KDD.train[-KDD.train.ind, ]   # Test Dataset


#new.KDD.train$outcome.response = as.factor(new.KDD.train$outcome.response)

ada_123=ada(outcome.response ~., data= new.KDD.train_0.8, verbose = TRUE, loss = "exponential", type = "real", iter = 50, nu = 1)

#Prediction on 20 percent training set
ada_train_0.2_predict=predict(ada_123, newdata=new.KDD.train_0.2)
#ada_train_0.2_predict_prob=predict(ada_123, newdata=new.KDD.train_0.2,type="prob")

y.target_0.2=new.KDD.train_0.2[,122]

#auc_ada = roc(y.target_0.2,ada_train_0.2_predict_prob[,2])

#plot(auc_ada, print.thres=TRUE)

#y.predict_0.2=ifelse(ada_train_0.2_predict_prob[,2] > 0.5, 1,0)
#Prediction out of fold 20 percent of training data 
#plot(preds)
cat("Ada 20 percent training")
performance(y.target_0.2,ada_train_0.2_predict)


#possible fit on whole traing data before making prediction on test 

#prediction on full test set 
ada_test_predict=predict(ada_123, newdata=new.KDD.test)
ada_test_target=new.KDD.test[,122]


cat("Ada full test")
performance(ada_test_target,ada_test_predict)



ada_123_nu_0.1=ada(outcome.response ~., data= new.KDD.train, verbose = TRUE, loss = "exponential", type = "real", iter = 50, nu = 0.1)

ada_test_predict_nu_0.1=predict(ada_123_nu_0.1, newdata=new.KDD.test)
ada_test_target_nu_0.1=new.KDD.test[,122]

performance(ada_test_target_nu_0.1,ada_test_predict_nu_0.1)

###################################
# Shuffle data
#########################################

set.seed(1234)


#new.KDD.train.shuffle$outcome.response=as.numeric(new.KDD.train.shuffle$outcome.response)-1
KDD.train.ind = sample(1:nrow(new.KDD.train.shuffle), 8*nrow(new.KDD.train.shuffle)/10) #Training indices.
new.KDD.train_0.8 = new.KDD.train.shuffle[KDD.train.ind, ] # Train Dataset
new.KDD.train_0.2= new.KDD.train.shuffle[-KDD.train.ind, ]   # Test Dataset


ada_123_shuffle=ada(outcome.response ~., data= new.KDD.train_0.8, verbose = TRUE, loss = "exponential", type = "real", iter = 50, nu = 1)
ada_123_shuffle_nu_0.1=ada(outcome.response ~., data= new.KDD.train_0.8, verbose = TRUE, loss = "exponential", type = "real", iter = 50, nu = 0.1)
#Prediction on 20 percent training set
ada_train_0.2_predict=predict(ada_123, newdata=new.KDD.train_0.2)

y.target_0.2=new.KDD.train_0.2[,122]

cat("Ada 20 percent training")
performance(y.target_0.2,ada_train_0.2_predict)


#prediction on full test set 
ada_test_predict=predict(ada_123, newdata=new.KDD.test.shuffle)
ada_test_target=new.KDD.test.shuffle[,122]

cat("Ada Shuffle full test")
performance(ada_test_target,ada_test_predict)





# ada_123_nu_0.1=ada(outcome.response ~., data= new.KDD.train, verbose = TRUE, loss = "exponential", type = "real", iter = 50, nu = 0.1)
# 
# ada_test_predict_nu_0.1=predict(ada_123_nu_0.1, newdata=new.KDD.test)
# ada_test_target_nu_0.1=new.KDD.test[,122]
# 
# performance(ada_test_target_nu_0.1,ada_test_predict_nu_0.1)


# ada_model = ada(Label ~ ., data = training, verbose = TRUE, loss = "exponential", type = "real", iter = 50, nu = 1)
# ada_model_loss_exponential = ada(Label ~ ., data = training, verbose = TRUE, loss = "exponential", type = "real", iter = 50, nu = 0.05)
# ada_model_loss_logistic = ada(Label ~ ., data = training, verbose = TRUE, loss = "logistic", type = "real", 
#                               iter = 50, nu = 0.05,
#                               bag.frac = 2)


##########################
#Neuralnet
###########################
normalize = function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
summary(new.KDD.train_0.8)
train_norm=new.KDD.train_0.8
train_norm=as.data.frame(lapply(train_norm, normalize))
summary(train_norm)

set.seed(0)

net_Formula <- as.formula(paste('outcome.response', paste(colnames(train_norm)[-122],collapse=' + '),sep=' ~ '))
net_Formula
net_neural=neuralnet::neuralnet(formula =  net_Formula,data = train_norm,hidden = c(20,14,10),act.fct = "logistic",err.fct = "ce",linear.output = FALSE)


model_results2 = neuralnet::compute(net_neural, new.KDD.test[, -122])
predicted_strength2 = model_results2$net.result



labels_net <-  new.KDD.test[, 122]
auc = roc(labels_net, predicted_strength2)


plot(auc, print.thres=TRUE)

y.predict_nn=ifelse(predicted_strength2 > 0.5, 1,0)
#Prediction out of fold 20 percent of training data 
#plot(preds)
performance(labels_net,y.predict_nn)