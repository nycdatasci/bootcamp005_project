#EDA
#logistic
#Ridge
#K-means clustering
setwd("~/Documents/Network_Fraud/")
FieldNames <-read.csv("Field Names.csv", header = FALSE,
                      stringsAsFactors = FALSE)

KDD.test <-read.csv("KDDTest+.csv", header = FALSE,
                    stringsAsFactors = FALSE)

KDD.train <-read.csv("KDDTrain+.csv", header = FALSE,
                     stringsAsFactors = FALSE)

column.names <- FieldNames[,1] #41 columns 
colnames(KDD.test) <- column.names # rename columns
colnames(KDD.train)<- column.names
colnames(KDD.train)[42] <- 'outcome'
KDD.train$outcome <- as.factor(KDD.train$outcome)

KDD.train$outcome.response <- ifelse(KDD.train$outcome == 'normal',0,1)

View(KDD.train) #44 cols  0.465% are malicious 
View(KDD.test)
#Dealing with 3 Categorical Variables, 0/1, expanding ncols, replace into new.KDD.train
library(nnet)
service_<-as.data.frame(class.ind(KDD.train$service))
protocol_type_<-as.data.frame(class.ind(KDD.train$protocol_type))
flag_<-as.data.frame(class.ind(KDD.train$flag))
new_ <- cbind(service_, protocol_type_, flag_) #84
new.KDD.train <-cbind(duration=KDD.train$duration, new_, KDD.train[,5:41], outcome.response=KDD.train[,44])
dim(new.KDD.train) #[1] 125973    123
View(new.KDD.train)


# Above Transformation on Test Data Set













##############################################################
# Random Trees !!!
#############################################################

##################################
#####Bagging & Random Forests#####
##################################
library(randomForest)
random_col=c(1:41,44)

###########################################
####Random Forest ########
#############################################

set.seed(0)
KDD.train.random$service = as.numeric(KDD.train.random$service)

rf.KDD = randomForest(outcome.response ~ ., data = KDD.train.random,ntree=100, importance = TRUE)
rf.KDD2 = randomForest(outcome.response ~ ., data = KDD.train.random,ntree=500, importance = TRUE)
importance(rf.KDD)
varImpPlot(rf.KDD)

importance(rf.KDD2)
varImpPlot(rf.KDD2)


set.seed(0)
oob.err = numeric(18)

temp=1

for (mtry in 4:21) {
  fit = randomForest(outcome.response ~ ., data = KDD.train.random, mtry=mtry)
  oob.err[temp] = fit$err.rate[500]
  temp=temp+1
  cat("We're performing iteration", mtry, "\n")
}

plot(4:21, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Error Rate ",
     main = "Random Forest OOB Error Rates\nby # of Variables")

best_rf=randomForest(outcome.response ~ ., data = KDD.train.random, mtry=13)

importance(best_rf)

varImpPlot(best_rf)

######################################################
# Tranforming the Test data as same steps as train data
############################################################


colnames(KDD.test)[42] <- 'outcome'
KDD.test$outcome <- as.factor(KDD.test$outcome)

KDD.test$outcome.response <- ifelse(KDD.test$outcome == 'normal',0,1)


###########################################

#Finding the output !!!

#######################################
y_test=KDD.test[,44]
yhat = predict(best_rf, newdata = KDD.test.random)





# c_full=c(1:41)
# fact_vec=c(2,3,4,7,12,14,15,21,22)
# fact_vec_1=c(2,3,4)
# cont_vec=c(1,5,6,8,9,10,11,13,16,17,18,19,20,23:41)
# cont_vec_1=c(1,5,6,8)
# lsm=FactoMineR::PCA(X=KDD.train.random_PCA, scale.unit = TRUE, ncp = 5,  
#     quanti.sup = cont_vec, quali.sup = fact_vec, row.w = NULL, 
#     col.w = NULL, graph = FALSE)

##########################################
#################XGBoost
#############################################
library(xgboost)
library(caret)







