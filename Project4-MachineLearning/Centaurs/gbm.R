######################
###  Main Function ###
######################


#############################
####  Preprocessing data ####
#############################

# Read data and mark 999.0 as 0s

dfTrain <- read.csv('~/NYCDSA/Bootcamp/Kaggle_Project4/data/training.csv', header=T)
dfTest <- read.csv('~/NYCDSA/Bootcamp/Kaggle_Project4/data/test.csv', header=T)
dfTrain[dfTrain==-999.0] <- 0
dfTest[dfTest==-999.0] <- 0
testId = dfTest$EventId

# Convert PRI_jet_num to factor as instructed on the website.
dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)

weight <- dfTrain$Weight
labels <- dfTrain$Label

train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

#Creating 80/20 split within training data
set.seed(0)
train.index = sample(1:dim(train)[1], dim(train)[1]*.8)
val.index = -train.index

# #Removing azimuth angles Phi
# train = train[,-c(16,19,21,26,29)]
# test = test[,-c(16,19,21,26,29)]
# 
# #Logging numerical variables
# for (i in c(1:22,24:30)) {
#   train[,i] = log(train[,i])
#   test[,i] = log(test[,i])
# }
# 
# #Scaling numerical variables
# for (i in c(1:22,24:30)) {
#   train[,i] = scale(train[,i])
#   test[,i] = scale(test[,i])
# }

##########################
####  Build gbm model ####
##########################

library(caret)

####### Library used for parallel processing
library(doMC)
registerDoMC(cores = 4)

# Load our customized metric function.
source('helper.R')

###### Setup a 5 fold cross-validation and use AMS as the metric
ctrl = trainControl(method = "repeatedcv",number = 5, #Should be k=5 or 10
                    summaryFunction = AMS_summary)
      #=> Update k

###### Setup grid search parameters. 
gbmGrid <-  expand.grid(interaction.depth = c(4,6,8), n.trees =(2:8)*100,
                        shrinkage = c(0.01, 0.005, 0.001),
                        n.minobsinnode = c(100, 500, 2000))
      #=> Tweak grid

###### Train the gbm model
m_gbm = train(x=train[train.index,], y=labels[train.index], 
              method="gbm", weights=weight[train.index], 
              verbose=TRUE, trControl=ctrl, metric="AMS")
              # tuneGrid=gbmGrid)
      #=> Specify the tuneGrid

###### Find the best threshold for ROC and AUC based on train data (80% of dfTrain)
gbmTrainPred <- predict(m_gbm, newdata=train[train.index,], type="prob")
library(pROC)
labels <- ifelse(as.character(labels)=='s', 1, 0) #To convert to 0,1 for logistic regression
auc_train = roc(labels[train.index], gbmTrainPred[,2])
auc_train$auc
plot(auc_train, print.thres=TRUE)
threshold <- 0.002
      #=> Verify threshold

#########Variable importance
var.imp = varImp(m_gbm)

ggplot(var.imp$importance, aes(reorder(x = labels(var.imp$importance)[[1]], Overall))) + 
  geom_bar(aes(y = Overall, fill = labels(var.imp$importance)[[1]]), stat = 'identity') + 
  coord_flip()  + ylab("Variable importance") + xlab("Features") + 
  ggtitle("Variable importance for gbm") + guides(fill = F)
       
       , aes(reorder(x = Feature, Gain), y = Gain)) + 
  geom_bar(aes(fill = 1), stat = "identity") + coord_flip() +
  


###### Apply fitted model to val data (i.e. 20% of dfTrain)
gbmValPred <- predict(m_gbm, newdata=train[val.index,], type="prob")
auc_val = roc(labels[val.index], gbmValPred[,2])
auc_val$auc

###### Apply fitted model to test data
gbmTestPred <- predict(m_gbm, newdata=test, type="prob")
write.csv(as.data.frame(gbmTestPred), "Submissions/gbm_prob.csv")
predicted <- rep("b",550000)
predicted[gbmTestPred[,2]>=threshold] <- "s"
weightRank = rank(gbmTestPred[,2], ties.method= "random")
submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "gbm_submission.csv", row.names=FALSE)