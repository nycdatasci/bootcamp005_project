###############################################################################
##########################################################################
#####################################################################
#### Decision Tree
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tree)

setwd("/Users/AlexandKelly/Documents/DataScienceBootcamp/Class_Slides/kaggle_jumpstart/")

# Read data and mark 999.0 as NAs
dfTrain <- read.csv('./data/training.csv', header=T)
dfTest <- read.csv('./data/test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA
testId = dfTest$EventId 

# Convert PRI_jet_num to factor as instructed on the website.
dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)

# good practice to save as an independent file and if you mess up you can always get the original data
train <- dfTrain[, -c(1,32,33)]# have to subtract the ID and the last two columns, the weight and label
test <- dfTest[,-1]
train2 <- dfTrain[, -c(1,32)]
#train2$Label=ifelse(train2$Label=="b", 0, 1)
#z=sample_n(train2, size=10000, replace=FALSE)
labels <- dfTrain$Label

######## Not bad only 7.3% errors made on one tree

#tree.higgs # by printout the object itself you get the details

train.cv = sample(1:nrow(train2), 8*nrow(train2)/10) #Training indices
test.cv = train2[-train.cv, ] #Test dataset. will be the values not in my train set

tree.cv1 = tree(Label ~ ., data = train2, subset = train.cv, split="gini")# normally include , split="gini"

tree.pred = predict(tree.cv1, test.cv, type = "class")

table(tree.pred, test.cv$Label)

(30870+5051)/(30870+5051+12044+2035)

#[1] 0.71842 accuracy

#> summary(tree.cv1)

#Classification tree:
#        tree(formula = Label ~ ., data = train2, subset = train.cv, split = "gini")
#Number of terminal nodes:  3398 
#Residual mean deviance:  0.2936 = 15000 / 51100 
#Misclassification error rate: 0.0729 = 3973 / 54500


tree_test_pred<- predict(tree.cv1, test, type = "class")

#> (30964+5015)/(30964+5015+2027+11994)

#[1] 0.71958 % of accuracy


tree.pred_train = predict(tree.cv1, train, type = "vector")#
tree.pred_test = predict(tree.cv1, test, type = "vector")#

dim(tree.pred_test)
dim(tree.pred_train)


library(pROC)
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, tree.pred_train[,2])###### 
plot(auc, print.thres=TRUE)

#Area under the curve of 0.7075


######## From the graph, we can tell the best threshold is 0.6254
threshold <- 0.529

predicted <- rep("b",550000)
predicted[tree.pred_test[,2]>=threshold] <- "s"
weightRank = rank(tree.pred_test[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "tree_Final_submission.csv", row.names=FALSE)

#Would place 1643 on the compettition score 1.132 under Zeyu's benchmark




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Pruning the tree


cv1.train = cv.tree(tree.cv1, FUN = prune.misclass)

cv1.train

############ GRAPH 1 for presentation
par(mfrow = c(1, 2))
plot(cv1.train$size, cv1.train$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "Misclassified Observations")
plot(cv1.train$k, cv1.train$dev, type  = "b",
     xlab = "Alpha", ylab = "Misclassified Observations")

## looks like we should prune to 5 terminal nodes

par(mfrow = c(1, 1))
prune.train = prune.misclass(tree.cv1, best = 5) # prune to the best, that had 5 nodes
plot(prune.train)
text(prune.train, pretty = 0)
################################



tree.pred_train = predict(prune.train, train, type = "vector")#
tree.pred_test = predict(prune.train, test, type = "vector")#

dim(tree.pred_test)
dim(tree.pred_train)


library(pROC)
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, tree.pred_train[,2])###### 
plot(auc, print.thres=TRUE)

#Area under the curve of 0.6516


######## From the graph, we can tell the best threshold is 0.6254
threshold <- 0.567

predicted <- rep("b",550000)
predicted[tree.pred_test[,2]>=threshold] <- "s"
weightRank = rank(tree.pred_test[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "treepruned_Final_submission.csv", row.names=FALSE)

#Would place 1630 on the compettition score 1.24 under Zeyu's benchmark pruned at 10
#Would place 1662 on the compettition score 1.0378 under Zeyu's benchmark pruned at 5



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# RANDOM FOREST with only DER columns with adjusted columns now adding dummy variables
setwd("/Users/AlexandKelly/Documents/DataScienceBootcamp/Class_Slides/kaggle_jumpstart/")

library(dplyr)
library(randomForest)


# Read data and mark 999.0 as NAs
dfTrain <- read.csv('./data/training.csv', header=T)
dfTest <- read.csv('./data/test.csv', header=T)
testId = dfTest$EventId 

# Convert PRI_jet_num to factor as instructed on the website.
# dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
# dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)


# good practice to save as an independent file and if you mess up you can always get the original data
train <- dfTrain[, -c(1,32,33)]# have to subtract the ID and the last two columns, the weight and label
test <- dfTest[,-1]
labels <- dfTrain$Label


train2 <- dfTrain[, -c(1,32)]
train5 <- train2[,-c(7, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)]
train5 <- mutate(train5, NA_Check_DER_mass_transverse_met_lep=ifelse(DER_mass_transverse_met_lep >990, 0, 1))# error rate 16.52
train5 <- mutate(train5, NA_check_DER_deltaeta_jet_jet=ifelse(DER_deltaeta_jet_jet >990, 0, 1))# error rate  16.54              

rf.higgs = randomForest(Label~., data=train5, importance = TRUE, na.action=na.omit, mtry=4, ntree=500)
rf.higgs


#Call:
#        randomForest(formula = Label ~ ., data = train5, importance = TRUE,      mtry = 4, ntree = 500, na.action = na.omit) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 4

#OOB estimate of  error rate: 16.54%
#Confusion matrix:
#        b     s class.error
#b 147679 16654   0.1013430
#s  24707 60960   0.2884074

plot(rf.higgs)

#Varying the number of variables used at each step of the random forest procedure.

#oob.err = numeric(30)
#for (mtry in 1:30) {
#        fit = randomForest(Label~., data=train.cv, importance = TRUE, na.action=na.omit)# 
#        oob.err[mtry] = fit$mse[500]#we want the last tree
#        cat("We're performing iteration", mtry, "\n")# tells us where we are in the process to know if our compyters gave up
#}


#plot(1:30, oob.err, pch = 16, type = "b",# type b says plot a line and a dot
#     xlab = "Variables Considered at Each Split",
#     ylab = "OOB Mean Squared Error",
#     main = "Random Forest OOB Error Rates\nby # of Variables")

importance(rf.higgs)
varImpPlot(rf.higgs) # will use this in the presentation

train1=train
test1=test 
train1 <- mutate(train1, NA_Check_DER_mass_transverse_met_lep=ifelse(DER_mass_transverse_met_lep >990, 0, 1))# error rate 16.52
train1 <- mutate(train1, NA_check_DER_deltaeta_jet_jet=ifelse(DER_deltaeta_jet_jet >990, 0, 1))# error rate  16.54              
test1 <- mutate(test1, NA_Check_DER_mass_transverse_met_lep=ifelse(DER_mass_transverse_met_lep >990, 0, 1))# error rate 16.52
test1 <- mutate(test1, NA_check_DER_deltaeta_jet_jet=ifelse(DER_deltaeta_jet_jet >990, 0, 1))# error rate  16.54              

rf.pred_train = predict(rf.higgs, train1, type = "prob")#
rf.pred_test = predict(rf.higgs, test1, type = "prob")#

dim(rf.pred_test)#check point
dim(rf.pred_train)#check point


labels <- dfTrain$Label
library(pROC)
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, rf.pred_train[,2])
plot(auc, print.thres=TRUE)

#Area under the curve of 1


######## From the graph, we can tell the best threshold is 0.494
threshold <- 0.494

predicted <- rep("b",550000)
predicted[rf.pred_test[,2]>=threshold] <- "s"
weightRank = rank(rf.pred_test[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "rf_Dummy_submission.csv", row.names=FALSE)


## rank 1216 AMS score 2.795



