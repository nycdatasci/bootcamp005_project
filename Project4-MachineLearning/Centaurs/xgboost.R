library(xgboost)
library(methods)
library(pROC)
library(caret)
library(xgboost)
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(dummies)
library(doMC)
registerDoMC(cores = 4)

#Read in the data
#higgs.___.full is raw data
higgs.train.full = read.csv('./data/training.csv', header=T)
higgs.test.full = read.csv('./data/test.csv', header=T)
higgs.testId = higgs.test.full$EventId

#############################################
########### DATA MUNGING ###################
##########################################
#higgs.__ will be what is analyzed
higgs.train = higgs.train.full
higgs.test = higgs.test.full

#Tranform PRI_jet_num into a factor, as instructed
higgs.train$PRI_jet_num <- as.factor(higgs.train$PRI_jet_num)
higgs.test$PRI_jet_num <- as.factor(higgs.test$PRI_jet_num)

#higgs.weight is the weight of the training data
higgs.weight <- higgs.train$Weight

#We make labels of the outcomes.
#The make.names is because the "train" function requires the factors to have names that are valid
# variable names (unlike 0,1 or True, False)
higgs.labels <- make.names(as.factor(as.numeric(higgs.train$Label == 's')))

#Scale the weight according to the length of the data.
scaled.weight = higgs.weight * nrow(higgs.test)/length(higgs.labels)

#Remove the ID, Weight, and Outcome columns
higgs.train = higgs.train[, -c(1,32,33)]
higgs.test <- higgs.test[,-1]

#Create a dummy variable for the "PRI_jet_num" variable
higgs.train.dummy = dummy.data.frame(higgs.train, names = "PRI_jet_num")
higgs.test.dummy = dummy.data.frame(higgs.test, names = "PRI_jet_num")

#############################################
##############################################
# Grid for the parameter search
#The guidlines for how to tune parameters are commented below and are taken from
# Owen Zheng http://www.slideshare.net/OwenZhang2/tips-for-data-science-competitions
xgb_grid_1 = expand.grid(
  eta = c(.5, 1, 1.5),                #[2-10]/num trees
  max_depth = c(4, 6, 8),             #Start with 6
  nrounds = 100,                      #Fix at 100
  gamma = 0,                          #Usually ok to leave at 0
  colsample_bytree = c(.3, .5, .7),   #.3 - .5
  min_child_weight = 1                #start with 1/sqrt(eventrate)
)

# Tuning control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                         # save losses across all models
  classProbs = TRUE,                            # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

# Train the model on each set of parameters in the grid and evaluate using cross-validation
xgb_train_1 = train(
  x = higgs.train.dummy,
  y = higgs.labels,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree",
  na.action = na.pass,
  missing = NA,
  metric = "ROC",
  weights = scaled.weight
)

###############################
#Best parameters of first grid search:
#eta = .5
#max_depth = 4
#nrounds = 100
#gamma = 0
#colsample_bytree = .7
#min_child_weight = 1
###############################

#Second grid search
xgb_grid_2 = expand.grid(
  eta = c(.4, .5, .6),                #[2-10]/num trees
  max_depth = c(3, 4, 5),             #Start with 6
  nrounds = 100,                      #Fix at 100
  gamma = 0,                          #Usually ok to leave at 0
  colsample_bytree = c(.6, .7, .8, .9),   #.3 - .5
  min_child_weight = 1                #start with 1/sqrt(eventrate)
)

xgb_train_2 = train(
  x = higgs.train.dummy,
  y = higgs.labels,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_2,
  method = "xgbTree",
  na.action = na.pass,
  missing = NA,
  metric = "ROC",
  weights = scaled.weight
)

############################################
#Best parameters of second grid search:
#eta = .4
#gamma = 0
#max_depth = 5
#colsample_bytree = .8
#min_child_weight = 1
###########################################

#Third grid search
xgb_grid_3 = expand.grid(
  eta = c(.2, .3, .4, .45),                #[2-10]/num trees
  max_depth = 5,             #Start with 6
  nrounds = 100,                      #Fix at 100
  gamma = 0,                          #Usually ok to leave at 0
  colsample_bytree = c(.75, .8, .85),   #.3 - .5
  min_child_weight = 1                #start with 1/sqrt(eventrate)
)

xgb_train_3 = train(
  x = higgs.train.dummy,
  y = higgs.labels,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_3,
  method = "xgbTree",
  na.action = na.pass,
  missing = NA,
  metric = "ROC",
  weights = scaled.weight
)

###########################################
#Best parameters for thrird grid search:
#eta = .2
#gamma = 0
#max_depth = 5
#colsample_bytree = .85
#min_child_weight = 1
########################################

#Fourth grid search
xgb_grid_4 = expand.grid(
  eta = c(.01, .05, .1, .15, .2, .25),                #[2-10]/num trees
  max_depth = 5,             #Start with 6
  nrounds = 200,                      #Fix at 100
  gamma = 0,                          #Usually ok to leave at 0
  colsample_bytree = .85,   #.3 - .5
  min_child_weight = 1                #start with 1/sqrt(eventrate)
)

xgb_train_4 = train(
  x = higgs.train.dummy,
  y = higgs.labels,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_4,
  method = "xgbTree",
  na.action = na.pass,
  missing = NA,
  metric = "ROC",
  weights = scaled.weight
)

#########################################
#Best parameters for fourth grid search:
#nrounds - 200
#eta = .2
#gamma = 0
#max_depth = 5
#colsample_bytree = .85
#min_child_weight = 1
########################################

#Five grid search
xgb_grid_5 = expand.grid(
  eta = .2,                #[2-10]/num trees
  max_depth = 5,             #Start with 6
  nrounds = c(100, 200, 400, 500, 1000),                      #Fix at 100
  gamma = 0,                          #Usually ok to leave at 0
  colsample_bytree = .85,   #.3 - .5
  min_child_weight = 1                #start with 1/sqrt(eventrate)
)

xgb_train_5 = train(
  x = higgs.train.dummy,
  y = higgs.labels,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_5,
  method = "xgbTree",
  na.action = na.pass,
  missing = NA,
  metric = "ROC",
  weights = scaled.weight
)

#nrounds = 200 is still the best


# scatter plot of the AUC against max_depth and eta
# ggplot(xgb_train_5$results, aes(x = as.factor(nrounds), y = colsample_bytree, size = ROC, color = ROC)) + 
#   geom_point() + 
#   theme_bw() + 
  scale_size_continuous(guide = "none")
#######################################
#Predictions
######################################
bst = xgb_train_5$finalModel
importance_matrix = xgb.importance(feature_names = colnames(higgs.train.dummy), model = bst)

ggplot(importance_matrix, aes(reorder(x = Feature, Gain), y = Gain)) + 
  geom_bar(aes(fill = 1), stat = "identity") + coord_flip() +
  ylab("Variable importance") + xlab("Features") + ggtitle("Variable importance for xgboost") +
  guides(fill = F)

#Predicting training data
xgmat.train <- xgb.DMatrix(as.matrix(higgs.train.dummy), 
                           label = as.numeric(higgs.labels == "X0"),
                           weight = scaled.weight)
xgboostTrainPred <- predict(bst, newdata = xgmat.train)
labels <- ifelse(as.character(higgs.labels)=="X1", 1, 0)

auc = roc(labels, xgboostTrainPred)
plot(auc, print.thres=TRUE)
######## From the graph, we can tell the best threshold is 0.002
threshold <- 0.662
err <- mean(as.numeric(xgboostTrainPred >= threshold) != (higgs.labels == "X0"))

#########################
#Predicting the test data
xgmat.test <- xgb.DMatrix(as.matrix(higgs.test.dummy))
xgboostTestPred <- predict(bst, newdata=xgmat.test)
  
predicted <- rep("s",550000)
predicted[xgboostTestPred>=threshold] <- "b"
weightRank = rank(xgboostTestPred, ties.method= "random")

write.csv(as.data.frame(xgboostTestPred), "Submissions/xgboost_prob.csv")
write.csv(as.data.frame(higgs.testId), "Submissions/EventID.csv")
  
submission = data.frame(EventId = higgs.testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "xgboost_submission.csv", row.names=FALSE)
