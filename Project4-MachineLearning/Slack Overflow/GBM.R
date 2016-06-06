# Based off Titanic
library(dplyr)
library(caret)
library(doMC)
registerDoMC(cores = 4)


data <- data.frame(read.csv('training.csv', header=TRUE))
tester <- data.frame(read.csv('test.csv', header=TRUE))

HiggsDF <- data
testId = tester$EventId

# Creates dummy variables PRI_jet_num.0 to PRI_jet_num.3
HiggsDF <- HiggsDF[,2:31] # removes the EventId and Weight
HiggsDF$PRI_jet_num <- as.factor(HiggsDF$PRI_jet_num)
HiggsDummy <- dummyVars("~.", data=HiggsDF, fullRank=FALSE)
HiggsDF <- bind_cols(as.data.frame(predict(HiggsDummy, HiggsDF)), data[33])

tester$PRI_jet_num <- as.factor(tester$PRI_jet_num)
testerDummy <- dummyVars("~.", data=tester, fullRank=FALSE)
tester <- as.data.frame(predict(testerDummy, tester))

# Proportion of background (65.7%) with Higgs events (34.2%)
prop.table(table(HiggsDF$Label))


# Creates variables for future use
outcomeName <- 'Label'
predictorsNames <- names(HiggsDF)[names(HiggsDF) != outcomeName]


# Training/Test split
set.seed(0)
splitIndex <- createDataPartition(HiggsDF[[outcomeName]], p = .8, list = FALSE, times = 1)
trainDF <- HiggsDF[ splitIndex,]
testDF  <- HiggsDF[-splitIndex,]
#length(trainDF)/nrow(x) # proportion of training to original
#length(testDF)/nrow(x) # proportion of test to original


# GBM Modeling
# We’re going to cross-validate the data 3 times, therefore training it 
# 3 times on different portions of the data before settling on the best
# tuning parameters (for gbm: trees, shrinkage, and interaction depth).
objControl <- trainControl(method='cv', number=3, returnResamp='none', 
                           summaryFunction = twoClassSummary, classProbs = TRUE)
objModel <- train(as.data.frame(trainDF[,predictorsNames]), trainDF[[outcomeName]],
                  method='gbm', 
                  trControl=objControl,
                  metric = "ROC",
                  preProc = c("center", "scale"))
summary(objModel)

saveRDS(objModel, "gbm_model.rds")
#                                       var               rel.inf
# DER_mass_MMC                               DER_mass_MMC 41.869512657
# DER_mass_transverse_met_lep DER_mass_transverse_met_lep 24.107792831
# DER_mass_vis                               DER_mass_vis 11.067010426
# PRI_tau_pt                                   PRI_tau_pt  5.374397314
# DER_deltaeta_jet_jet               DER_deltaeta_jet_jet  3.923478456
# DER_met_phi_centrality           DER_met_phi_centrality  2.554897820
# DER_deltar_tau_lep                   DER_deltar_tau_lep  1.776484726
# DER_lep_eta_centrality           DER_lep_eta_centrality  1.476714710
# DER_pt_ratio_lep_tau               DER_pt_ratio_lep_tau  1.405932969
# PRI_jet_leading_eta                 PRI_jet_leading_eta  1.073320736
# DER_mass_jet_jet                       DER_mass_jet_jet  1.068201244
# PRI_lep_eta                                 PRI_lep_eta  0.863316106
# DER_pt_h                                       DER_pt_h  0.699408214
# PRI_met                                         PRI_met  0.677514160
# PRI_jet_num.3                             PRI_jet_num.3  0.521637585
# PRI_tau_eta                                 PRI_tau_eta  0.399286524
# DER_sum_pt                                   DER_sum_pt  0.370701121
# DER_pt_tot                                   DER_pt_tot  0.322405806
# PRI_jet_leading_pt                   PRI_jet_leading_pt  0.212640613
# DER_prodeta_jet_jet                 DER_prodeta_jet_jet  0.082787472
# PRI_jet_num.1                             PRI_jet_num.1  0.066795648
# PRI_jet_leading_phi                 PRI_jet_leading_phi  0.026335769
# PRI_jet_subleading_pt             PRI_jet_subleading_pt  0.026224741
# PRI_jet_all_pt                           PRI_jet_all_pt  0.014299056
# PRI_jet_subleading_eta           PRI_jet_subleading_eta  0.010631025
# PRI_met_sumet                             PRI_met_sumet  0.008272271
# PRI_tau_phi                                 PRI_tau_phi  0.000000000
# PRI_lep_pt                                   PRI_lep_pt  0.000000000
# PRI_lep_phi                                 PRI_lep_phi  0.000000000
# PRI_met_phi                                 PRI_met_phi  0.000000000
# PRI_jet_num.0                             PRI_jet_num.0  0.000000000
# PRI_jet_num.2                             PRI_jet_num.2  0.000000000
# PRI_jet_subleading_phi           PRI_jet_subleading_phi  0.000000000
print(objModel)
# interaction.depth  n.trees  ROC        Sens       Spec     
# 1                   50      0.8572145  0.8994272  0.6301106
# 1                  100      0.8657168  0.8905505  0.6594683
# 1                  150      0.8716144  0.8890824  0.6701053
# 2                   50      0.8703186  0.8972670  0.6491522
# 2                  100      0.8823223  0.8948481  0.6717542
# 2                  150      0.8879239  0.8946199  0.6842152
# 3                   50      0.8807978  0.8956620  0.6637289
# 3                  100      0.8913322  0.8959891  0.6855429
# 3                  150      0.8960815  0.8966508  0.6971284

# Evaluate GBM Modeling
predictions <- predict(object=objModel, testDF[,predictorsNames], type='raw')
#head(predictions)
print(postResample(pred=predictions, obs=testDF[[outcomeName]]))
# Accuracy     Kappa 
# 0.8276566    0.6071090 


# AUC - labels on trained data set
# predict - real labels vs predicted labels

# Probabilities
library(pROC)
predictions <- predict(object=objModel, testDF[,predictorsNames], type='prob')
labelx <- ifelse(testDF[[outcomeName]]=="s",1,0)
auc <- roc(labelx, predictions[,2])
plot(auc, print.thres=TRUE)
# Area under the curve: 0.896


threshold <- 0.341

predictions <- predict(object=objModel, tester, type='prob')

predicted <- rep("b",550000)
predicted[predictions[,2]>=threshold] <- "s"
weightRank <- rank(predictions[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "gbm_submission.csv", row.names=FALSE)








# --------------------------------- GOOD UP TO HERE ---------------------------------


tempOutcome <- HiggsDF$Label
HiggsDF$Label  <- tempOutcome

# GLMNET Modeling

set.seed(0)
splitIndex <- createDataPartition(HiggsDF[[outcomeName]], p = .01, list = FALSE, times = 1)
trainDF <- HiggsDF[ splitIndex,]
testDF  <- HiggsDF[-splitIndex,]

objControl <- trainControl(method='cv', number=3, returnResamp='none')
objModel <- train(as.data.frame(trainDF[,predictorsNames]), as.factor(trainDF[[outcomeName]]), method='glmnet',  metric = "RMSE", trControl=objControl)
