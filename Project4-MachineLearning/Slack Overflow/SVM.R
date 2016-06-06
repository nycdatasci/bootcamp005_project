library(kernlab)
library(dplyr)
library(corrplot)
library(e1071)
library(pROC)

df = read.csv('training.csv')

# exclude first and last column of dataframe
df[,2:32]

# correlation plot of remaining columns
corr_all = cor(df[,2:32])
corrplot(corr_all, method = 'circle')
summary(df['DER_deltaeta_jet_jet'])
summary(df['PRI_jet_subleading_eta'])
# looking at correlation among primitive variables
# PRI_met_sumet & PRI_let_all_pt
# PRI_jet_leading_pt & PRI_let_leading_eta
# PRI_jet_leading_pt & PRI_let_leading_phi
# PRI_jet_leading_eta & PRI_let_leading_phi
# PRI_jet_subleading_pt & PRI_jet_leading_eta
# PRI_jet_subleading_pt & PRI_jet_subleading_phi
# PRI_jet_subleading_eta & PRI_jet_subleading_phi

# Histogram
hist(df[,'Weight'])

# Boxplot
boxplot(df[,'Weight'])

summary(df)

###########
### SVM ###
###########

# how to deal with missingness
# iid
# scale
# factor variable

##########################
### SVM: linear kernel ###
##########################

## data as is ##
dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)
testId = dfTest$EventId
train.index = sample(1:250000, 250000*.01)
test.index = -train.index
labels <- dfTrain$Label
# tune on 1% of train data
train <- dfTrain[, c(-1,-32)]
str(train)
set.seed(0)
cv.svc.linear = tune(svm,
                          Label ~ .,
                          data = train[train.index,],
                          kernel = "linear",
                          ranges = list(cost = 10^(seq(-5, -.5, length = 5))),
                          tunecontrol = tune.control(cross = 5))
best.linear.model = cv.svc.linear$best.model
summary(best.linear.model)
attributes(best.linear.model)
ypred = predict(best.linear.model, train[test.index,])
table("Predicted Values" = ypred, "True Values" = train[test.index,'Label'])
(134194+50542)/(134194+50542+34224+28540)
# accuracy of 0.7464 at length of 5 and cross of 5
# train on 1% of the data
svm.best.linear = svm(Label ~ .,
                           data = train[train.index,],
                           kernel = "linear",
                           cost = best.linear.model$cost,
                      probability = TRUE)
# predict on full train
ypred = predict(svm.best.linear, train, probability = T)
table("Predicted Values" = ypred, "True Values" = train[,'Label'])
(135498+51083)/(135498+51083+34584+28835)
# accuracy of 0.7463 at length of 5 and cross of 5
labels <- ifelse(labels=='s', 1, 0)
ypred_prob = attr(ypred, 'probabilities')
auc = roc(labels, ypred_prob[,2])
plot(auc, print.thres=TRUE)
# threshold 0.338
threshold = 0.338
ypred = predict(svm.best.linear, dfTest, probability = T)
ypred_prob = attr(ypred, 'probabilities')
predicted <- rep("b",550000)
predicted[ypred_prob[,2]>=threshold] <- "s"
weightRank = rank(ypred_prob[,2], ties.method= "random")
submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "svm_linear_submission.csv", row.names=FALSE)
# score 1.90257

##########################
### SVM: radial kernel ###
##########################
dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)
testId = dfTest$EventId
train.index = sample(1:250000, 250000*.01)
test.index = -train.index
labels <- dfTrain$Label

## data as is ##
# tuning on 1% of train data
cv.svm.radial = tune(svm,
                     Label ~ .,
                     data = train[train.index, ],
                     kernel = "radial",
                     ranges = list(cost = 10^(seq(-1, 1.5, length = 5)),
                                   gamma = 10^(seq(-2, 1, length = 5))),
                     tunecontrol = tune.control(cross = 5))
best.radial.model = cv.svm.radial$best.model
summary(best.radial.model)
ypred = predict(best.radial.model, train[test.index, ])
table("Predicted Values" = ypred, "True Values" = train[test.index, "Label"])
(137736+58819)/(137736+58819+24998+25947)
# accuracy of 0.7942 at length of 5 and cross of 5
# train on 1% of the data
svm.best.radial = svm(Label ~ .,
                      data = train[train.index,],
                      kernel = "radial",
                      cost = best.radial.model$cost,
                      gamma = best.radial.model$gamma,
                      probability = TRUE)
# predict on full train
ypred = predict(svm.best.radial, train, probability = T)
table("Predicted Values" = ypred, "True Values" = train[,'Label'])
(142471+56484)/(142471+56484+29183+21862)
# accuracy of 0.7958 at length of 5 and cross of 5
labels <- dfTrain$Label
labels <- ifelse(labels=='s', 1, 0)
ypred_prob = attr(ypred, 'probabilities')
auc = roc(labels, ypred_prob[,2])
plot(auc, print.thres=TRUE)
# threshold 0.351
threshold = 0.351
ypred = predict(svm.best.radial, dfTest, probability = T)
ypred_prob = attr(ypred, 'probabilities')
predicted <- rep("b",550000)
predicted[ypred_prob[,2]>=threshold] <- "s"
weightRank = rank(ypred_prob[,2], ties.method= "random")
submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "svm_radial_submission.csv", row.names=FALSE)
# score 2.12172

## data scaled ##
dfTrain_scale = as.data.frame(scale(dfTrain[,-c(1,32,33)]))
summary(dfTrain_scale)
dfTrain = cbind(dfTrain_scale,dfTrain[,33])
colnames(dfTrain)[31] = 'Label'
set.seed(0)
train.index = sample(1:250000, 250000*.01)
head(train.index)
test.index = -train.index
train <- dfTrain[, c(-1,-32)]
# tuning on 1% of train data
cv.svm.radial = tune(svm,
                     Label ~ .,
                     data = train[train.index, ],
                     kernel = "radial",
                     ranges = list(cost = 10^(seq(-1, 1.5, length = 5)),
                                   gamma = 10^(seq(-2, 1, length = 5))),
                     tunecontrol = tune.control(cross = 5))
best.radial.model = cv.svm.radial$best.model
summary(best.radial.model)
ypred = predict(best.radial.model, train[test.index, ])
table("Predicted Values" = ypred, "True Values" = train[test.index, "Label"])
(137734+58854)/(137734+58854+25937+24975)
# accuracy of 0.7942 at length of 5 and cross of 5
# train on 1% of the data
svm.best.radial = svm(Label ~ .,
                      data = train[train.index,],
                      kernel = "radial",
                      cost = best.radial.model$cost,
                      gamma = best.radial.model$gamma,
                      probability = TRUE)
# predict on train
ypred = predict(svm.best.radial, train, probability = T)
table("Predicted Values" = ypred, "True Values" = train[,'Label'])
(142323+56794)/(142323+56794+28873+22010)
# accuracy of 0.7965 at length of 5 and cross of 5
labels <- dfTrain$Label
labels <- ifelse(labels=='s', 1, 0)
ypred_prob = attr(ypred, 'probabilities')
auc = roc(labels, ypred_prob[,2])
plot(auc, print.thres=TRUE)
# threshold 0.366
threshold = 0.366
ypred = predict(svm.best.radial, dfTest, probability = T)
summary(ypred)
ypred_prob = attr(ypred, 'probabilities')
predicted <- rep("b",550000)
predicted[ypred_prob[,2]>=threshold] <- "s"
weightRank = rank(ypred_prob[,2], ties.method= "random")
submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
summary(submission)
write.csv(submission, "svm_radial_scaled_submission.csv", row.names=FALSE)
# score 2.16148

## data scaled and select columns ##
# DER_mass_MMC, DER_mass_transverse_met_lep, DER_mass_vis, DER_deltar_tau_lep,
# PRI_tau_pt, PRI_met, DER_pt_h, DER_sum_pt,DER_pt_ratio_lep_tau, DER_pt_tot,
# DER_mass_jet_jet, DER_met_phi_centrality, DER_deltaeta_jet_jet,
# DER_lep_eta_centrality
dfTrain <- read.csv('training.csv', header=T)
summary(dfTrain)
dfTest <- read.csv('test.csv', header=T)
selected_col = c('DER_mass_MMC', 'DER_mass_transverse_met_lep', 'DER_mass_vis',
                 'DER_deltar_tau_lep', 'PRI_tau_pt', 'PRI_met', 'DER_pt_h',
                 'DER_sum_pt', 'DER_pt_ratio_lep_tau', 'DER_pt_tot',
                 'DER_mass_jet_jet', 'DER_met_phi_centrality', 'DER_deltaeta_jet_jet',
                 'DER_lep_eta_centrality', 'Label')
dfTrain = dfTrain[selected_col]
set.seed(0)
train.index = sample(1:250000, 250000*.01)
test.index = -train.index
summary(dfTrain[train.index,])
(876)/(1624+876)
# tuning on 1% of train data
set.seed(0)
cv.svm.radial = tune(svm,
                     Label ~ .,
                     data = dfTrain[train.index, ],
                     kernel = "radial",
                     ranges = list(cost = 10^(seq(-1, 1.5, length = 5)),
                                   gamma = 10^(seq(-2, 1, length = 5))),
                     tunecontrol = tune.control(cross = 5))
best.radial.model = cv.svm.radial$best.model
summary(best.radial.model)
ypred = predict(best.radial.model, dfTrain[test.index, ])
table("Predicted Values" = ypred, "True Values" = dfTrain[test.index, "Label"])
(139834+59061)/(139834+59061+25730+22875)
# accuracy of 0.8036 at length of 5 and cross of 5
# train on 1% of the data
set.seed(0)
svm.best.radial = svm(Label ~ .,
                      data = dfTrain[train.index,],
                      kernel = "radial",
                      cost = best.radial.model$cost,
                      gamma = best.radial.model$gamma,
                      probability = TRUE)
# predict on train data
summary(svm.best.radial)
ypred = predict(svm.best.radial, dfTrain, probability = T)
summary(ypred)
# 171747 / (171747+78253)
table("Predicted Values" = ypred, "True Values" = dfTrain[,'Label'])
(143744+57664)/(143744+57664+28003+20589)
# accuracy of 0.8056 at length of 5 and cross of 5
labels <- dfTrain$Label
labels <- ifelse(labels=='s', 1, 0)
attr(ypred,'probabilities')
ypred_prob = attr(ypred, 'probabilities')
auc = roc(labels, ypred_prob[,2]) # 4 minutes
plot(auc, print.thres=TRUE)
# threshold 0.345
threshold = 0.345
selected_col_test = c('DER_mass_MMC', 'DER_mass_transverse_met_lep', 'DER_mass_vis',
                 'DER_deltar_tau_lep', 'PRI_tau_pt', 'PRI_met', 'DER_pt_h',
                 'DER_sum_pt', 'DER_pt_ratio_lep_tau', 'DER_pt_tot',
                 'DER_mass_jet_jet', 'DER_met_phi_centrality', 'DER_deltaeta_jet_jet',
                 'DER_lep_eta_centrality')
ypred = predict(svm.best.radial, dfTest[selected_col_test], probability = T) # 2 minutes
ypred
summary(ypred)
ypred_prob = attr(ypred, 'probabilities')
summary(ypred_prob)
predicted <- rep("b",550000)
predicted[ypred_prob[,2]>=threshold] <- "s"
weightRank = rank(ypred_prob[,2], ties.method= "random")
submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
summary(submission)
write.csv(submission, "svm_radial_submission_reduced.csv", row.names=FALSE)
# score 2.30382, rank 1392-1393