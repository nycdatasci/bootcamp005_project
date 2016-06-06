####################
#### GBM MODEL #####
####################

library(caret)
source('helper.R')
library(doMC)
registerDoMC(cores = 4)
library(pROC)

##### LOAD DATA #####
src.train = readRDS('MichaelsData.rds')
src.EventID = src.train$EventID
src.label = src.train$Label
src.weight = src.train$Weight
mod.train = src.train[,c(-1,-28, -29)]

#### SCALE AND SUBSET #####
scaled.train <- predict(preProcess(mod.train, method = c('center','scale')), mod.train)
set.seed(4)
dataSubset <- sample(x = nrow(scaled.train), size = nrow(scaled.train) * .8)
sub.train <- scaled.train[dataSubset,]
sub.label <- src.label[dataSubset]
sub.weight <- src.weight[dataSubset]

##### Correlation PLOTS #####
dfTrain <- read.csv('./data/training.csv', header=T)
dfTrain = dfTrain[, -c(1,32,33)]
library(corrplot)
traincorrplot = cor(dfTrain)
png(height=1200, width=1500, pointsize=20, file="CORR_NO_FE.png")
corrplot(traincorrplot,method = 'circle',
         title = 'Correlation before Feature Enginering',
         tl.col = 'black')
dev.off()

feat_eng = mod.train
feat_eng_plot = cor(feat_eng)
png(height=1200, width=1500, pointsize=20, file="CORR_FE.png")
corrplot(feat_eng_plot,method = 'circle',
         title = 'Correlation after Feature Enginering',
         tl.col = 'black')
dev.off()



#### Train model ####
gbm.ctrl <- trainControl(method = 'repeatedcv',
                         number = 3,
                         summaryFunction = AMS_summary)
gbm.grid <- expand.grid(n.trees = c(50,100,150),
                        interaction.depth = c(1,5,9),
                        shrinkage = c(0.01, 0.005, 0.001),
                        n.minobsinnode = c(10,100,1000))
gbm.model <- train(x = sub.train, y = sub.label, method = 'gbm', weights = sub.weight, 
                   verbose = TRUE, trControl = gbm.ctrl, tuneGrid = gbm.grid, metric = 'AMS')

gbm.model
plot(gbm.model)


gbm.ctrl2 <- trainControl(method = 'repeatedcv',
                          number = 3,
                          summaryFunction = AMS_summary)
gbm.grid2 <- expand.grid(n.trees = c(150, 300, 600),
                         interaction.depth = c(1,5,9),
                         shrinkage = c(0.05, 0.01, 0.005),
                         n.minobsinnode = c(10,100,1000))
gbm.model2 <- train(x = sub.train, y = sub.label, method = 'gbm', weights = sub.weight, 
                    verbose = TRUE, trControl = gbm.ctrl2, tuneGrid = gbm.grid2, metric = 'AMS')

gbm.model2
plot(gbm.model2)

# gbm.model_A=readRDS('gbm_model.rds')

#### PREDICT AGAINST 20% DATA #####
test.data <- mod.train[-dataSubset,]
test.label <- src.label[-dataSubset]
test.weight <- src.weight[-dataSubset]
test.pred <- predict(gbm.model_A,newdata = test.data, type='prob')
test.pred2 <- predict(gbm.model2,newdata = test.data, type='prob')


auc.labels <- ifelse(as.character(test.label)=='s',1,0)
auc <- roc(auc.labels,test.pred[,2])

plot(auc,print.thres=TRUE)

auc2 <- roc(auc.labels,test.pred2[,2])
plot(auc2,print.thres=TRUE)

# Get threshold from above plot for gbm model 1
Threshold = .001
predicted = rep('b',length(test.pred[,2]))
predicted[test.pred[,2]>= Threshold] = 's'
accuracy.tbl = table(truth = test.label, pred = predicted)
accuracy.tbl
accuracy = (accuracy.tbl['s','s'] + accuracy.tbl['b','b'])/length(test.pred[,2])
accuracy


# Get threshold from above plot for gbm model 2
Threshold2 = .001
predicted2 = rep('b',length(test.pred2[,2]))
predicted2[test.pred2[,2]>= Threshold2] = 's'
accuracy.tbl2 = table(truth = test.label, pred = predicted2)
accuracy.tbl2
accuracy2 = (accuracy.tbl2['s','s'] + accuracy.tbl2['b','b'])/length(test.pred2[,2])
accuracy2


##### submit to kaggle: model 1 #####
gbmTestPred <- predict(gbm.model_A, newdata=test2, type="prob")

predicted <- rep("b",550000)
predicted[gbmTestPred[,2]>=Threshold] <- "s"
weightRank = rank(gbmTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "gbm_submission.csv", row.names=FALSE)

gbm.model2 = readRDS('data/gbm_model2.rds')

##### submit to kaggle: model 2 #####
gbmTestPred2 <- predict(gbm.model2, newdata=test2, type="prob")

predicted2k <- rep("b",550000)
predicted2k[gbmTestPred2[,2]>=Threshold] <- "s"
weightRank2 = rank(gbmTestPred2[,2], ties.method= "random")

submission2 = data.frame(EventId = testId, RankOrder = weightRank2, Class = predicted2k)
write.csv(submission2, "gbm_submission2.csv", row.names=FALSE)




####################################
###### modifying test dataset ######
####################################
dfTest <- read.csv('data/test.csv', header=T)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)
test <- dfTest[,-1]
test[test==-999.0] <- NA


rotate_phi <- function(phi,ref_phi) {
  rot_phi = phi                                #create result object
  for (i in 1:length(phi)) {                   #loop through each row in phi
    if (is.na(ref_phi[i]) | is.na(phi[i]) )    #return original phi when angle is not meaningful
      rot_phi[i]=phi[i]
    else
      rot_phi[i]=phi[i]-ref_phi[i]             #calculate rotated phi
  }
  a = as.integer(rot_phi/pi)                   #find rot_phi outside of range +pi and -pi
  rot_phi = rot_phi - a*pi                     #adjust rotated phi to have values between +pi and -pi
  return(rot_phi)
} 

PRI_lep_phi_rot <- rotate_phi(test$PRI_lep_phi, test$PRI_tau_phi)
PRI_met_phi_rot <- rotate_phi(test$PRI_met_phi, test$PRI_tau_phi)
PRI_jet_leading_phi_rot <- rotate_phi(test$PRI_jet_leading_phi, test$PRI_tau_phi)
PRI_jet_subleadint_phi_rot <- rotate_phi(test$PRI_jet_subleading_phi, test$PRI_tau_phi)

test2 <- data.frame(test, PRI_lep_phi_rot, PRI_met_phi_rot, PRI_jet_leading_phi_rot, PRI_jet_subleadint_phi_rot)
test2 <- subset(test2, select = -c(PRI_tau_phi,PRI_lep_phi,PRI_met_phi,PRI_jet_leading_phi,PRI_jet_subleading_phi))

Mass_miss <- is.na(test2$DER_mass_MMC)
test2 <- data.frame(test2, Mass_miss)
test2$DER_mass_MMC[is.na(test2$DER_mass_MMC)] = mean(test2$DER_mass_MMC, na.rm=TRUE)

test2$Miss1 <- ifelse(test2$PRI_jet_num == 0 & test2$Mass_miss == 0, 1, 0)
test2$Miss2 <- ifelse(test2$PRI_jet_num == 0 & test2$Mass_miss == 1, 1, 0)
test2$Miss3 <- ifelse(test2$PRI_jet_num == 1 & test2$Mass_miss == 0, 1, 0)
test2$Miss4 <- ifelse(test2$PRI_jet_num == 1 & test2$Mass_miss == 1, 1, 0)
test2$Miss5 <- ifelse(test2$PRI_jet_num == 2 & test2$Mass_miss == 0, 1, 0)
test2$Miss6 <- ifelse(test2$PRI_jet_num == 2 & test2$Mass_miss == 1, 1, 0)
test2$Miss7 <- ifelse(test2$PRI_jet_num == 3 & test2$Mass_miss == 0, 1, 0)
test2$Miss8 <- ifelse(test2$PRI_jet_num == 3 & test2$Mass_miss == 1, 1, 0)

colnames(test2)[apply(is.na(test2), 2, any)]  

test2$DER_deltaeta_jet_jet [is.na(test2$DER_deltaeta_jet_jet)] = mean(test2$DER_deltaeta_jet_jet, na.rm=TRUE)
test2$DER_mass_jet_jet[is.na(test2$DER_mass_jet_jet)] = mean(test2$DER_mass_jet_jet, na.rm=TRUE)
test2$DER_prodeta_jet_jet[is.na(test2$DER_prodeta_jet_jet)] = mean(test2$DER_prodeta_jet_jet, na.rm=TRUE)
test2$DER_lep_eta_centrality[is.na(test2$DER_lep_eta_centrality)] = mean(test2$DER_lep_eta_centrality, na.rm=TRUE)
test2$PRI_jet_leading_pt[is.na(test2$PRI_jet_leading_pt)] = mean(test2$PRI_jet_leading_pt, na.rm=TRUE)
test2$PRI_jet_leading_eta[is.na(test2$PRI_jet_leading_eta)] = mean(test2$PRI_jet_leading_eta, na.rm=TRUE)
test2$PRI_jet_subleading_pt[is.na(test2$PRI_jet_subleading_pt)] = mean(test2$PRI_jet_subleading_pt, na.rm=TRUE)
test2$PRI_jet_subleading_eta[is.na(test2$PRI_jet_subleading_eta)] = mean(test2$PRI_jet_subleading_eta, na.rm=TRUE)
test2$PRI_jet_leading_phi_rot[is.na(test2$PRI_jet_leading_phi_rot)] = mean(test2$PRI_jet_leading_phi_rot, na.rm=TRUE)
test2$PRI_jet_subleadint_phi_rot[is.na(test2$PRI_jet_subleadint_phi_rot)] = mean(test2$PRI_jet_subleadint_phi_rot, na.rm=TRUE)



View(test2)


