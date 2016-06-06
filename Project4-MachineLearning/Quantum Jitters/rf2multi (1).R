setwd("C:/Users/User/Documents/RClass1/kaggle_jumpstart1/kaggle_jumpstart")
dfTrain <- read.csv('./training.csv', header=T)
dfTest <- read.csv('./test.csv', header=T)

dfTrain <- training
dfTest <- test

#http://www.jmlr.org/proceedings/papers/v42/diaz14.pdf
#Explains how to remove the phi features and to use bagging rather than just random forests

View(dfTrain)

# Convert PRI_jet_num to factor as instructed on the website.
dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)
str(dfTrain)

weight <- dfTrain$Weight
labels <- dfTrain$Label
testId = dfTrain$EventId

train <- dfTrain[, -c(1,32,33)]
test1 <- dfTest[,-1]

View(train)

#rotate phi columns, replace them with rotated phis, and remove them
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

PRI_lep_phi_rot <- rotate_phi(train$PRI_lep_phi, train$PRI_tau_phi)
PRI_met_phi_rot <- rotate_phi(train$PRI_met_phi, train$PRI_tau_phi)
PRI_jet_leading_phi_rot <- rotate_phi(train$PRI_jet_leading_phi, train$PRI_tau_phi)
PRI_jet_subleadint_phi_rot <- rotate_phi(train$PRI_jet_subleading_phi, train$PRI_tau_phi)


train2 <- data.frame(train, PRI_lep_phi_rot, PRI_met_phi_rot, PRI_jet_leading_phi_rot, PRI_jet_subleadint_phi_rot)
train2 <- subset(train2, select = -c(PRI_tau_phi,PRI_lep_phi,PRI_met_phi,PRI_jet_leading_phi,PRI_jet_subleading_phi))

train2[train2==-999.0] <- NA
train2[train2==-999.0] <- NA


View(train2)

#implement missingness solution
#first create boolean(TRUE=1) for missing mass, append it to data frame, mean impute the mass column
Mass_miss <- is.na(train2$DER_mass_MMC)
train2 <- data.frame(train2, Mass_miss)
train2$DER_mass_MMC[is.na(train2$DER_mass_MMC)] = mean(train2$DER_mass_MMC, na.rm=TRUE)
View(train2)

#add missing variation from columns that will be mean imputed
train2$Miss1 <- ifelse(train2$PRI_jet_num == 0 & train2$Mass_miss == 0, 1, 0)
train2$Miss2 <- ifelse(train2$PRI_jet_num == 0 & train2$Mass_miss == 1, 1, 0)
train2$Miss3 <- ifelse(train2$PRI_jet_num == 1 & train2$Mass_miss == 0, 1, 0)
train2$Miss4 <- ifelse(train2$PRI_jet_num == 1 & train2$Mass_miss == 1, 1, 0)
train2$Miss5 <- ifelse(train2$PRI_jet_num == 2 & train2$Mass_miss == 0, 1, 0)
train2$Miss6 <- ifelse(train2$PRI_jet_num == 2 & train2$Mass_miss == 1, 1, 0)
train2$Miss7 <- ifelse(train2$PRI_jet_num == 3 & train2$Mass_miss == 0, 1, 0)
train2$Miss8 <- ifelse(train2$PRI_jet_num == 3 & train2$Mass_miss == 1, 1, 0)

View(train2)

#mean impute within columns across the dataset
colnames(train2)[apply(is.na(train2), 2, any)]      


train2$DER_deltaeta_jet_jet [is.na(train2$DER_deltaeta_jet_jet)] = mean(train2$DER_deltaeta_jet_jet, na.rm=TRUE)
train2$DER_mass_jet_jet[is.na(train2$DER_mass_jet_jet)] = mean(train2$DER_mass_jet_jet, na.rm=TRUE)
train2$DER_prodeta_jet_jet[is.na(train2$DER_prodeta_jet_jet)] = mean(train2$DER_prodeta_jet_jet, na.rm=TRUE)
train2$DER_lep_eta_centrality[is.na(train2$DER_lep_eta_centrality)] = mean(train2$DER_lep_eta_centrality, na.rm=TRUE)
train2$PRI_jet_leading_pt[is.na(train2$PRI_jet_leading_pt)] = mean(train2$PRI_jet_leading_pt, na.rm=TRUE)
train2$PRI_jet_leading_eta[is.na(train2$PRI_jet_leading_eta)] = mean(train2$PRI_jet_leading_eta, na.rm=TRUE)
train2$PRI_jet_subleading_pt[is.na(train2$PRI_jet_subleading_pt)] = mean(train2$PRI_jet_subleading_pt, na.rm=TRUE)
train2$PRI_jet_subleading_eta[is.na(train2$PRI_jet_subleading_eta)] = mean(train2$PRI_jet_subleading_eta, na.rm=TRUE)

View(train2)

library(caret)

#Parallel processing
library(doSNOW)
cl <- makeCluster(4, outfile="") #number of jobs
registerDoSNOW(cl)

# Load our customized metric function.
source('helper.R')


###### Setup a 5 fold cross-validation and use AMS as the metric
###### AMS_summary function defined in helper.R
###### Check details here: http://topepo.github.io/caret/training.html#custom
ctrl = trainControl(method = "oob", summaryFunction = AMS_summary)
#number = 2-5, p = .80, not needed with bagging/RF

###### The only thing you need to change is the name of method.
###### Check all the available algorithms by typing names(getModelInfo())
###### Check avaliable tuning parameters here: http://topepo.github.io/caret/modelList.html
rfGrid <- expand.grid(mtry = c(7,12,38)) #muliple tries
rfGrid <- expand.grid(mtry = c(38)) #just bagging

m_rf = train(x=train2, y=labels, 
             method="rf", weights=weight, 
             verbose=TRUE, tuneGrid=rfGrid, trControl=ctrl, metric="AMS")


###### You can think of this model as a logistic regression. For a logistic regression, we need to find the best threshold for ROC and AUC.
###### Check the definition of ROC and AUC here: 
###### http://thestatsgeek.com/2014/05/05/area-under-the-roc-curve-assessing-discrimination-in-logistic-regression/
rfTrainPred2 <- predict(m_rf, newdata=train2, type="prob")

library(pROC)
labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, rfTrainPred2[,2])
plot(auc, print.thres=TRUE)

######## From the graph, we can tell the best threshold is 0.002
threshold <- 0.501 #multiple mtries
threshold <- 0.495 #bagging model

rfTestPred2 <- predict(m_rf, newdata=test2, type="prob")

predicted <- rep("b",550000)
predicted[rfTestPred2[,2]>=threshold] <- "s"
weightRank = rank(rfTestPred2[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "MWrf2_submission.csv", row.names=FALSE)
