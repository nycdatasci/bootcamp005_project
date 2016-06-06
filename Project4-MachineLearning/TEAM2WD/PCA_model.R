#############################
#### PCA Model           ####
#############################
setwd('~/Desktop/Higg')
source('data_prep.R')
source('evaluation.R')

#########################################################
sample <- sample(1:nrow(train), 1*nrow(train)/10)
sampleTrain <- train[sample,]
vars <- setdiff(colnames(sampleTrain), c('Label', 'EventId','Weight','Label', 'outcome'))
formula <- paste(Label,paste(vars,collapse=' + '),sep=' ~ ')

library(caret)
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
scaledTrain <- apply(subset(sampleTrain[,vars], select = -PRI_jet_num),2,rescale)

###########################################################
library(psych)
fa.parallel(scaledTrain, fa = "pc", n.iter = 100) 
abline(h = 1)

##the final principles 
pca<- principal(scaledTrain, nfactors = 15, rotate = "none") 
pca$loadings

############################################################
#PC variables construction on Sample Scaled Train
trainPCs <- predict(pca, scaledTrain)

# training logit regression on the PC variables.
tc <- trainControl(method = "repeatedcv", 5, 5, savePredictions=T,
                   classProbs = TRUE) #crossvalidation 5 times
fit <- train(x=trainPCs, y=sampleTrain$Label,
             method="glm", 
             family = binomial,
             trControl = tc,
             metric = 'ROC',
             control = list(maxit = 50))
fit$finalModel

#####on the 10k samples#######################################
SamplePred <- predict(fit, trainPCs, type = 'prob')

library(pROC)
#b =0 s=1
auc = roc(sampleTrain$outcome, (SamplePred[[2]]))
plot(auc, print.thres=TRUE)

######### training on the all the training dataset############
scaledTotalTrain <- apply(subset(train[vars], select = -PRI_jet_num),2,rescale)
scaledTotalTest <- apply(subset(test[vars], select = -PRI_jet_num),2,rescale)

AlltrainPCs <- predict(pca, scaledTotalTrain)
AlltestPCs <- predict(pca, scaledTotalTest)

Alltrainpred <- predict(fit, AlltrainPCs, type = 'prob')
Alltestpred <- predict(fit, AlltestPCs, type ='prob')

#model measurement###########################################
confusionMatrix(SamplePred, sampleTrain$Label) 
confusionMatrix(Alltrainpred, train$Label) 
confusionMatrix(Alltestpred, test$Label)


predictedSample=rep("b",nrow(SamplePred))
predictedSample[SamplePred[[2]]>0.294]="s"
confusionMatrix(predictedSample, sampleTrain$Label) 
AMS(sampleTrain$Label,predictedSample,weight=sampleTrain$Weight)

predictedAlltrain=rep("b",nrow(Alltrainpred))
predictedAlltrain[Alltrainpred[[2]]>0.294]="s"
confusionMatrix(predictedAlltrain, train$Label) 
AMS(train$Label,predictedAlltrain,weight=train$Weight)

predictedAlltest=rep("b",nrow(Alltestpred))
predictedAlltest[Alltestpred[[2]]>0.294]="s"
confusionMatrix(predictedAlltest, test$Label) 
AMS(test$Label,predictedAlltest,weight=test$Weight)

#########################
dfTest <- read.csv('test.csv', header=T)

momenta<-function(particlename,data) {
  coln=paste(particlename,'pt',sep="")
  pt=data[,coln]
  eta=data[,paste(particlename,'eta',sep="")]
  phi=data[,paste(particlename,'phi',sep="")]
  px=pt*cos(phi)
  py=pt*sin(phi)
  tantheta2=exp(-eta)
  pz=pt*(1+tantheta2*tantheta2)/2/tantheta2
  #  pz=pt*sinh(eta)
  ptot=sqrt(px*px+py*py+pz*pz)
  px[pt<0]=-999
  py[pt<0]=-999
  pz[pt<0]=-999
  ptot[pt<0]=-999  
  df=data.frame(px,py,pz,ptot)
  sx=paste(particlename,'px',sep="")
  sy=paste(particlename,'py',sep="")
  sz=paste(particlename,'pz',sep="")
  st=paste(particlename,'ptot',sep="")
  colnames(df)=c(sx,sy,sz,st)
  return (df)
}

lep_mom_test=momenta('PRI_lep_',dfTest)
jet_leading_mom_test =momenta('PRI_jet_leading_',dfTest)
jet_subleading_mom_test = momenta('PRI_jet_subleading_',dfTest)
tau_mom_test =momenta('PRI_tau_',dfTest)

dfTestNew <- data.frame(dfTest,lep_mom_test, jet_leading_mom_test, jet_subleading_mom_test,tau_mom_test)

##drop off phi terms
dfTestNew <- subset(dfTestNew, select = -c(PRI_lep_phi,PRI_jet_leading_phi,PRI_jet_subleading_phi,PRI_tau_phi))

# Convert PRI_jet_num to factor as instructed on the website.
dfTestNew$PRI_jet_num <- as.factor(dfTestNew$PRI_jet_num)

scaledSubTest <- apply(subset(dfTestNew[,vars], select = -PRI_jet_num),2,rescale)
AllSubPCs <- predict(pca, scaledSubTest)
AllSubPred <- predict(fit, AllSubPCs, type = 'prob')

predictedSub=rep("b",nrow(AllSubPred))
predictedSub[AllSubPred[[2]]>=0.294]="s"
weightRank = rank(AllSubPred[[2]], ties.method= "random")


submission = data.frame(EventId = dfTest$EventId, RankOrder = weightRank, Class = predictedSub)
write.csv(submission, "submission_pcalogit.csv", row.names=FALSE)

summary(submission)


