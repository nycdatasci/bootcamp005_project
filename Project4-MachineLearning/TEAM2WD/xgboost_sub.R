require(ggplot2)
library(xgboost)
require(methods)
library(dplyr)
library(tidyr)
library(Ckmeans.1d.dp)
library(caret)
dfTrain <- read.csv('C:/wanglf2016/kaggle/ATLAS/data/training.csv', header=T) #,nrows = 10000)
dfTest <- read.csv('C:/wanglf2016/kaggle/ATLAS/data/test.csv', header=T)#,nrows = 1000)
testsize = 550000

testId = dfTest$EventId##adding interpretive variables
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
lep_mom=momenta('PRI_lep_',dfTrain)
jet_leading_mom =momenta('PRI_jet_leading_',dfTrain)
jet_subleading_mom = momenta('PRI_jet_subleading_',dfTrain)
tau_mom =momenta('PRI_tau_',dfTrain)

lep_mom_test = momenta('PRI_lep_',dfTest)
jet_leading_mom_test = momenta('PRI_jet_leading_',dfTest)
jet_subleading_mom_test = momenta('PRI_jet_subleading_',dfTest)
tau_mom_test = momenta('PRI_tau_',dfTest)


dfTrainNew <- data.frame(dfTrain,lep_mom,      jet_leading_mom,       jet_subleading_mom,tau_mom)
dfTestNew <- data.frame(dfTest,  lep_mom_test, jet_leading_mom_test, jet_subleading_mom_test,tau_mom_test) 

dfTest <- dfTest[,-1]  # Convert PRI_jet_num to factor as instructed on the website.
dfTrainNew$outcome <- ifelse(dfTrain$Label == 's', 1, 0)

train <-subset(dfTrainNew, select = -c(EventId, outcome,Weight,Label))
test <- subset(dfTestNew, select = -c(EventId)) 
trainOutcome <-dfTrainNew$outcome   
trainWeight <- dfTrainNew$Weight

label<-trainOutcome
data<-as.matrix(train)
weight<-trainWeight*testsize/length(label)

sumwpos <- sum(weight * (label==1.0))
sumwneg <- sum(weight * (label==0.0))
print(paste("weight statistics: wpos=", sumwpos, "wneg=", sumwneg, "ratio=", sumwneg / sumwpos))
  test<-as.matrix(test)
  testxgmat <- xgb.DMatrix(test,  missing = -999.0)
xgmat <- xgb.DMatrix(data, label = label, weight = weight, missing = -999.0)
param <- list("objective" = "binary:logitraw",
              "scale_pos_weight" = sumwneg / sumwpos,
              "bst:eta" = 0.051,
              "bst:max_depth" = 6,
              "eval_metric" = "auc",
              "eval_metric" = "ams@0.15",
              "silent" = 1,
              "nthread" = 1)
watchlist <- list("train" = xgmat)
nround = 120
print ("loading data end, start to boost trees")
bst = xgb.train(param, xgmat, nround, watchlist,prediction = TRUE);

xgb.save(bst, "xgb0.model")
print ('finish training')


pred_test = predict( bst,testxgmat )
rorder <- rank(pred_test, ties.method="first")

threshold <- 0.15

ntop <- length(rorder) - as.integer(threshold*length(rorder))
plabel <- ifelse(rorder > ntop, "s", "b")
outdata <- list("EventId" = testId ,
                "RankOrder" = rorder,
                "Class" = plabel)
write.csv(outdata, file = "xgb_submission.csv", quote=FALSE, row.names=FALSE)


