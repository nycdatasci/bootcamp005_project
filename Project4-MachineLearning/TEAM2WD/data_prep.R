
setwd('~/Desktop/Higg')
# Read data and mark 999.0 as NAs
dfTrain <- read.csv('training.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
##adding interpretive variables
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

dfTrainNew <- data.frame(dfTrain,lep_mom, jet_leading_mom, jet_subleading_mom,tau_mom)

##drop off phi terms
dfTrainNew <- subset(dfTrainNew, select = -c(PRI_lep_phi,PRI_jet_leading_phi,PRI_jet_subleading_phi,PRI_tau_phi))

# Convert PRI_jet_num to factor as instructed on the website.
dfTrainNew$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)

#assigning label columns to 1 or 0, if it ==s, it's 1.
dfTrainNew$outcome <- ifelse(dfTrain$Label == 's', 1, 0)

##in the training dataset, split for 80 /20 test
train <- sample(1:nrow(dfTrainNew), 8*nrow(dfTrainNew)/10)
df.train <- dfTrainNew[train, ]
df.test <- dfTrainNew[-train, ]
testWeight <- df.test$Weight
trainWeight <- df.train$Weight

train <- df.train
test <- df.test

###################
#checking the missing values
navar <- apply(df.train,2,function(col)sum(is.na(col))/length(col))
colswithallmiss <-names(navar[navar>0])

####imputation test###########################
library(Hmisc)
train[,2] <- impute(train[,2] ,mean)
train[,c(23,24,34:37)] <- impute(train[,c(23,24,34:37)], 0)
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}
train[,c(6:8,14,25:26, 38:41)] <- apply(train[,c(6:8,14,25:26, 38:41)],2, random.imp)
########################################
test[,2] <- impute(test[,2] ,mean)
test[,c(23,24,34:37)] <- impute(test[,c(23,24,34:37)], 0)
test[,c(6:8,14,25:26, 38:41)] <- apply(test[,c(6:8,14,25:26, 38:41)],2, random.imp)
