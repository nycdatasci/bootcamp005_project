#############################
#### PCA Model           ####
#############################

# Read data and mark 999.0 as NAs
dfTrain <- read.csv('./data/training.csv', header=T)
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

#the train and test is for the use going forward
train <-subset(df.train, select = -c(EventId, Weight,outcome))#20K record in the train
test <- subset(df.test, select = -c(EventId, Weight, outcome)) #5K record in the test

###################
#checking the missing values
navar <- apply(train,2,function(col)sum(is.na(col))/length(col))
colswithallmiss <-names(navar[navar>0])

####imputation test###########################
library(Hmisc)
train[,1] <- impute(train[,1] ,mean)
train[,c(22,23,32:35)] <- impute(train[,c(22,23,32:35)], 0)
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}
train[,c(5:7,13,24:25, 36:39)] <- apply(train[,c(5:7,13,24:25, 36:39)],2, random.imp)
########################################
test[,1] <- impute(test[,1] ,mean)
test[,c(22,23,32:35)] <- impute(test[,c(22,23,32:35)], 0)
test[,c(5:7,13,24:25, 36:39)] <- apply(test[,c(5:7,13,24:25, 36:39)],2, random.imp)

##########################################################
sample <- sample(1:nrow(train), 1*nrow(train)/10)
sampleTrain <- train[sample,]
Label <- 'Label'
vars <- setdiff(colnames(sampleTrain), 'Label')
formula <- paste(Label,paste(vars,collapse=' + '),sep=' ~ ')

#trainX <- train[, names(train) != "Label"]
#testX <- test[, names(train) != "Label"]
## Methods are "BoxCox", "YeoJohnson", center", "scale",
## "range", "knnImpute", "bagImpute", "pca", "ica",
## "spatialSign", "medianImpute", "expoTrans"
preProcValues <- preProcess(sampleTrain[,vars], method = c("center", "scale"))
scaledTrain <- predict(preProcValues, sampleTrain[,vars])
scaledTrain <- cbind(scaledTrain, sampleTrain$Label)
names(scaledTrain)[names(scaledTrain)=="sampleTrain$Label"] <- 'Label'

###########################################################
first.prc<-prcomp(subset(scaledTrain,select = -PRI_jet_num))
screeplot(first.prc, main="Scree Plot", xlab="Components")
screeplot(first.prc, main="Scree Plot", type="line" )

pca<- principal(subset(scaledTrain,select = -PRI_jet_num), nfactors = 5, rotate = "none") 
pca$loadings

#PC variables 
trainPCs <- predict(pca, subset(scaledTrain,select = -PRI_jet_num))

# training only on the PC variables.
modelFit <- train(train$Label ~ ., method="glm", data=trainPCs)

#for prediction
testPCs <- predict(pca, subset(scaledTest,select = -PRI_jet_num))
testpred <- predict(modelFit, testPCs)

#model measurement
confusionMatrix(predict(modelFit, trainPCs), train$Label) 
confusionMatrix(testpred, test$Label)


###########################################################
##random sample 20K rows. 

modelCsmall <- ksvm(as.formula(formula),data=scaledTrain,type='C-svc',C=0.1,
                    kernel='rbfdot',
                    prob.model=TRUE)

#Support Vector Machine object of class "ksvm" 
#SV type: C-svc  (classification) 
#parameter : cost C = 0.1 

#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  0.0171308601181031 

#Number of Support Vectors : 11762 

#Objective Function Value : -1072.822 
#Training error : 0.2097 
#Probability model included. 


library(caret)
library(pROC)
library(kernlab)
cvCtrl <- trainControl(method = "repeatedcv", repeats = 1,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)

set.seed(1)
svmTune <- train(x = subset(scaledTrain,select = -c(PRI_jet_num, Label)),
                 y = scaledTrain$Label,
                 method = "svmRadial",
                 # The default grid of cost parameters go from 2^-2,
                 # 0.5 to 1, 
                 # We'll fit 9 values in that sequence via the tuneLength
                 # argument.
                 tuneLength = 10,
                 metric = "ROC",   
                 verbose = FALSE,
                 trControl = cvCtrl)

svmTune


###############################################################
## Slide 111: SVM Example

svmTune$finalModel


###############################################################
## Slide 112: SVM ROC Profile

plot(svmTune, metric = "ROC", scales = list(x = list(log = 2))))


###############################################################
## Slide 113: Test Set Results

svmPred <- predict(svmTune, testing[, names(testing) != "Class"])
confusionMatrix(svmPred, testing$Class)

