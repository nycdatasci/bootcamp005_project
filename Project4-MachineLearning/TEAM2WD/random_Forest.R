#############################
####  Preprocessing data ####
#############################
library(doMC)
registerDoMC(cores = 4)

source('data_prepare.R')
train <- sample(1:nrow(dfTrainNew), 8*nrow(dfTrainNew)/10)
df.train <- dfTrainNew[train, ]
df.test <- dfTrainNew[-train, ]
testWeight <- df.test$Weight
trainWeight <- df.train$Weight

trainRF <-subset(df.train, select = -EventId)#20K record in the train
testRF <- subset(df.test, select = -EventId) #5K record in the test

sample <- sample(1:nrow(trainRF), 1*nrow(trainRF)/10)
sampleTrain <- trainRF[sample,]
vars <- setdiff(colnames(sampleTrain), c('Label','Weight'))
formula <- paste('Label',paste(vars,collapse=' + '),sep=' ~ ')

library(caret)

#########################
#   random forest       #
#########################

library(randomForest)
set.seed(5123512)
fmodel <- randomForest(as.formula(formula), data =sampleTrain, 
                       na.action = na.roughfix, 
                       nodesize = 7, 
                       importance = T)

varImp<- importance(fmodel)
varImp
varImpPlot(fmodel, type = 1)
ord <- order(varImp[,3], decreasing=TRUE) # sort, descending order
vord <- varImp[ord, 3]
topvars <- names(vord[1:30]) ##this is to pick the top 30 variables to rerun the tree.


###################################################################
##caret##############################################################
####### The AMS function defined according to the evaluation page on the website
AMS <- function(real,pred,weight)
{
  pred_s_ind = which(pred=="s")                          # Index of s in prediction
  real_s_ind = which(real=="s")                          # Index of s in actual
  real_b_ind = which(real=="b")                          # Index of b in actual
  s = sum(weight[intersect(pred_s_ind,real_s_ind)])      # True positive rate
  b = sum(weight[intersect(pred_s_ind,real_b_ind)])      # False positive rate
  b_tau = 10                                             # Regulator weight
  ans = sqrt(2*((s+b+b_tau)*log(1+s/(b+b_tau))-s))
  return(ans)
}


####### Using AMS function defined above as a metrics function for caret
####### Check the details here: http://topepo.github.io/caret/training.html#metrics
AMS_summary <- function(data, lev = NULL, model = NULL){
  out = (AMS(data$obs, data$pred, data$weights))
  names(out) <- "AMS"
  return(out)
}


ctrl = trainControl(method = "repeatedcv",number = 2, repeats = 2, classProbs = TRUE,
                    summaryFunction = AMS_summary)

rfGrid <-  expand.grid(mtry = seq(5,10, by=1))

m_rf <- train(as.formula(formula),
              data= sampleTrain,
              method="rf", 
              weights= sampleTrain$Weight, 
              tuneGrid = rfGrid,
              verbose=TRUE, 
              trControl=ctrl, 
              metric="AMS")

m_rf$finalModel

#save(m_rf, file ="m_rf.RData")

#load('m_rf.RData')

rfTestPred <- predict(m_rf, newdata=subset(testRF, select =-Label), type="prob", na.action = na.roughfix)
rfSamplePred <- predict(m_rf, newdata=subset(sampleTrain, select =-Label), type="prob", na.action = na.roughfix)

library(pROC)
labels <- ifelse(testRF$Label=='s', 1, 0)
auc = roc(labels, rfTestPred[,2])
plot(auc, print.thres=TRUE)

predicted <- rep("b",nrow(rfTestPred))
a <- quantile(rfTestPred[,2], 0.84)
a
predicted[rfTestPred[,2] > 0.61] <- 's'

confusionMatrix(as.factor(predicted), testRF$Label)
AMS(testRF$Label, predicted, testRF$Weight)

###
rfSubPred <- predict(m_rf, newdata=subset(dfTest, select = -EventId), type="prob", na.action= na.roughfix)

predictedSub<- rep("b",nrow(rfSubPred))
predictedSub[rfSubPred[,2] > 0.61] <- 's'
weightRank = rank(rfSubPred[,2], ties.method= "random")

submission = data.frame(EventId = dfTest$EventId, RankOrder = weightRank, Class = predictedSub)
write.csv(submission, "submission_rf.csv", row.names=FALSE)

summary(submission)

