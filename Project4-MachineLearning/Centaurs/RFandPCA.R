######################
###  Main Function ###
######################
library(psych)
library(corrplot)
#############################
####  Preprocessing data ####
#############################

# Read data and keep -999.0 as is
dfTrain <- read.csv('./data/training.csv', header=T)
dfTest <- read.csv('./data/test.csv', header=T)

##################
######Trees#######
##################

library(caret)
library(doMC)
registerDoMC(cores = 4)

weight <- dfTrain$Weight
labels <- dfTrain$Label
train99 <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

#train99 is just a reference to the fact that the train data still has -999 in the dataframe
#train8099 is randomly selected 80 % of the train99 data
set.seed(0)
trainindex = sample(1:nrow(train99), 8*nrow(train99)/10)
train8099 = train99[trainindex,]
train2099 = train99[-trainindex,]
labels80 = labels[trainindex]
weight80 =  weight[trainindex]
labels20 = labels[-trainindex]

#summaryfunction 
ctrl = trainControl(method = "repeatedcv",number = 5, #shoudl be 5 or 10 but for exmaple used 2
                    summaryFunction = twoClassSummary, classProbs = TRUE, allowParallel = TRUE)

#initial grid looks at a wide range of mtrys
rfGrid <-  expand.grid(mtry = c(1,2,3,6,9))

#in order to speed up the process, only 20% of training data was used for the initial grid
set.seed(0)
smalltrainindex = sample(1:nrow(train8099), nrow(train8099)/4)
smalltrain = train8099[smalltrainindex,]
smallweight = weight80[smalltrainindex]
smalllabels = labels80[smalltrainindex]
m_rf80small = train(x=smalltrain, y=smalllabels, 
                 method="rf", weights=smallweight, tuneGrid = rfGrid, 
                 verbose=TRUE, trControl=ctrl, metric="ROC")

# Random Forest 
# 
# 50000 samples
# 31 predictor
# 2 classes: 'b', 's' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 40000, 40001, 40000, 39999, 40000 
# Resampling results across tuning parameters:
#     
#     mtry  ROC           Sens          Spec        
# 1     0.8614907396  0.9470045843  0.5211682213
# 2     0.8910540385  0.9148908481  0.6646251531
# 3     0.8993618613  0.9081029207  0.6917419846
# 6     0.9018030519  0.9029586854  0.7071955971
# 9     0.9013201478  0.9019237149  0.7098195719
# 
# ROC was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 6. 

#initial RF estimates that mtry = 6 is the best, used 6 as the middle point in the next RF train
#This time 80% of the training data was used. 
rfGrid <-  expand.grid(mtry = c(4,5,6,7,8))
set.seed(0)
m_rf8099 = train(x=train8099, y=labels80, 
               method="rf", weights=weight80, tuneGrid = rfGrid, 
               verbose=TRUE, trControl=ctrl, metric="ROC")
# 
# Random Forest 
# 
# 200000 samples
# 31 predictor
# 2 classes: 'b', 's' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 160001, 159999, 160000, 160000, 160000 
# Resampling results across tuning parameters:
#     
#     mtry  ROC           Sens          Spec        
# 4     0.9049659094  0.9055427871  0.7098096429
# 5     0.9056386714  0.9049568513  0.7122736939
# 6     0.9055370199  0.9046600818  0.7136296426
# 7     0.9055341083  0.9044165752  0.7137171125
# 8     0.9053367964  0.9040893626  0.7141836911
# 
# ROC was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 5. 

#Final RF model with mtry = 5 and training with the 80% dataset
rfGrid <-  expand.grid(mtry = 5)
set.seed(0)
m_rf = train(x=train8099, y=labels80, 
                    method="rf", weights=weight80, tuneGrid = rfGrid, 
                    verbose=TRUE, trControl=ctrl, metric="ROC")

# Random Forest 
# 
# 200000 samples
# 31 predictor
# 2 classes: 'b', 's' 
# 
# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 1 times) 
# Summary of sample sizes: 160001, 159999, 160000, 160000, 160000 
# Resampling results:
#     
#     ROC        Sens       Spec     
# 0.9055061  0.9053906  0.7131193
# 
# Tuning parameter 'mtry' was held constant at a value of 5

#predict the 20% of the training data that was not used to fit the final RF model
library(pROC)
rfTrainPred <- predict(m_rf, newdata=train2099, type="prob")

#calculating the ROC AUC
bilabels <- ifelse(labels20=='s', 1, 0)
auc = roc(bilabels, rfTrainPred[,2])
auc$auc
plot(auc, print.thres=TRUE)
#Area under the curve: 0.9071
threshold <- 0.381 

#predicting the test set
rfTestPred <- predict(m_rf, newdata=test, type="prob")

testId = dfTest$EventId
predicted <- rep("b",550000)
predicted[rfTestPred[,2]>=threshold] <- "s"
weightRank = rank(rfTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "rf_submission.csv", row.names=FALSE)

probsubmission = data.frame(EventId = testId, Probbackg = rfTestPred[,1], Probsignal = rfTestPred[,2])

write.csv(probsubmission, "rf_prosubmission.csv", row.names=FALSE)


##Weight is confidence of that data point because some data points are being simulated. 
#Use weight to calculate AMS 

# Convert PRI_jet_num to factor as instructed on the website.
#dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
#dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)
str(dfTrain)

weight <- dfTrain$Weight
labels <- dfTrain$Label


dfTrain$Label = ifelse(dfTrain$Label == 's', 1, 0)

train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

library(psych) #Library that contains helpful PCA functions, such as:

corrplot(cor(dfTrain[,-1]))

cortrain = cor(dfTrain[,-1])

fa.parallel(dfTrain[,-1], #The data in question.
            #n.obs = 305, #Since we supplied a covaraince matrix, need to know n.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 100) #Number of simulated analyses to perform.
abline(h = 1)

pc_train = principal(dfTrain[,-1], #The data in question.
                      nfactors = 9, #The number of PCs to extract.
                      rotate = "none")
pc_train

#biplot(pc_train)

factor.plot(pc_train,
            labels = colnames(dfTrain))
#pairs(pc_train$scores)

load <- pc_train$loadings[,1:2]
plot(load, type = 'n') # set up plot
text(load,labels=names(dfTrain),cex=.7)

MMmodel = lm(Label ~ ., dfTrain[,c(-1,-32)])

library(FactoMineR)
result <- PCA(dfTrain)
dfTrain[dfTrain==-999.0] <- NA
hist(dfTrain$DER_mass_MMC, color = dfTrain$Label, xlim = c(0,400), breaks = 200)

plot(dfTrain$DER_mass_MMC, dfTrain$Label)

singal = dfTrain[dfTrain$Label == 1,]

hist(singal$DER_mass_MMC, xlim = c(0,400), breaks = 200)
noise = dfTrain[dfTrain$Label == 0,]
hist(noise$DER_mass_MMC, xlim = c(0,400), breaks = 200)


library(ggplot2)
g <- ggplot(data = dfTrain, aes(x = DER_mass_MMC))
g+geom_density(aes(fill=factor(Label)), alpha = 0.7)+xlim(0, 200)

g <- ggplot(data = dfTrain, aes(x = PRI_jet_leading_pt))
g+geom_density(aes(fill=factor(Label)), alpha = 0.7)+xlim(0.000001, 400)

g <- ggplot(data = dfTrain, aes(x = log(PRI_jet_leading_pt)))
g+geom_density(aes(fill=factor(Label)), alpha = 0.7)#+xlim(0.01, 400)


print(VIM::aggr(dfTrain))
summary(VIM::aggr(dfTrain))

jet0 = dfTrain[dfTrain$PRI_jet_num == 0,]
jet1 = dfTrain[dfTrain$PRI_jet_num == 1,]
jet2 = dfTrain[dfTrain$PRI_jet_num == 2,]
jet3 = dfTrain[dfTrain$PRI_jet_num == 3,]

VIM::aggr(jet0)
VIM::aggr(jet1)
VIM::aggr(jet2)
VIM::aggr(jet3)
