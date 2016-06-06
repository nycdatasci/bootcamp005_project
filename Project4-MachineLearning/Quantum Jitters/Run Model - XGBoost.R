library(caret)
setwd("C:/Users/steve/OneDrive/Documents/BootCamp/Kaggle")
source('helper.R')

# LOAD DATA
    src.train = readRDS('MichaelsData.rds')
    
    src.EventID = src.train$EventID
    src.label = src.train$Label
    src.weight = src.train$Weight
    mod.train = src.train[,c(-1,-28, -29)]
    
# SCALE AND SUBSET
    scaled.train <- predict(preProcess(mod.train, method = c('center','scale')), mod.train)
    set.seed(4)
    dataSubset <- sample(x = nrow(scaled.train), size = nrow(scaled.train) * .8)
    sub.train <- mod.train[dataSubset,]
    sub.label <- src.label[dataSubset]
    sub.weight <- src.weight[dataSubset]
    
# SET UP TRAINING MODEL
    xgb.ctrl <- trainControl(method = 'cv',
                        number = 3,
                        returnResamp = 'all',
                        summaryFunction = AMS_summary)

    xgb.grid <- expand.grid(nrounds = 300,
                           max_depth = 4,
                           eta = .8,                   #shrinkage
                           gamma = 1,            #min loss reduction
                           colsample_bytree = .7,            #subsample ratio of columns
                           min_child_weight = 1)             #min sum of instance weight
                   
    
    xgb.model <- train(x = sub.train, y = sub.label, method = 'rf', 
                       trControl = xgb.ctrl, tuneGrid = xgb.grid, 
                       preProc=c('center','scale'))
    
    xgb.model
    
    png("xgbmodel6.png", width=6, height=4, units="in", res=300)
    plot(xgb.model)
    dev.off() #only 129kb in size
    
    
    saveRDS(xgb.model,'xgbResults6.rds')

    
# NOW PREDICT AGAINST 20% DATA    
    test.data <- mod.train[-dataSubset,]
    test.label <- src.label[-dataSubset]
    test.weight <- src.weight[-dataSubset]
    test.pred <- predict(xgb.model,newdata = test.data, type='prob')
    library(pROC)
    auc.labels <- ifelse(as.character(test.label)=='s',1,0)
    auc <- roc(auc.labels,test.pred[,2])
    plot(auc,print.thres=TRUE)
    
    # Get threshold from above plot
    Threshold = .338
    predicted = rep('b',length(test.pred[,2]))
    predicted[test.pred[,2]>= Threshold] = 's'
    accuracy.tbl = table(truth = test.label, pred = predicted)
    accuracy.tbl
    accuracy = (accuracy.tbl['s','s'] + accuracy.tbl['b','b'])/length(test.pred[,2])
    accuracy
    
# CREATE SUBMISSION FILE
    submission.raw <- readRDS('TestData.rds')
    submission.pred <- predict(xgb.model,newdata = submission.raw, type='prob')
    predicted <- rep('b',550000)
    predicted[submission.pred[,2]>=Threshold] = 's'
    weightRank <- rank(submission.pred[,2],ties.method = 'random')
    
    submission = data.frame(EventId = submission.raw$EventId,
                            RankOrder = weightRank,
                            Class = predicted)
    
    write.csv(submission, 'SGSubmission.csv', row.names = FALSE)
    