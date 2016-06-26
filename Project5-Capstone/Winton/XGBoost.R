#############################
##########XG BOOST###########
#############################
install.packages('mlr')
install.packages('xgboost')
install.packages('Matrix')
install.packages('caret')
install.packages('slackr')
library(slackr)
library(caret)
library(Matrix)
library(xgboost)
library(mlr)

slackr_setup(channel = '@zach.escalante', username = "olivia", icon_emoji = "",
             incoming_webhook_url = "statbusters.slack.com", api_token = "xoxb-54321210150-8qhdboKSVUv7iC0HLoCL17ky",
             echo = FALSE)

slackr('Hey you')

#Step 1: Clean the data and evaluate missingness
winton = read.csv('/Users/zacharyescalante/Downloads/train.csv')
test_set = read.csv('/Users/zacharyescalante/Downloads/test_2.csv')
class(winton) #Dataframe
str(winton)   #all numeric data types
summary(winton) 
#Feature 1: Integer (1 - 10), NA = 33313~  83% of it's data/drop
#Feature 2: Continuous, NA = 9146   (min = 1.00, max = 10.00, mean = 3.59, median = 3.00) -> Impute 3.00 for missing
#Feature 3: Continuous, NA = 1237   (min = -4.6435, max = 4.5304, mean = 0.5584)
#Feature 4: Continuous, NA = 77721  (min = -5.441, max = 2.953, mean = 0.406)
#Feature 5: Integer, NA = 0
#Feature 6: Continuous, NA = 1933  (min = 0.9366, max = 12.6099, mean = 0.4310)
#Feature 7: Integer, NA = 0        (min = 338, max = 99861, mean = 49245)
#Feature 8: Continuous, NA = 469  (min = 0.0098, max = 0.3650, mean = 0.1970)
#Feature 9: Integer, NA = 1875 (min = 0.00, max = 36.00, mean = 10.68, median = 11.00) -> Impute 11.00 for missing
#Feature 10: Integer, NA = 19471 (min = 1.00, max = 6.00, mean = 4.745, median = 5.00) -> Impute 5.00 for missing values
#Feature 11: Continuous, NA = 987   (min = -7.3591, max = 1.7869, mean = -0.5722)
#Feature 12: Continuous, NA = 1096  (min = 0.00, max = 1.00, mean = 0.4985)
#Feature 13: Continuous, NA = 594   (min = 0.00, max = 9.00, mean = 4.238, median = 4.00) -> Impute 4.00 for missing values
#Feature 14: Continuous, NA = 728   (min = -0.1493, max = 3.1618, mean = 1.5885)
#Feature 15: Continuous, NA = 2141  (min = 0.0217, max = 28.0181, mean = 3.8914)
#Feature 16: Continuous, NA = 610   (min = 1.00, max = 2.00, mean = 1.007, median = 1.00) -> Impute 1.00 for missing values
#Feature 17: Continuous, NA = 646   (min = -2.614, max = 1.7869, mean = -0.5722)
#Feature 18: Continuous, NA = 568   (min = -5.758, max = 6.3524, mean = 0.8031)
#Feature 19: Continuous, NA = 1190  (min = -3.2929, max = 0.8982, mean = -1.2054)
#Feature 20: Intger, NA = 7826    (min = 2, max = 10, mean = 5.267, median = 5.00) -> Impute 5.00 for missing values
#Feature 21: Continuous, NA = 1018  (min = -1.515, max = 7.737, mean = 0.6056)
#Feature 22: Continuous, NA = 1345  (min = -5.8199, max = 2.2850, mean = -0.7731)
#Feature 23: Continuous, NA = 1711  (min = -7.2214, max = 3.2289, mean = 0.7998)
#Feature 24: Continuous, NA = 726   (min = -11.4422, max = 2.5267, mean = -1.2093)
#Feature 25: Continuous, NA = 655   (min = -1.903, max = 4.0203, mean = -0.3297)

#Drop 'Feature_1' due to over 80% missignness (was column 2)
data = winton[, -2]
test_ = test_set[, -2]

#Impute the median to these variables
categorical <- c(5, 9, 10, 13, 16, 20)
#Convert potentially categorical variables to factors
for(i in categorical){
  data[,i] = as.factor(data[,i])
  test_[,i] = as.factor(test_[,i])
}


cont.median <- c(2, 12, 15)
#Impute the mean to these variables
cont.mean <- c(3, 4, 6, 7, 8, 11, 14, 17, 18, 19, 21, 22, 23, 24, 25)

#Impute values to every missing data point
for (i in 1:ncol(test_)){
  if(i %in% categorical){
    data[,i][is.na(data[,i])] = names(which(max(table(data[,i])) == table(data[,i])))
    test_[,i][is.na(test_[,i])] = names(which(max(table(test_[,i])) == table(test_[,i])))
  }
  else if(i %in% cont.median){
    data[,i][is.na(data[,i])] = median(data[,i], na.rm=TRUE)
    test_[,i][is.na(test_[,i])] = median(test_[,i], na.rm=TRUE)
  }
  else{
    data[,i][is.na(data[,i])] = mean(data[,i], na.rm=TRUE)
    test_[,i][is.na(test_[,i])] = mean(test_[,i], na.rm=TRUE)
  }
}

#WMAE function
wmae <- function(actual, predicted, weights){
  ret <- sum(abs(actual -  predicted)*weights)/length(actual)
  return(ret)
}



#Edit columns in matrix to only variables used to train our model
#Delete all returns after the 120 minute returns and leave the T+1
#and T+2 returns (respectively for RetPlusOne_df and RetPlusTwo_df)
RetPlusOne_df <- data[,-c(147:206, 208, 209)]
RetPlustOneID <- RetPlusOne_df$Id                    #Pull the ID#'s off, but save them for later
RetPlusOne_df <- RetPlusOne_df[,-1]
RetPlusOneWeightDaily <- RetPlusOne_df$Weight_Daily
RetPlusOne_df$Weight_Daily <- NULL

RetPlusTwo_df <- data[,-c(147:207, 209)]
RetPlustTwoID <- RetPlusTwo_df$Id                    #Pull the ID#'s off, but save them for later
RetPlusTwo_df <- RetPlusTwo_df[,-1]
RetPlusTwoWeightDaily <- RetPlusTwo_df$Weight_Daily
RetPlusTwo_df$Weight_Daily <- NULL


#Create ID column for Test Set
TestID <- test_$Id
test_$Id = NULL

#Create folds for training data
train_index_R1 <- createFolds(RetPlusOne_df$Ret_PlusOne, k = 4, list = TRUE, returnTrain = FALSE)
train_index_R1 = as.data.frame(train_index_R1)
train_index_R2 <- createFolds(RetPlusTwo_df$Ret_PlusTwo, k = 4, list = TRUE, returnTrain = FALSE)
train_index_R2 = as.data.frame(train_index_R2)

for(i in 1:length(train_index_R1)){
  
  #Training index and set for RetPlusOne
  RetOneTrain = RetPlusOne_df[-train_index_R1[[i]],]
  RetOneTest = RetPlusOne_df[train_index_R1[[i]],]
  RetOneWeights = RetPlusOneWeightDaily[train_index_R1[[i]]]
  
  #Training index and set for RetPlusTwo
  RetTwoTrain = RetPlusTwo_df[-train_index_R2[[i]],]
  RetTwoTest = RetPlusTwo_df[train_index_R2[[i]],]
  RetTwoWeights = RetPlusTwoWeightDaily[train_index_R2[[i]]]
  
  #Create sparse model matrix
  sparse_matrix_ret_1 <- sparse.model.matrix(Ret_PlusOne ~ ., data = RetOneTrain)
  sparse_matrix_ret_2 <- sparse.model.matrix(Ret_PlusTwo ~ ., data = RetTwoTrain)
  
  #Train the first XGBoost on RetPlustOne_df
  dtrain_ret1 = xgb.DMatrix(data = sparse_matrix_ret_1,
                            label = RetOneTrain$Ret_PlusOne)   #Check this tomorrow
  
  #Received thsi error before adjusting the 'label' input of xgb.Dmatrix
  #Error in xgb.setinfo(dmat, names(p), p[[1]]) : 
  #The length of labels must equal to the number of rows in the input data
  
  #Train the second XGBoost on RetPlusTwo_df
  dtrain_ret2 = xgb.DMatrix(data = sparse_matrix_ret_2,
                            label = RetTwoTrain$Ret_PlusTwo)   #Check this tomorrow
  
  
  watchlist_1 <- list(train = dtrain_ret1)
  watchlist_2 <- list(train = dtrain_ret2)
  
  set.seed(0)
  params <- list(objective = 'reg:linear',
                 booster = 'gbtree',
                 eval_method = 'MSE',
                 eta = 0.1,
                 max_depth = 9)
  
  RetPlusOneModel <- xgb.train(params = params,
                               data = dtrain_ret1,
                               nrounds = 500,
                               verbose = 1,
                               watchlist = watchlist_1,
                               maximize = FALSE)
  
  RetPlusTwoModel <- xgb.train(params = params,
                               data = dtrain_ret2,
                               nrounds = 500,
                               verbose = 1,
                               watchlist = watchlist_2,
                               maximize = FALSE)
  
  
  #predict values
  validationSetY <- as.vector(RetOneTest$Ret_PlusOne)
  validationSetZ <- as.vector(RetTwoTest$Ret_PlusTwo)
  
  RetOneTest$Ret_PlusOne <- -1
  RetOneSparse <- sparse.model.matrix(Ret_PlusOne ~., data = RetOneTest)
  predsOne <- predict(RetPlusOneModel, RetOneSparse)
  
  RetTwoTest$Ret_PlusTwo <- -1
  RetTwoSparse <- sparse.model.matrix(Ret_PlusTwo ~., data = RetTwoTest)
  predsTwo <- predict(RetPlusTwoModel, RetTwoSparse)
  
  #Test Set Sparse Model Matrix
  test_$Ret_PlusOne <- -1
  TestModel1 <- sparse.model.matrix(Ret_PlusOne ~., data = test_)
  FinalPredictions1 <- predict(RetPlusOneModel, TestModel1)
  test_$Ret_PlusOne = NULL
  
  test_$Ret_PlusTwo <- -1
  TestModel2 <- sparse.model.matrix(Ret_PlusTwo ~., data = test_)
  FinalPredictions2 <- predict(RetPlusTwoModel, TestModel2)
  
  #Create Data Frames for ID, Predicted Values, Actual Values and Weights
  solutions_df1 = data.frame(Stock_ID = RetPlustOneID[train_index_R1[[i]]], Predictions = predsOne, 
                             Actual_Values = validationSetY, Weights = RetOneWeights)
  
  solutions_df2 = data.frame(Stock_ID = RetPlustTwoID[train_index_R2[[i]]], Predictions = predsTwo, 
                             Actual_Values = validationSetZ, Weights = RetTwoWeights)
  
  final_solutions = data.frame(Stock_ID = TestID, RetPlusOne = FinalPredictions1, 
                             RetPlusTwo = FinalPredictions2)
  
  #Write data frame solutions to CSV
  filename1 = paste0('~/Desktop/Winton/PredsOne/RetPlusOne_Prediction', '_', i, '.csv')
  filename2 = paste0('~/Desktop/Winton/PredsTwo/RetPlusTwo_Prediction', '_', i, '.csv')
  filename3 = paste0('~/Desktop/Winton/Final_Predictions/Predictions', '_', i, '.csv')
  
  write.csv(solutions_df1, filename1, row.names = FALSE)
  write.csv(solutions_df2, filename2, row.names = FALSE)
  write.csv(final_solutions, filename3, row.names = FALSE)
  
}

#Stack the files
setwd('/Users/zacharyescalante/Desktop/Winton/PredsOne')
files  <- list.files(pattern = '\\.csv')
tables <- lapply(files, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)
RetOneFinal <- combined.df[order(combined.df$Stock_ID),]  #Order the dataframe by 'Stock ID' 

#Stack the Return Two files
setwd('/Users/zacharyescalante/Desktop/Winton/PredsTwo')
files  <- list.files(pattern = '\\.csv')
tables <- lapply(files, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)
RetTwoFinal <- combined.df[order(combined.df$Stock_ID),]  #Order the dataframe by 'Stock ID'

vec <- rep(0, length(RetOneFinal$Predictions))

#WMAE for RetOneFinal using the training data
K = wmae(RetOneFinal$Actual_Values, RetOneFinal$Predictions, RetOneFinal$Weights)
#wmae(RetOneFinal$Actual_Values, rep(0, length(RetOneFinal$Stock_ID)), RetOneFinal$Weights)
message1 = paste0('Just ran PredOne with an WMAE of: ', round(K, digits = 0), ' and ', param_r, ' = ', j)
slackr(message1)

#WMAE for RetTwoFinal using the training data
R = wmae(RetTwoFinal$Actual_Values, RetTwoFinal$Predictions, RetTwoFinal$Weights)
#wmae(RetTwoFinal$Actual_Values, rep(0, length(RetTwoFinal$Stock_ID)), RetTwoFinal$Weights)
message2 = paste0('Just ran PredTwo with an WMAE of: ', round(R, digits = 0), ' and ', param_r, ' = ', j)
slackr(message2)

