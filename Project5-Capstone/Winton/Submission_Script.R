#Combine all Predictions
dataframe1 <- read.csv('/Users/zacharyescalante/Desktop/Winton/Final_Predictions/Predictions_1.csv')
dataframe2 <- read.csv('/Users/zacharyescalante/Desktop/Winton/Final_Predictions/Predictions_2.csv')
dataframe3 <- read.csv('/Users/zacharyescalante/Desktop/Winton/Final_Predictions/Predictions_3.csv')
dataframe4 <- read.csv('/Users/zacharyescalante/Desktop/Winton/Final_Predictions/Predictions_4.csv')

predictions_1 <- data.frame(Pred1 = dataframe1$RetPlusOne, Pred2 = dataframe2$RetPlusOne,
                            Pred3 = dataframe3$RetPlusOne, Pred4 = dataframe4$RetPlusOne)
FinalPredictionsRetOne = rowMeans(predictions_1) 


predictions_2 <- data.frame(Pred1 = dataframe1$RetPlusTwo, Pred2 = dataframe2$RetPlusTwo,
                            Pred3 = dataframe3$RetPlusTwo, Pred4 = dataframe4$RetPlusTwo)
FinalPredictionsRetTwo = rowMeans(predictions_2) 

Ret1_Index = 61
Ret2_Index = 62
replacement = rep(0, 62*length(FinalPredictionsRetOne))
for(i in 1:length(FinalPredictionsRetOne)){
  #replacement[Ret1_Index] = FinalPredictionsRetOne[i]
  replacement[Ret2_Index] = FinalPredictionsRetTwo[i]
  #Ret1_Index = Ret1_Index + 62
  Ret2_Index = Ret2_Index + 62
}

head(replacement, 130)

submission <- read.csv('/Users/zacharyescalante/Desktop/Winton/sample_submission.csv')
submission$Predicted = replacement

filename = paste0('~/Desktop/Winton/Submission_Files/FinalSubmissions_RetTwo', '_', i, '.csv')
write.csv(submission, filename, row.names = FALSE)

