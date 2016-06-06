prob1 = 1 - read.csv("Submissions/xgboost_prob.csv")$xgboostTestPred
threshold1 = 1-.662
#AUC for xgboost = .9254
prob2 = read.csv("Submissions/gbm_prob.csv")$s
threshold2 = .002
#AUC for gbm = .855
prob3 = read.csv("Submissions/rf_probsubmission.csv")[,"Probsignal"]
threshold3 = .381
#AUC = .9071

#################################################
EventId = read.csv("Submissions/EventID.csv")$higgs.testId
#################################################
#Individual submissions
#xgboost
xgboost.prediction = ifelse(prob1 >= threshold1,
                            's',
                            'b')
xgboost.rank = rank(prob1, ties.method = "random")
xgboost.submission = data.frame(EventId = EventId, RankOrder = xgboost.rank, Class = xgboost.prediction)
write.csv(xgboost.submission, "Submissions/xgboost_ensemble_prediction.csv", row.names = F)
#Rank 1340
#AMS = 2.49958

#gbm
gbm.prediction = ifelse(prob2 >= threshold2,
                        's',
                        'b')
gbm.rank = rank(prob2, ties.method = "random")
gbm.submission = data.frame(EventId = EventId, RankOrder = gbm.rank, Class = gbm.prediction)
write.csv(gbm.submission, "Submissions/gbm_ensemble_prediction.csv", row.names = F)
#Rank 1394
#AMS = 2.30069

#random forest
rf.prediction = ifelse(prob3 >= threshold3,
                       's',
                       'b')
rf.rank = rank(prob3, ties.method = "random")
rf.submission = data.frame(EventId = EventId, RankOrder = rf.rank, Class = rf.prediction)
write.csv(rf.submission, "Submissions/rf_ensemble_prediction.csv", row.names = F)
#Rank 1311
#AMS = 2.57949
################################################################

ensembled.prob = (prob1 + prob2 + prob3)/3
threshold = (threshold1 + threshold2 + threshold3)/3

final.prediction = ifelse(ensembled.prob >= threshold,
                         's',
                         'b')

ensembled.rank = rank(ensembled.prob, ties.method= "random")
  
final.submission = data.frame(EventId = EventId, RankOrder = ensembled.rank, Class = final.prediction)
write.csv(submission, "Submissions/ensembled_submission.csv", row.names=FALSE)

##################################################################
#The above ensemble didn't work.
#Now try just majority vote on outcome.
majority.vote = ifelse(((xgboost.prediction == 's') + 
                          (rf.prediction == 's') + 
                          (gbm.prediction == 's'))/2 >= 1,
                       's',
                       'b')

simple.submission = data.frame(EventId = EventId, RankOrder = ensembled.rank, Class = majority.vote)
write.csv(simple.submission, "Submissions/simple_ensemble_submission.csv", row.names = F)

#Ensemble
#1309
#AMS = 2.58510