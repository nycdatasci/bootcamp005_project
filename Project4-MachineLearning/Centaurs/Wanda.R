library(caret)
library(dplyr)
library(car)
library(pscl)
library(ROCR)

dfTrain <- read.csv('./data/training.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA  # to check skewness log(NA) [1] NA [1] 6200031
dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
train2 <- dfTrain[, -c(1,32)] # keeping Label

Train <- createDataPartition(train2$Label, p=0.8, list=FALSE) #80/20 split
training <- train2[Train, ] #200001 obs.
# run transf.training is the transformed model after taking logs of 14 positively skewed variables
ttesting <- train2[ -Train, ] #49999 obs. 
#validation set, missing values should be <-0 instead of NA. has to also have log columns?
ttesting[is.na(ttesting)] <- 0 
#rename ttesting column names to match log ones. 
ttesting <- ttesting %>% rename(., PRI_jet_subleading_pt_log =PRI_jet_subleading_pt, DER_deltaeta_jet_jet_log = DER_deltaeta_jet_jet,
   DER_mass_jet_jet_log = DER_mass_jet_jet, DER_sum_pt_log = DER_sum_pt, PRI_met_sumet_log = PRI_met_sumet, 
   DER_pt_ratio_lep_tau_log = DER_pt_ratio_lep_tau, DER_mass_vis_log = DER_mass_vis, DER_mass_transverse_met_lep_log = DER_mass_transverse_met_lep,
   PRI_lep_pt_log=PRI_lep_pt, DER_pt_tot_log=DER_pt_tot, PRI_met_log=PRI_met, PRI_jet_leading_pt_log=PRI_jet_leading_pt,
   DER_pt_h_log = DER_pt_h, PRI_tau_pt_log = PRI_tau_pt)


#Adding transformed variables. keep NA's at first because log(0) = -Inf
trans.training <- training %>% mutate(., PRI_jet_subleading_pt_log = log(training$PRI_jet_subleading_pt + 1e-8),
                                      DER_deltaeta_jet_jet_log = log(training$DER_deltaeta_jet_jet+ 1e-8), DER_mass_jet_jet_log = log(training$DER_mass_jet_jet+ 1e-8),
                                      DER_sum_pt_log = log(training$DER_sum_pt+ 1e-8), PRI_met_sumet_log=log(training$PRI_met_sumet+ 1e-8), 
                                      DER_pt_ratio_lep_tau_log = log(training$DER_pt_ratio_lep_tau+ 1e-8),
                                      DER_mass_vis_log = log(training$DER_mass_vis+ 1e-8), DER_mass_transverse_met_lep_log = log(training$DER_mass_transverse_met_lep+ 1e-8),
                                      PRI_lep_pt_log = log(training$PRI_lep_pt+ 1e-8), DER_pt_tot_log = log(training$DER_pt_tot+ 1e-8),
                                      PRI_met_log = log(training$PRI_met+ 1e-8), PRI_jet_leading_pt_log = log(training$PRI_jet_leading_pt+ 1e-8),
                                      DER_pt_h_log = log(training$DER_pt_h+ 1e-8), PRI_tau_pt_log = log(training$PRI_tau_pt+ 1e-8))
#-27, -5, -6, -10, -22, -11, -3, -2, -17, -9, -20, -24, -4, -14
#length(is.na(trans.training)) #[1] 9000045

#run second
#training set with transformed variables
transf.training <- select(trans.training, DER_mass_MMC, DER_prodeta_jet_jet, DER_deltar_tau_lep, DER_met_phi_centrality, 
                          DER_lep_eta_centrality,PRI_tau_eta,PRI_tau_phi,PRI_lep_eta, PRI_lep_phi,PRI_met_phi,
                          PRI_jet_num,PRI_jet_leading_eta, PRI_jet_leading_phi,
                          PRI_jet_subleading_eta, PRI_jet_subleading_phi,PRI_jet_all_pt,Label,PRI_jet_subleading_pt_log,DER_deltaeta_jet_jet_log,
                          DER_mass_jet_jet_log,DER_sum_pt_log,PRI_met_sumet_log,DER_pt_ratio_lep_tau_log,DER_mass_vis_log,DER_mass_transverse_met_lep_log,
                          PRI_lep_pt_log,DER_pt_tot_log,PRI_met_log,PRI_jet_leading_pt_log,DER_pt_h_log,PRI_tau_pt_log)

#imputing NA's to 0
transf.training[is.na(transf.training)] <- 0 

#Saturated model
saturated.model = glm(formula = Label ~., family = "binomial", data = transf.training) 
# keeping NAs, AIC: 58230 # when NAs <- 0, AIC: 205300
summary(saturated.model) # significant variables include mostly DER variables
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -5.367e+00  1.613e-01 -33.275  < 2e-16 ***
#   DER_mass_MMC                    -1.614e-03  1.222e-04 -13.214  < 2e-16 ***
#   DER_prodeta_jet_jet             -9.977e-02  5.094e-03 -19.586  < 2e-16 ***
#   DER_deltar_tau_lep               1.245e+00  1.932e-02  64.439  < 2e-16 ***
#   DER_met_phi_centrality           2.795e-01  6.238e-03  44.807  < 2e-16 ***
#   DER_lep_eta_centrality           1.207e+00  3.241e-02  37.255  < 2e-16 ***
#   PRI_jet_num1                    -4.042e+00  1.241e-01 -32.569  < 2e-16 ***
#   PRI_jet_num2                    -6.162e+00  2.172e-01 -28.372  < 2e-16 ***
#   PRI_jet_num3                    -6.534e+00  2.080e-01 -31.420  < 2e-16 ***
#   PRI_jet_all_pt                  -7.213e-03  2.586e-04 -27.888  < 2e-16 ***
#   PRI_jet_subleading_pt_log        4.491e-01  3.963e-02  11.332  < 2e-16 ***
#   DER_deltaeta_jet_jet_log        -1.583e-01  1.448e-02 -10.929  < 2e-16 ***
#   DER_mass_jet_jet_log             9.325e-02  2.482e-02   3.757 0.000172 ***
#   DER_mass_vis_log                -1.623e+00  3.741e-02 -43.383  < 2e-16 ***
#   DER_mass_transverse_met_lep_log -3.809e-01  5.653e-03 -67.372  < 2e-16 ***
#   DER_pt_tot_log                  -4.721e-02  5.194e-03  -9.090  < 2e-16 ***
#   PRI_met_log                     -1.528e-01  8.647e-03 -17.674  < 2e-16 ***
#   PRI_jet_leading_pt_log           1.163e+00  3.910e-02  29.742  < 2e-16 ***
#   DER_pt_h_log                     5.402e-02  7.929e-03   6.813 9.59e-12 ***

varImp(saturated.model)
# absolute value of the t-statistic for each model parameter..determine if it’s significantly different from zero.
# ranked
#                                 Overall
# DER_mass_transverse_met_lep_log 66.76642050
# DER_deltar_tau_lep              63.86521642
# DER_met_phi_centrality          45.71434091
# DER_mass_vis_log                42.51086547
# DER_lep_eta_centrality          36.87521288
# PRI_jet_num1                    32.33279353
# PRI_jet_num3                    31.20354816
# PRI_jet_leading_pt_log          29.46570191
# PRI_jet_num2                    28.06169743
# PRI_jet_all_pt                  27.18501980
# DER_prodeta_jet_jet             20.14246581
# PRI_met_log                     17.10157772
# DER_mass_MMC                    13.45848377
# PRI_jet_subleading_pt_log       11.10450003
# DER_deltaeta_jet_jet_log        10.90790792
# DER_pt_tot_log                   9.50775726
# DER_pt_h_log                     6.56258156
# DER_mass_jet_jet_log             3.69749767

# varImp(BIC.model)
# Overall
# DER_mass_MMC                    13.461914
# DER_prodeta_jet_jet             20.164759
# DER_deltar_tau_lep              63.999820
# DER_met_phi_centrality          45.745976
# DER_lep_eta_centrality          36.875659
# PRI_jet_num1                    37.502580
# PRI_jet_num2                    30.596516
# PRI_jet_num3                    33.546763
# PRI_jet_all_pt                  27.277514
# PRI_jet_subleading_pt_log       11.761509
# DER_deltaeta_jet_jet_log        10.880872
# DER_mass_jet_jet_log             3.670054
# DER_pt_ratio_lep_tau_log        26.171945
# DER_mass_vis_log                42.544046
# DER_mass_transverse_met_lep_log 66.863476
# DER_pt_tot_log                  10.089022
# PRI_met_log                     17.251937
# PRI_jet_leading_pt_log          38.270655
# DER_pt_h_log                     6.669903
# PRI_tau_pt_log                  70.405693

#somewhat high residual deviance
pchisq(saturated.model$df.residual,199968) #[1] 0.50
pchisq(BIC.model$df.residual, 199980) #[1] 0.50
#goodness of fit is good for both models, not very clearly significantly different
pchisq(saturated.model$null.deviance - saturated.model$df.residual, 200000-199968) #[1] 1
pchisq(BIC.model$null.deviance - BIC.model$df.residual, 200000-199980) #[1] 1

#model with no variables
model.empty = glm(formula = Label ~ 1, family = "binomial", data = transf.training) #AIC: 257119
scope = list(lower = formula(model.empty), upper = formula(saturated.model))
n=nrow(transf.training)
#Stepwise Regression k = log(n) is BIC. refer to formula.
bothBIC.full = step(saturated.model, scope, direction = "both", k = log(n)) 
#takes 10 minutes to run. run again later. save workspace

BIC(bothBIC.full) #[1] 247909.6 why is this number different from the smallest one from bothBIC.full? 205486.7
#BIC penalizes a model depending on how complex it is
# Step:  BIC=205486.7

BIC.model = glm(formula = Label ~ DER_mass_MMC + DER_prodeta_jet_jet + DER_deltar_tau_lep + 
                    DER_met_phi_centrality + DER_lep_eta_centrality + PRI_jet_num +
                    PRI_jet_all_pt + PRI_jet_subleading_pt_log + DER_deltaeta_jet_jet_log +
                    DER_mass_jet_jet_log + DER_pt_ratio_lep_tau_log + DER_mass_vis_log +
                    DER_mass_transverse_met_lep_log + DER_pt_tot_log + PRI_met_log +
                    PRI_jet_leading_pt_log + DER_pt_h_log + PRI_tau_pt_log, family = "binomial", data = transf.training)
summary(BIC.model)
# all have signifcant p values. BIC: 205260

# Evaluate Collinearity
vif(BIC.model) # GVIF very high for PRI_jet_num and all jet variables - which indicates it is influencing other variables as expected
vif(saturated.model) 

#new model
#curvy plot 
scatter.smooth(BIC.model$fit,
               residuals(BIC.model, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot for\nLogistic Regression of Data")
abline(h = 0, lty = 2) 

influencePlot(saturated.model) # There are some high leverage points but luckily they have low residuals 
# StudRes          Hat        CookD
# 32661 -1.869177 1.035143e-02 0.0014734206
# 49452 -1.112085 1.757672e-02 0.0004646464
# 84704 -3.952744 6.720194e-06 0.0004987364
exp(saturated.model$coefficients)

#cbind("Log Odds" = model.full$coefficients,
#      "Odds" = exp(model.full$coefficients))
#confint(model.full)

pchisq(saturated.model$deviance, saturated.model$df.residual, lower.tail = FALSE)
#[1] 1.001728e-16 very close to 0 ???
pchisq(BIC.model$deviance, BIC.model$df.residual, lower.tail = FALSE)
#[1] 1.08386e-16 very close to 0 

pred = predict(saturated.model, newdata=ttesting) # have to update ttesting column names above. 
accuracy <- table(pred, ttesting[,"Label"])
sum(diag(accuracy))/sum(accuracy) #[1] 4.00008e-05 is so low
#confusionMatrix(data=pred, ttesting$Label) doesn't work

#http://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation
#null deviance is the same between the 2
saturated.model$deviance-BIC.model$deviance #[1] -6.073273
pR2(saturated.model)
#[1] 0.2018732...scale of 0 to 1, 0 indicating no predictive power
pR2(BIC.model)
#[1] 0.2018496.... worser fit than saturated
# Alternative way to find R^2
# Rsquared.saturated= 1 - saturated.model$deviance/saturated.model$null.deviance #[1] 0.2018732
# Rsquared.BIC= 1 - BIC.model$deviance/BIC.model$null.deviance #[1] 0.2018496..worse fit than the saturated

#Accuracy Testing
# Compute AUC for predicting Class with the model
prob <- predict(saturated.model, newdata=ttesting, type="response")
pred <- prediction(prob, ttesting$Label)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
#Area under the curve: 0.66

# Compute AUC for predicting Class with the model
prob <- predict(BIC.model, newdata=ttesting, type="response")
pred <- prediction(prob, ttesting$Label)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
#Saturated model: 
# 80%: 0.799
# 20%: Area under the curve: [1] 0.6634277
# BIC model: 
#80%: 
#20% [1] 0.6638395 #not much different

#prediction?
label.predicted = round(saturated.model$fitted.values) #?
label.predicted.BIC = round(BIC.model$fitted.values)
#sum(diag(accuracy))/sum(accuracy) #[1] 0??
#diag(accuracy) #0?
#confusionMatrix(data=pred, ttesting$Label)

#KEY TAKEAWAYS#
#Data Pre-processing
#Missingness Patterns
#Positively skewed variables were log transformed

# Logistic Regression 
# Collinearity with Jet variables (VIF)
# Top Variables - Mass
# BIC stepwise regression reduced the number of PRI variables, but is not much a better fit than Saturated model
# AUC not much different, pschi not much different
# try Ridge Regression instead

#> saturated.model$deviance/saturated.model$null.deviance
#[1] 0.7981268

#remove the 0's and make them NA's before looking at skewness
# 1 - model.bic$deviance/model.bic$null.deviance
# [1] 0.2294176

#the data cannot have more levels than the reference
#Named num [1:49999]

#17 variables are positively skewed...mean>median..If skewness = 0, the data are perfectly symmetrical.
#If skewness is less than −1 or greater than +1, the distribution is highly skewed.
# qplot(training$PRI_jet_subleading_pt, geom = 'histogram', binwidth = 2) # positively skewed
# qplot(training$DER_deltaeta_jet_jet, geom = 'histogram', binwidth = .2) skewed
# qplot(training$DER_mass_jet_jet, geom = 'histogram', binwidth = .2) skewed
# qplot(training$DER_sum_pt, geom = 'histogram', binwidth = .2) skewed #2.3
# qplot(training$PRI_met_sumet, geom = 'histogram', binwidth = .2) skewed #1.85
# qplot(training$DER_pt_ratio_lep_tau, geom = 'histogram', binwidth = .2) skewed #2.7
# qplot(training$DER_mass_vis, geom = 'histogram', binwidth = .2) skewed #3.75
# qplot(training$DER_mass_transverse_met_lep, geom = 'histogram', binwidth = .2) skewed #1.2
# qplot(training$PRI_lep_pt, geom = 'histogram', binwidth = .2) skewed
# qplot(training$DER_pt_tot, geom = 'histogram', binwidth = .2) skewed
# qplot(training$PRI_met, geom = 'histogram', binwidth = .2) skewed
# qplot(training$PRI_jet_leading_pt, geom= 'histogram', binwidth = .2)  skewed
# qplot(training$DER_pt_h, geom= 'histogram', binwidth = .2) skewed
# qplot(training$PRI_tau_pt, geom= 'histogram', binwidth = .2) skewed
# qplot(training$DER_mass_MMC, geom= 'histogram', binwidth = .2) skewed ### i don't want to touch this?
# qplot(training$DER_met_phi_centrality, geom= 'histogram', binwidth = .2) #wierd u shape
# library(moments)
# 
# qplot(training$PRI_jet_subleading_pt, geom = 'histogram', binwidth = 2)

#varImp(model.full)
#compar bic results with varimp results 
# > varImp(model.full) 12 variables
# Overall
# DER_mass_MMC                21.66877807
# DER_mass_transverse_met_lep 85.21689265
# DER_mass_vis                80.18645816
# DER_mass_jet_jet            31.43334481
# DER_deltar_tau_lep          88.30106449
# DER_pt_ratio_lep_tau        54.56148667
# DER_met_phi_centrality      31.84153497
# DER_lep_eta_centrality      33.74806260
# PRI_jet_num1                24.42359195
# PRI_met                     16.85300515
# DER_pt_h                    14.26482920
# PRI_met_sumet               14.77320070

# 
# #run first
# trans.training <- training %>% mutate(., PRI_jet_subleading_pt_log = log(training$PRI_jet_subleading_pt),
#                                       DER_deltaeta_jet_jet_log = log(training$DER_deltaeta_jet_jet), DER_mass_jet_jet_log = log(training$DER_mass_jet_jet),
#                                       DER_sum_pt_log = log(training$DER_sum_pt), PRI_met_sumet_log=log(training$PRI_met_sumet), 
#                                       DER_pt_ratio_lep_tau_log = log(training$DER_pt_ratio_lep_tau),
#                                       DER_mass_vis_log = log(training$DER_mass_vis), DER_mass_transverse_met_lep_log = log(training$DER_mass_transverse_met_lep),
#                                       PRI_lep_pt_log = log(training$PRI_lep_pt), DER_pt_tot_log = log(training$DER_pt_tot),
#                                       PRI_met_log = log(training$PRI_met), PRI_jet_leading_pt_log = log(training$PRI_jet_leading_pt),
#                                       DER_pt_h_log = log(training$DER_pt_h), PRI_tau_pt_log = log(training$PRI_tau_pt))
# #-27, -5, -6, -10, -22, -11, -3, -2, -17, -9, -20, -24, -4, -14
# #run second
# #training set with transformed variables
# transf.training <- select(trans.training, DER_mass_MMC, DER_prodeta_jet_jet, DER_deltar_tau_lep, DER_met_phi_centrality, 
#                           DER_lep_eta_centrality,PRI_tau_eta,PRI_tau_phi,PRI_lep_eta, PRI_lep_phi,PRI_met_phi,
#                           PRI_jet_num,PRI_jet_leading_eta, PRI_jet_leading_phi,
#                           PRI_jet_subleading_eta, PRI_jet_subleading_phi,PRI_jet_all_pt,Label,PRI_jet_subleading_pt_log,DER_deltaeta_jet_jet_log,
#                           DER_mass_jet_jet_log,DER_sum_pt_log,PRI_met_sumet_log,DER_pt_ratio_lep_tau_log,DER_mass_vis_log,DER_mass_transverse_met_lep_log,
#                           PRI_lep_pt_log,DER_pt_tot_log,PRI_met_log,PRI_jet_leading_pt_log,DER_pt_h_log,PRI_tau_pt_log)
# 
# qplot(new.training$log.pri.jet.sub.pt, geom = 'histogram', binwidth = .2)
# qplot(training$PRI_lep_phi, geom = 'histogram', binwidth = .2)
