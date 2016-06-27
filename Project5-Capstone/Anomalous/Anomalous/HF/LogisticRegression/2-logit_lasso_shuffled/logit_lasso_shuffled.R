#################################################
####       Network Intrusion Detection       ####
#### Logistic Regression with Lasso - GLMNET ####
#################################################

library(glmnet)
library(car)
library(MASS)
library(ggplot2)
library(boot)

###################
#### Functions ####
###################

#Define standardization function for train
Standard_self<-function(data) {
  data=(data-mean(data))/sd(data)
  return (data)
}

#Define standardization function for test
Standard<-function(data,ave,sd) {
  data=(data-ave)/sd
  return (data)
}

#Define performance function
performance = function(target, pred) {
  contingency = table(truth = target, prediction = pred)
  cat('Accuracy: ',sum(diag(contingency))/sum(contingency),'\n')
  cat('True positive rate: ',contingency[2,2]/sum(contingency[2,]),'\n')
  cat('False positive rate: ',contingency[1,2]/sum(contingency[1,]),'\n')
  return(contingency)
}



#########################
#### Scale and split ####
#########################

#Finding binary columns to exclude from scaling
bin.col = data.frame(matrix(ncol = 4, nrow = dim(new.KDD.train.shuffle)[2]-1))
colnames(bin.col) = c('col','unique','max','min')
for (i in 1:(dim(new.KDD.train.shuffle)[2]-1)) {
  bin.col[i,1]=i
  bin.col[i,2]=length(unique(new.KDD.train.shuffle[,i]))
  bin.col[i,3]=max(new.KDD.train.shuffle[,i])
  bin.col[i,4]=min(new.KDD.train.shuffle[,i])
}
bin.col = bin.col[bin.col$unique==2,1]

#Scaling non-binary columns in train
new.KDD.train.scaled = sapply(new.KDD.train.shuffle[,-c(bin.col,122)], Standard_self)
new.KDD.train.scaled = cbind(new.KDD.train.shuffle[,c(bin.col,122)], new.KDD.train.scaled)
new.KDD.train.scaled = new.KDD.train.scaled[,colnames(new.KDD.train.shuffle)]

#Scaling non-binary columns in test
ave = sapply(new.KDD.train.shuffle[,-c(bin.col,122)], mean)
sd = sapply(new.KDD.train.shuffle[,-c(bin.col,122)], sd)
new.KDD.test.scaled = sapply(new.KDD.test.shuffle[,-c(bin.col,122)], function(x) Standard(x,ave,sd))
new.KDD.test.scaled = cbind(new.KDD.test.shuffle[,c(bin.col,122)], new.KDD.test.scaled)
new.KDD.test.scaled = new.KDD.test.scaled[,colnames(new.KDD.test.shuffle)]


#####################
#### Logit Lasso ####
#####################

#Creating the data matrices for the glmnet() function.
x.train = model.matrix(outcome.response ~ ., new.KDD.train.scaled)[, -1]
y.train = new.KDD.train.scaled$outcome.response
x.test = model.matrix(outcome.response ~ ., new.KDD.test.scaled)[, -1]
y.test = new.KDD.test.scaled$outcome.response

#Creating training and test sets within Train dataset
set.seed(5)
train = sample(1:nrow(x.train), 7*nrow(x.train)/10)

#Fitting the logistic regression on a grid of lambda (for plot only). Alpha = 1 for lasso penalty
grid = 10^seq(0, -5, length = 200)
logit.models = glmnet(x.train[train, ], y.train[train],
                      alpha = 1,
                      lambda = grid,
                      family="binomial")
plot(logit.models,
     xvar = "lambda",
     label = TRUE,
     main = "Logistic Regression with Lasso penalty\n")


#Cross-validation
set.seed(5)
logit.cv = cv.glmnet(x.train[train, ], y.train[train], 
                     alpha = 1,            #Lasso penalty
                     nfolds = 10,          #k-fold CV
                     type.measure='class', #Misclassification measure
                     family="binomial",    #Logistic regression
                     lambda = grid)
plot(logit.cv, main = "Logistic Regression with Lasso penalty\n")


#Best lambda
logit.cv$lambda.min #9.011018e-05
log(logit.cv$lambda.min) #-9.314477
lambda = exp(-3) #lamdba.min still keeps 96 features. Need to balance complexity and accuracy


#Checking performance of model - train data, test subset
logit.test.class = predict(logit.cv, 
                           s = lambda, 
                           type = 'class',
                           newx = x.train[-train, ]) #As per function documentation
performance(y.train[-train], logit.test.class) 
# Accuracy:  0.9172047 
# True positive rate:  0.8830164 
# False positive rate:  0.05141088 
# prediction
# truth     0     1
# 0 18691  1013
# 1  2116 15972

#Checking performance of model - test data
logit.test.class.final = predict(logit.cv, 
                                 s = lambda, 
                                 type = 'class',
                                 newx = x.test) #As per function documentation
performance(y.test, logit.test.class.final) 
# Accuracy:  0.856319 
# True positive rate:  0.7283088 
# False positive rate:  0.02426477 
# prediction
# truth     0     1
# 0 11380   283
# 1  2956  7924

# #Save predicted probabilities for Joseph
# saveRDS(predict(logit.cv, s=lambda, newx = x.train, type='response'), file='logit.pred.train.proba.unshuffled.rds')
# saveRDS(predict(logit.cv, s=lambda, newx = x.test, type='response'), file='logit.pred.test.proba.unshuffled.rds')

#Coefficients: 
logit.coef = predict(logit.cv, 
                     s = lambda, 
                     type = 'coefficients')
sum(logit.coef!=0)-1 #Keep 96 features with lambda.min, 11 with lambda=exp(-3)
logit.nonzero = predict(logit.cv, 
                        s = lambda, 
                        type = 'nonzero')
logit.coef.df = data.frame(var = colnames(x.train)[logit.nonzero[,1]],
                           coef = logit.coef[logit.nonzero[,1]+1])
logit.coef.df[order(logit.coef.df$coef, decreasing = T),]
# write.csv(logit.coef.df, file='logit.coef.df.csv')


#############################
#### Performance Summary ####
#############################

#Plot lambda vs coefficients vs accuracy
summary.plot = function(x,y) {
  res = data.frame(matrix(ncol = 3, nrow = length(grid)))
  colnames(res) = c('lambda','accuracy','coef')
  for (i in 1:length(grid)) {
    #Insert lambda
    res[i,1] = grid[i]
    
    #Insert accuracy %
    pred.class = predict(logit.cv, s = grid[i], type = 'class', newx = x)
    t = table(truth = y, prediction = pred.class)
    res[i,2] = sum(diag(t))/sum(t)
    
    #Insert coef count
    pred.coef = predict(logit.cv, s = grid[i], type = 'coefficients')
    res[i,3] = sum(pred.coef!=0)-1
  }
  return(res)
}
plot.train = summary.plot(x.train[-train,], y.train[-train])
plot.test = summary.plot(x.test, y.test)

#Draw summary plot
ggplot(plot.train,aes(coef, accuracy)) + 
  geom_point(aes(colour='red')) +
  geom_point(data=plot.test, aes(x=coef, y=accuracy, colour='blue')) +
  labs(title="Model accuracy by number of features",
       x="Count of features",
       y="Accuracy",
       colour="Data") +
  scale_colour_discrete(labels = c("Test", "Train")) +
  geom_vline(xintercept = (sum(logit.coef!=0)-1)) +
  theme_bw()


#####################
#### Diagnostics ####
#####################

#Regenerate logistic model using GLM (no regularization), with columns identified previously
model.var = colnames(x.train)[logit.nonzero[,1]]
model.var = model.var[-c(7)] #"logged_in" is not significant
formula = as.formula(paste("outcome.response ~", paste(model.var, collapse = " + ")))
logit.glm = glm(formula, 
                family = "binomial", 
                data = new.KDD.train.scaled[train,])
summary(logit.glm) #All coefficients are now significant

#CV error:
logit.glm.cv = cv.glm(new.KDD.train.scaled[train,], logit.glm, K = 5)
1-logit.glm.cv$delta[2] #0.9453371

#Checking performance of prediction:
logit.pred.train = round(predict(logit.glm, newdata=new.KDD.train.scaled[-train,], type='response'))
performance(new.KDD.train.scaled[-train,122], logit.pred.train)
# Accuracy:  0.9329752 
# True positive rate:  0.9034166 
# False positive rate:  0.03989038 
# prediction
# truth     0     1
# 0 18918   786
# 1  1747 16341

logit.pred.test = round(predict(logit.glm, newdata=new.KDD.test.scaled, type='response'))
performance(new.KDD.test.scaled$outcome.response, logit.pred.test)
# Accuracy:  0.8763696 
# True positive rate:  0.7806066 
# False positive rate:  0.03429649 
# prediction
# truth     0     1
# 0 11263   400
# 1  2387  8493

#Goodness of fit test i.e. Test of deviance
pchisq(logit.glm$deviance, logit.glm$df.residual, lower.tail = FALSE) #p-value of 1 > 0.05 cutoff so we fail to reject null hypothesis; model is appropriate

#McFadden's pseudo R^2 based on the deviance
1 - logit.glm$deviance/logit.glm$null.deviance #0.7317411 variance explained

#Variance Inflation Factor
vif(logit.glm)
vif(logit.glm)[vif(logit.glm)>5] #Null, no multicollinearity

# write.csv(as.data.frame(logit.glm$coefficients), file='logit.glm.coef.csv')
# saveRDS(predict(logit.glm, newdata=new.KDD.train.scaled, type='response'), file='logit.pred.train.proba.rds')
# saveRDS(predict(logit.glm, newdata=new.KDD.test.scaled, type='response'), file='logit.pred.test.proba.rds')

#Checking other model summary and assumptions
plot(logit.glm)
influencePlot(logit.glm)
avPlots(logit.glm)
confint(logit.glm)