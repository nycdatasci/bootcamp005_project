data <- data.frame(read.csv('training.csv', header=TRUE))

# Data Exploration
#dim(data)
#head(data)
#summary(data)

clean_data <- data[,-c(1,32,33)] # Remove EventId, Weight, Label

#### PCA
library(psych)
fa.parallel(clean_data, fa = "pc", n.iter = 100) 
# show the eigen values for a principal components using fa = "pc" 
abline(h = 1)
# Parallel analysis suggests that the number of factors =  NA  and the number of components =  8
# K = 8

pc_data = principal(clean_data, #The data in question.
                      nfactors = 8, #The number of PCs to extract.
                      rotate = "none")
pc_data
pc_data$loadings
#                   PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8
# SS loadings    12.267 2.369 2.317 1.980 1.665 1.555 1.207 1.125
# Proportion Var  0.409 0.079 0.077 0.066 0.056 0.052 0.040 0.037
# Cumulative Var  0.409 0.488 0.565 0.631 0.687 0.738 0.779 0.816
# http://stats.stackexchange.com/questions/2691/making-sense-of-principal-component-analysis-eigenvectors-eigenvalues

factor.plot(pc_data, labels = colnames(clean_data))




#### RIDGE
data_label <- data[,-32]

x = model.matrix(Label ~ ., data_label)[, -1]
y = as.numeric(data_label$Label)-1

set.seed(0)
train = sample(1:nrow(x), 8*nrow(x)/10)
test = (-train)
y.test = y[test]
length(train)/nrow(x)
length(y.test)/nrow(x)

grid = 10^seq(5, -2, length = 100)

#Fitting the ridge regression. Alpha = 0 for ridge regression.
library(glmnet)
ridge.models = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
plot(ridge.models, xvar = "lambda", label = TRUE, main = "Ridge Regression")

set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train], alpha = 0, nfolds = 10, lambda = grid)
plot(cv.ridge.out, main = "Ridge Regression\n")
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge
log(bestlambda.ridge)
# The error seems to be reduced with a log lambda value of around -4.60517; this
# corresponds to a lambda value of about 0.01. This is the value of lambda
# we should move forward with in our future analyses.

ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)
# Test MSE is 0.1690062

ridge.out = glmnet(x, y, alpha = 0)
predict(ridge.out, type = "coefficients", s = bestlambda.ridge)
# DER_deltar_tau_lep           1.376898e-01
# DER_pt_ratio_lep_tau        -8.457204e-02
# PRI_jet_num                 -6.704548e-02
# DER_met_phi_centrality       4.768406e-02
# PRI_lep_pt                   4.766785e-03
# PRI_tau_pt                   4.005498e-03
# DER_mass_transverse_met_lep -3.292986e-03
# DER_mass_vis                -2.366445e-03

ridge.bestlambda = predict(ridge.out, s = bestlambda.ridge, newx = x)
mean((ridge.bestlambda - y)^2)
# Overall MSE is 0.1708973



#### LASSO

lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso
log(bestlambda.lasso)
# The error seems to be reduced with a log lambda value of around -4.60517; this
# corresponds to a lambda value of about 0.01 This is the value of lambda
# we should move forward with in our future analyses.

lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)

# Test MSE is 0.1770922

lasso.out = glmnet(x, y, alpha = 1)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)
# DER_deltar_tau_lep           7.449233e-02
# DER_met_phi_centrality       4.692523e-02
# DER_pt_ratio_lep_tau        -2.663997e-02
# PRI_jet_num                 -1.097780e-02
# PRI_tau_pt                   3.658258e-03
# DER_mass_transverse_met_lep -3.139438e-03
# PRI_lep_pt                   1.357683e-03

lasso.bestlambda = predict(lasso.out, s = bestlambda.lasso, newx = x)
mean((lasso.bestlambda - y)^2)

# Overall MSE is 0.1781932



#### COMPARISON

predict(ridge.out, type = "coefficients", s = bestlambda.ridge)
predict(lasso.out, type = "coefficients", s = bestlambda.lasso)
mean((ridge.bestlambda - y)^2)
mean((lasso.bestlambda - y)^2)

# The ridge regression MSE was slightly lower than that of the lasso. Although 
# this might be due to random variability among our dataset, if we are strictly
# talking about predictive power, we might choose to move forth with the ridge
# regression in this scenario. 
# If we are particularly interested in dimension reduction and variable selection,
# we might choose to move forth with the lasso regression.