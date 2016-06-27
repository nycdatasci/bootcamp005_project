logit_lasso_shuffled = readRDS('./logit_lasso_shuffled/logit_lasso_shuffled.rds')
logit_elasticnet_shuffled = readRDS('./logit_elasticnet_shuffled/logit_elasticnet_shuffled.rds')
logit_ridge_shuffled = readRDS('./logit_ridge_shuffled/logit_ridge_shuffled.rds')

library(MuMIn)

logit.ave <- model.avg(logit_lasso_shuffled, logit_elasticnet_shuffled, logit_ridge_shuffled, s=lambda)
summary(logit.ave)

f1 = glm(outcome.response~udp, family = "binomial", data = new.KDD.train.scaled)
f2 = glm(outcome.response~http, family = "binomial", data = new.KDD.train.scaled)
f3 = glm(outcome.response~icmp, family = "binomial", data = new.KDD.train.scaled)

f.ave = model.avg(f1, f2, f3)
summary(f.ave)