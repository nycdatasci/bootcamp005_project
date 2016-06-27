
#Comparing columns in test and train
a = sapply(unique(KDD.train[,42]), function(i) ifelse(i %in% unique(KDD.test[,42]), TRUE, FALSE))
which(a==FALSE)
b = sapply(unique(KDD.test[,42]), function(i) ifelse(i %in% unique(KDD.train[,42]), TRUE, FALSE))
which(b==FALSE)


plot(density(KDD.train$src_bytes))
table(KDD.train$land, KDD.train[,42])
table(KDD.train$wrong_fragment)
table(KDD.train$urgent)