prep = function(df) {
  colnames(df) <- column.names #Rename columns
  names(df)[42] <- "outcome"
  df$outcome <- as.factor(df$outcome)
  df$outcome.response <- ifelse(df$outcome == 'normal',0,1)
  
  #Dealing with 3 Categorical Variables, 0/1, expanding ncols, replace into new.KDD.*
  service_<-as.data.frame(class.ind(df$service))
  protocol_type_<-as.data.frame(class.ind(df$protocol_type))
  flag_<-as.data.frame(class.ind(df$flag))
  new <- cbind(service_, protocol_type_, flag_)
  cat('Dummy features:',dim(new)[2],'\n')
  new.df = cbind(duration=df$duration, new, df[,5:41], outcome.response=df[,44])
  cat('New dim:',dim(new.df))
  return(new.df)
}

new.KDD.test= prep(KDD.test)

#Comparing columns in test and train
a = sapply(colnames(new.KDD.test), function(i) ifelse(i %in% colnames(new.KDD.train), TRUE, FALSE))
which(a==FALSE)
b = sapply(colnames(new.KDD.train), function(i) ifelse(i %in% colnames(new.KDD.test), TRUE, FALSE))
which(b==FALSE)

for (i in names(b[b==FALSE])) {
  new.KDD.test[,i] = 0
}
new.KDD.test = new.KDD.test[colnames(new.KDD.train)]

#Check if columns match between train and test
names(new.KDD.test)==names(new.KDD.train)

y_test=new.KDD.test[,123]

yhat = predict(best_rf, newdata = KDD.test.random[,-123])


