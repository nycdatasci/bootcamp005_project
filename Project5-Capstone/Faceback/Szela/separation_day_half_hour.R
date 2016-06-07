library(data.table)
setwd("~/GitHub-kszela24/facebook/Szela")

#We NEED to split up the data sets.  Ideally, I'd like to split them based on day and hour, to create
#data subsets for each set, and then predict on them individually.
#We can additionally split into subsections of the 10x10 grid if necessary.
#Length of dataset is 1.5 years: 786239/(60 * 24 * 7 * 52)
#786239 is the max # of minutes.

train = data.table(read.csv("train.csv"))

train$hour = as.integer(floor(train$time/30) %% 48)
train$day = as.integer(floor(train$time/(30 * 48)) %% 7)

summary(train$hour)
dim(train[train$hour == 1 & train$day == 1,])
max(train$day)

for (i in 0:47) {
  for (j in 0:6) {
    file_name = paste0("./partitioned_train/train_day_", j, "_hour_", i,".csv")
    write.csv(train[train$hour == i & train$day == j,], file_name, row.names = F)
  }
}

#Doing the same for the test set.
test = data.table(read.csv("test.csv"))

test$hour = as.integer(floor(test$time/30) %% 48)
test$day = as.integer(floor(test$time/(30 * 48)) %% 7)

for (i in 0:47) {
  for (j in 0:6) {
    file_name = paste0("./partitioned_test/test_day_", j, "_hour_", i,".csv")
    write.csv(test[test$hour == i & test$day == j,], file_name, row.names = F)
  }
}