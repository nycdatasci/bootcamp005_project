#FEATURE ENGINEERING AND STACKING

# Loading the dataset and dropping age, gender, all country dataset variables, the datetime and the time lag variables,\
#as well a user id and the duplicative index.

train_starting <- read.csv("~/unicorn-capstone/train_starting.csv", stringsAsFactors=TRUE)
View(train_starting)
t <- train_starting
t <- t[-c(1:5, 17:20, 22, 25:30)]
View(t)


#parallel processing on server
library(doSNOW)
cl <- makeCluster(4, outfile="")
registerDoSNOW(cl)

#Using a Gradient Boosting Model first
library(gbm)
# splitting 80/20 into train/test
set.seed(0)
train = sample(1:nrow(t), 8*nrow(t)/10)
t.test = t[-train, ]
bookings.test = t$bookings[-train]

set.seed(0)
boost.opt = gbm(bookings ~ ., data = t[train, ],
                distribution = "multinomial",
                n.trees = 1000, shrinkage = .001, interaction.depth = 2)

n.trees = seq(from = 100, to = 1000, by = 10)
boost.pred = predict(boost.opt, newdata = t.test, n.trees = n.trees)

o <- apply(boost.pred[,,i], 1, function(x) which(x == max(x)))

acc=rep(0,43)
for (i in 1:43){
  acc[i]<- sum(diag(table(o, t.test$bookings)))/sum(table(o, t.test$bookings))
}

max(acc)
[1] 0.6161486
which.max(acc)
[1] 41

# Predict bookings variable on 20% set aside for optimal level of trees. This is 410 trees, .01 shrinkage.
# Use optimal level of trees and predict cross-validated bookings variable on 80% train and 20% set aside (for accuracy).
#Predicting bookings variable on test_starting.
set.seed(0)
book.stack = gbm(bookings ~ ., data = t[train, ],
                 distribution = "multinomial",
                 n.trees = 410, shrinkage = .01, cv.folds = 5, interaction.depth = 2)

book.stack.train = predict(book.stack, newdata = t[train,], n.trees = 410)

j <- apply(book.stack.train, 1, function(x) which(x == max(x)))
sum(diag(table(j, t[train,]$bookings)))/sum(table(j, t[train,]$bookings))
0.6146639

book.stack.test = predict(book.stack, newdata = t.test, n.trees = 410)
k <- apply(book.stack.test, 1, function(x) which(x == max(x)))
sum(diag(table(k, t.test$bookings)))/sum(table(k, t.test$bookings))
0.6152116

book.stack.p = predict(book.stack, newdata = p, n.tress = 410)

# MODIFYING TEST
# Loading the dataset and dropping age, gender, all country dataset variables, the datetime and the time lag variables,\
#as well a user id and the duplicative index.

View(test_starting)
p <- test_starting
View(p)
p <- p[-c(1:5, 17)]
View(p)

#Now I have a test to predict bookings into.

book.stack.p = predict(book.stack, newdata = p, n.tress = 410)

bookings.stack.test <- apply(book.stack.p, 1, function(x) which(x == max(x)))

stacked.test <- cbind(p, bookings)

#ADDING PREDICTED BOOKINGS COLUMN TO TRAINING SET

#Predicting bookings column onto training set using optimal trees and shrinkage and saved predictions
t <- train_starting
t <- t[-c(1:5, 17:20, 22, 25:30)]
View(t)

library(caret)
ctrl = trainControl(method = "cv", number = 3, savePred=T)
gbmGrid <-  expand.grid(interaction.depth = 2, n.trees = 410,
                        shrinkage = .01, n.minobsinnode = c(30, 180, 300))
m_gbm = train(x=t, y=t$bookings, method="gbm", distribution = "multinomial", tuneGrid = gbmGrid, verbose=TRUE, trControl=ctrl)

print(m_gbm)
Stochastic Gradient Boosting 

213451 samples
14 predictors
3 classes: 'early', 'NB', 'waited' 

No pre-processing
Resampling: Cross-Validated (3 fold) 
Summary of sample sizes: 142301, 142301, 142300 
Resampling results across tuning parameters:
  
  n.minobsinnode  Accuracy  Kappa
30             1         1    
180             1         1    
300             1         1    

Tuning parameter 'n.trees' was held constant at a value of 410
Tuning parameter 'interaction.depth' was held constant at a value of
2
Tuning parameter 'shrinkage' was held constant at a value of 0.01
Accuracy was used to select the optimal model using  the largest value.
The final values used for the model were n.trees = 410, interaction.depth = 2, shrinkage = 0.01 and n.minobsinnode = 30.

#Removing bookings column
View(t)
t <- t[-c(12)]


#Adding bookings column to training dataset
library(dplyr)
v <- filter(m_gbm$pred, m_gbm$pred[,6] == '30') #Out of fold predictions at optimal min. # of observations in node
bookings <- as.factor(as.vector(v[,1]))
v <- as.data.frame(bookings)
stackedtrain <- data.frame(t, bookings)
View(stackedtrain)

#Re-adding user id and some other columns and writing files
f <- cbind(p[c(2,17)],stacked.test)
View(f)
write.csv(f, "bkgs.stacked.test.csv", row.names=FALSE)
z <-train_starting
x <- cbind(z[c(2, 18:20)],stackedtrain, z[c(17)])
write.csv(x, "bkgs.stacked.train.csv", row.names=FALSE)

