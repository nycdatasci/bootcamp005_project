setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 5 test")



#basic eda
library(data.table)
library(dplyr)
library(ggplot2)
library(VIM)
library(mice)
library(corrplot)

dtTrain = fread('train.csv', header=T)
dtTest = fread('test.csv', header=T)
client = fread('cliente_tabla.csv', header=T)
product = fread('producto_tabla.csv', header=T)
town = fread('town_state.csv', header = T)

str(dtTrain)

summary(dtTrain)

head(dtTrain)

head(dtTest)

corrplot(cor(dtTrain))

#Demanda_uni_equil and Venta variables are correlated

#memory limits reached
#which(colSums(is.na(dfTrain)) != 0 )

#which(rowSums(is.na(dfTrain)) != 0 )

#md.pattern(dfTrain)

#aggr(dfTrain)

head(submission)

#id and predicted Demanda_uni_equil

head(client)

#935362 clients
#Cliente_ID Factor

head(product)

#2592 products
#Producto_ID Factor

head(town)

# 790 locations
#Agencia_ID Factor

#make ID items factors


#Train
#dtTrain[, Canal_ID := as.factor(Canal_ID)]
#dtTrain[, Producto_ID := as.factor(Producto_ID)]
#dtTrain[, Agencia_ID := as.factor(Agencia_ID)]

#Test
#dtTest[, Cliente_ID := as.factor(Cliente_ID)]
#dtTest[, Producto_ID := as.factor(Producto_ID)]
#dtTest[, Agencia_ID := as.factor(Agencia_ID)]

str(dtTrain)

#Create test and train tables 
#dt[, (colsToDelete) := NULL]


Train <- select(dtTrain,c(1,2,3,4,5,11))
train <- select(dtTrain,c(1,2,3,4,5))
test <- select(dtTest,-1)
testId = dtTest$id

#seperate train and test by week

Trainw3 <-  subset(Train, Semana == 3)
trainw3 <-  subset(train, Semana == 3)
Testw4 <-  subset(Train, Semana == 4)
testw4 <-  subset(train, Semana == 4)

#Trainw3[, Semana := NULL]
#trainw3[, Semana := NULL]
#testw4[, Semana := NULL]
#Testw4[, Semana := NULL]

remove(dtTest,dtTrain, test,Train,train,testId,client,town,product)

library(randomForest)

set.seed(0)
rf = randomForest(Demanda_uni_equil ~ ., data = Trainw3, importance = TRUE,
                  ntree=25, nodesize = 100)
rf

varImpPlot(rf)

library(Metrics)

rfTrainPred <- predict(rf, newdata=trainw3)

rfTrainPred

rmsle(round(rfTrainPred), Trainw3$Demanda_uni_equil)
# 0.8747914 100 trees 100 nodes, semana not removed
# 0.8785682 25 trees 100 nodes, semana not removed

rfTestPred <- predict(rf, newdata=testw4)

rfTestPred

rmsle(round(rfTestPred), Testw4$Demanda_uni_equil)
#0.8833386 25 trees 100 nodes, semana not removed


#need feature engineering
head(town)

length(unique(town$State))
# 33 states

length(unique(town$Town))
# 260 towns

head(client)

length(unique(client$Cliente_ID))
# 930500 client ID's

length(unique(client$NombreCliente))
# 311155 Client names

length(unique(dtTrain$Cliente_ID))
# 880604 clients in train

head(product,10)

length(unique(product$NombreProducto))
# 2592 product names

length(unique(product$Producto_ID))
# 2592

#feature




setkeyv(dtTrain, c("Semana", 'Cliente_ID','Producto_ID'))

dtTrain[ , PurchaseCount := .N, by = list(Semana, Cliente_ID, Producto_ID)]

str(dtTrain)

library(data.table)
library(dplyr)
library(randomForest)
library(Metrics)

dtTrain = fread('train.csv', header=T)

Train <- select(dtTrain,c(1,2,3,4,5,11,12))
train <- select(dtTrain,c(1,2,3,4,5,12))

Trainw3 <-  subset(Train, Semana == 3)
trainw3 <-  subset(train, Semana == 3)
Testw4 <-  subset(Train, Semana == 4)
testw4 <-  subset(train, Semana == 4)

remove(dtTest,dtTrain, test,Train,train,testId,client,town,product)

set.seed(0)
rf = randomForest(Demanda_uni_equil ~ ., data = Trainw3, ntree=25, nodesize = 100, 
                  importance = TRUE)
varImpPlot(rf)



rfTrainPred <- predict(rf, newdata=trainw3)

rfTrainPred

rmsle(round(rfTrainPred), Trainw3$Demanda_uni_equil)
# 0.8747914 100 trees 100 nodes, semana not removed
# 0.8785682 25 trees 100 nodes, semana not removed

rfTestPred <- predict(rf, newdata=testw4)

rfTestPred

rmsle(round(rfTestPred), Testw4$Demanda_uni_equil)
#0.8833386 25 trees 100 nodes, semana not removed



