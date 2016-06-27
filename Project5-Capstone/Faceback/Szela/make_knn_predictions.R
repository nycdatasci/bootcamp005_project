library(kknn)
library(class)
library(data.table)
setwd("~/GitHub-kszela24/Faceback/Szela")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

for (i in 0:47) {
  for (p in -1:1) {
    c = 0
    if (i + p < 0) {
      c = 47
    } else if (i + p > 47) {
      c = 0
    } else {
      c = i + p
    }
    for (j in 0:6) {
      print(j)
      print(i)
      for (q in 0:6) {
        train_file = paste0("./partitioned_train/train_day_", q, "_hour_", c, ".csv")
        train = data.frame(read.csv(train_file))
        test_file = paste0("./partitioned_test/test_day_", j, "_hour_", i, ".csv")
        test = data.frame(read.csv(test_file))
        
        train$x_scaled = range01(train$x)
        train$y_scaled = range01(train$y)
        test$x_scaled = range01(test$x)
        test$y_scaled = range01(test$y)
        
        knn_fit = knn(train[,c(9, 10)], test[,c(8, 9)], train[, 6], k = 1, l = 0, prob = FALSE, use.all = TRUE)
        
        submission = data.frame(row_id = test$row_id, place_id = knn_fit)
        
        pred_file = paste0("./test_predictions/test_day_", j, "/test_hour_", i, "/predictions_from_train_day_", q, "_train_hour_", c, ".csv")
        write.csv(submission, pred_file, row.names = F)
      }
    }
  }
}

#Create filesystem!
for (j in 0:6) {
  pred_file = paste0("./test_predictions/test_day_", j)
  dir.create(pred_file)
  for (i in 0:47) {
    pred_file = paste0("./test_predictions/test_day_", j, "/test_hour_", i, "/")
    dir.create(pred_file)
  }
}

prediction_total = data.frame()

for (i in 0:47) {
  print(i)
  for (j in 0:6) {
    print(j)
    for (p in -1:1) {
      c = 0
      if (i + p < 0) {
        c = 47
      } else if (i + p > 47) {
        c = 0
      } else {
        c = i + p
      }
      
      #Need to get rowId's only once.
      if (p == -1) {
        
      }
      
      for (q in 0:6) {
        
      }
    }
  }
}

pred_file = paste0("./test_predictions/test_day_", 0, "/test_hour_", 0, "/predictions_from_train_day_", 0, "_train_hour_", 0, ".csv")
preds = read.csv(pred_file)
