library(data.table)
setwd("~/GitHub-kszela24/Faceback/Szela")

train = read.csv("centroid_positions_na_0.csv")
train_real = read.csv("train.csv")
test = read.csv("test.csv")
test_read = test

set.seed(8)
idx = sample(1:29118020, size = 4000)

test = train_real[idx,]



x_eps = 1
y_eps = 0.01

predictions = data.frame(row_id = test$row_id,
                         pred1 = 1:length(test$row_id),
                         pred2 = 1:length(test$row_id),
                         pred3 = 1:length(test$row_id))
for (i in 1:length(test[,1])) {
  if (i %% 100 == 0) {
    print(i)
  }
  curr_row = test[i,]
  box_x_max = (curr_row$x) + x_eps
  box_x_min = (curr_row$x) - x_eps
  box_y_max = (curr_row$y) + y_eps
  box_y_min = (curr_row$y) - y_eps
  within_box = train[train$x_center <= box_x_max & train$x_center >= box_x_min & 
                       train$y_center <= box_y_max & train$y_center >= box_y_min,]
  
  curr_x = curr_row$x
  curr_y = curr_row$y

  curr_dist = sqrt(((within_box[, 2] - curr_x)^2) + ((within_box[, 3] - curr_y)^2))
  within_box$dist = curr_dist
  within_box = within_box[order(within_box$dist),]

  predictions[i, 2] = within_box[1, 1]
  predictions[i, 3] = within_box[2, 1]
  predictions[i, 4] = within_box[3, 1]
}


count = 1
for (q in 1:length(idx)) {
  if (predictions[q, 2] == test[q, 6] || predictions[q, 3] == test[q, 6] || predictions[q, 4] == test[q, 6]) {
    count = count + 1
  }
}
print((count/length(idx))*100)
