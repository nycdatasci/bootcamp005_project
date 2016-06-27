library(data.table)
setwd("~/GitHub-kszela24/Faceback/Szela")

train = read.csv("train.csv")

train_less = train[, c(2, 3, 4, 6)]
train_less = train_less[order(train_less$place_id), ]
#train_less_temp = train_less
#train_less = train_less_temp
ret_frame = data.frame(place_id = numeric(), 
                       x_center = numeric(),
                       y_center = numeric(),
                       x_std = numeric(),
                       y_std = numeric(),
                       acc_avg = numeric(),
                       max_x_distance = numeric(),
                       max_y_distance = numeric())

row_list = c(1)
curr_place_id = train_less[1, 4]
for (i in 2:length(train_less$place_id)) {
  if (i %% 1000 == 0) {
    print(i)
  }
  if (curr_place_id == train_less[i, 4]) {
    row_list = append(row_list, c(i))
  } else {
    curr_place = train_less[row_list,]
    new_row = data.frame(place_id = curr_place_id, 
                         x_center = mean(curr_place$x),
                         y_center = mean(curr_place$y),
                         x_std = sd(curr_place$x),
                         y_std = sd(curr_place$y),
                         acc_avg = mean(curr_place$accuracy),
                         max_x_distance = max(abs(mean(curr_place$x) - curr_place$x)),
                         max_y_distance = max(abs(mean(curr_place$y) - curr_place$y)))
    ret_frame = rbind(ret_frame, new_row)
    row_list = c(i)
    curr_place_id = train_less[i, 4]
  }
}

curr_place = train_less[train_less$place_id == 9999932225,]
new_row = data.frame(place_id = 9999932225, 
                     x_center = mean(curr_place$x),
                     y_center = mean(curr_place$y),
                     x_std = sd(curr_place$x),
                     y_std = sd(curr_place$y),
                     acc_avg = mean(curr_place$accuracy),
                     max_x_distance = max(abs(mean(curr_place$x) - curr_place$x)),
                     max_y_distance = max(abs(mean(curr_place$y) - curr_place$y)))
ret_frame = rbind(ret_frame, new_row)


write.csv(ret_frame, "centroid_positions_na_0.csv", row.names = F, na = "0")