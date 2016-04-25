library(ggplot2)
library(dplyr)
library(pracma)
#loaded the citibike data from 03/2015 - 03/2016 into cb_df
#cb_0216 = read.csv("201602-citibike-tripdata.csv")
cb_df = read.csv("201601-citibike-tripdata.csv")
#cb_1215 = read.csv("201512-citibike-tripdata.csv")
#cb_1115 = read.csv("201511-citibike-tripdata.csv")
#cb_1015 = read.csv("201510-citibike-tripdata.csv")
#cb_0915 = read.csv("201509-citibike-tripdata.csv")
#cb_0815 = read.csv("201508-citibike-tripdata.csv")
#cb_0715 = read.csv("201507-citibike-tripdata.csv")
#cb_0615 = read.csv("201506-citibike-tripdata.csv")
#cb_0515 = read.csv("201505-citibike-tripdata.csv")
#cb_0415 = read.csv("201504-citibike-tripdata.csv")
#cb_0315 = read.csv("201503-citibike-tripdata.csv")

#merge all dataframes into one
'''filenames <- list.files()
winsum_df = do.call("rbind", lapply(filenames, read.csv, header = TRUE))'''

#mutated minutes, hours, days of duration and subseted out the duration >= 1 hr. into day_df
jan_df = filter(cb_df, tripduration <= 2700)


#Histogram for Ride Durations Under an Hour
time_hist = ggplot(jan_df, aes(x=tripduration))
time_hist + geom_histogram(binwidth = 10) + 
  ggtitle("Ride Durations Under 45 Min") +
  scale_fill_brewer(palette = "Blues") +
  xlab("Trip Duration (sec)") + 
  ylab("Frequency") +
  theme_bw()

#split starttime to date and time
start_df = data.frame(do.call('rbind', strsplit(as.character(jan_df$starttime),' ',fixed=TRUE)))
jan_df$startdate = start_df[,1]
jan_df$starthour = start_df[,2]
head(jan_df)


#calculate distance taking into account the curvature of earth
cal_dist = function (lat1, lon1, lat2, lon2) {
  dlon = deg2rad(lon2-lon1)
  dlat = deg2rad(lat2-lat1)
  a = (sin(dlat/2))^2 + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) *(sin(dlon/2))^2
  c = 2 * atan2(a^(1/2),(1-a)^(1/2))
  return (6373*c)
}

#calculate velocity
vel = function (dist, time) {
  return (dist/time)
}

temp = jan_df[1:3,]
head(temp)
jan_dist = mutate(temp, Distance = sapply(temp$start.station.latitude, FUN = cal_dist, lon1 = temp$start.station.longitude, lat2 = temp$end.station.latitude, lon2 = temp$end.station.longitude))
names(jan_df)

#Count of BikeID
summarise(group_by(cb_df, bikeid), Count = n())