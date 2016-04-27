library(ggplot2)
library(dplyr)
library(pracma)
library(leaflet)
library(maps)
library(sp)
library(ggmap)
library(RColorBrewer)

#loaded the citibike data from 03/2015 - 03/2016 into cb_df
cb_df = read.csv("201601-citibike-tripdata.csv")

#merge all dataframes into one
#filenames <- list.files()
#winsum_df = do.call("rbind", lapply(filenames, read.csv, header = TRUE))

#function for distance taking into account the curvature of earth
cal_dist = function (lat1, lon1, lat2, lon2) {
  dlon = deg2rad(lon2-lon1)
  dlat = deg2rad(lat2-lat1)
  a = (sin(dlat/2))^2 + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) *(sin(dlon/2))^2
  c = 2 * atan2(a^(1/2),(1-a)^(1/2))
  return (3959*c)
}

#function for velocity
vel = function (dist, time) {
  return (dist/time)
}

#Categorizing into Morning, Afternoon, Evening
categorize = function (time) {
  if (time < 12) {
    return ("Morning")
  }
  if (time >= 12 & time <= 18) {
    return ("Afternoon")
  }
  if (time > 18) {
    return ("Evening")
  }
}

#mutated minutes, hours, days of duration and subseted out the duration >= 1 hr. into day_df
#jan_df = filter(cb_df, tripduration <= 2700)
#create hour and age columns
cb_df = mutate(cb_df, hour = tripduration/3600, age = 2015-birth.year)

#split starttime to date and time
start_df = data.frame(do.call('rbind', strsplit(as.character(cb_df$starttime),' ',fixed=TRUE)))
cb_df$startdate = start_df[,1]
cb_df$starthour = start_df[,2]
head(cb_df)

#create hour
temp_df = data.frame(do.call('rbind', strsplit(as.character(cb_df$starttime),':',fixed=TRUE)))
temporary = data.frame(do.call('rbind', strsplit(as.character(temp_df[,1]), ' ', fixed=TRUE)))
cb_df$timehour = as.numeric(temporary[,2])-1

#calculate distance
cb_df$distance = mapply(cal_dist, 
                         cb_df$start.station.latitude, 
                         cb_df$start.station.longitude, 
                         cb_df$end.station.latitude, 
                         cb_df$end.station.longitude)

#calculate velocity
cb_df$velocity = mapply(vel,
                         cb_df$distance,
                         cb_df$hour)

#Categorized by time of day
cb_df$timeofday = mapply(categorize,
                          cb_df$timehour)

#remove 0
main_df = filter(cb_df, velocity != 0)
jan_df = filter(main_df, tripduration <= 2700)

#leave 0
zero_df = filter(cb_df, velocity == 0 & tripduration <= 2700)

#Histogram for Ride Durations Under an Hour
time_hist = ggplot(cb_df, aes(x=velocity))
time_hist + geom_histogram(binwidth = .1, fill="darkblue") + 
  ggtitle("Velocity") +
  scale_fill_brewer(palette = "Blues") +
  xlab("Velocity (mph)") + 
  ylab("Frequency")
mean(cb_df$velocity)

#velocity everyday
everyday = ggplot(jan_df, aes(x=startdate, y=mean(velocity)))
everyday + geom_bar(stat="identity", fill = "darkblue") +
  labs(title = "Average Velocity per Day", x = "Date", y = "Average Velocity") +
  coord_flip()

#LineGraph for zero_velocity and time of day
vel_hist = ggplot(zero_df, aes(x=timehour, y=hour))
vel_hist + 
  geom_smooth(aes(color = "red"), se=FALSE) +
  ggtitle("Zero-Velocity")+
  ylab("Duration (hr)") +
  xlab("Time (24Hr)") +
  theme_bw() +
  guides(color = "none")

#Age Line Graph
  age_hist = ggplot(jan_df, aes(x=age, y=velocity))
  age_hist + geom_smooth(se = FALSE) +
    ggtitle("Age and Velocity")

#map of gender vs. velocity
gender_hist = ggplot(jan_df, aes(x=hour, y=velocity))
gender_hist + geom_v() +
  ggtitle("Gender and Velocity")

#UserType Histogram
subscriber = ggplot(jan_df, aes(x=velocity))
subscriber + geom_histogram(binwidth = .1, aes(fill=jan_df$usertype)) +
  ggtitle("UserType and Velocity (duration <= 45 min)") +
  ylab("Frequency") +
  scale_fill_manual(values = c("red", "darkblue"), name = "UserType") +
  theme_light()

#map for stations
temp = jan_df[1:100,]
leaflet(temp) %>%
setView(lng = -73.98928, lat = 40.75042, zoom = 13) %>%
addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(
    lng=temp$start.station.longitude,
    lat=temp$start.station.latitude,
    radius = 5,
    stroke=FALSE, # Circle stroke
    fillOpacity=0.5
    )

#map for zero velocity
leaflet(zero_df) %>%
  setView(lng = -73.98928, lat = 40.75042, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(
    lng=zero_df$start.station.longitude,
    lat=zero_df$start.station.latitude,
    radius = 1,
    stroke=FALSE, # Circle stroke
    fillOpacity=0.5
  )