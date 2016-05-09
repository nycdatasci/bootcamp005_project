# preparing data for Shiny app

load("/Users/adamcone/Desktop/projects/Shiny Project/data munging/op_data_conversion_filtered.RData")
rm(sales_tbl)
items_tbl = select(items_tbl, DateTime, Payment_Type, Unit_Cost, Item_Total)
items_tbl = mutate(items_tbl, Date = as.Date(DateTime))

first_date = as.Date(min(items_tbl$DateTime))
last_date = as.Date(max(items_tbl$DateTime))
break_points = seq(from = first_date,
                   to = last_date,
                   length.out = 21)

# Now I'll add this column to the dataframe
Date_Bin_vector = rep(0, length(items_tbl$Date))
Date_Bin_vector[1:100] = 1
for (i in 1:20) {
  Date_Bin_vector = Date_Bin_vector +
                    (items_tbl$Date > break_points[i] &
                     items_tbl$Date <= break_points[i+1]) *
                     i
}
items_tbl = mutate(items_tbl, Date_Bin = Date_Bin_vector)
rm(Date_Bin_vector)

# Now, I'll try to generate a new column with the day of the week for each item

dow_converter = function(Date) {
  ymd = as.integer(unlist(strsplit(as.character(Date), "-")))
  year = ymd[1]
  month = ymd[2]
  day = ymd[3]
  return(day.of.week(month, day, year))
}

dow_df = data.frame(Date = unique(items_tbl$Date))
dow_df = mutate(dow_df, Day_of_Week_index = sapply(Date, dow_converter))
dow_name_df = data.frame(Day_of_Week_index = 0:6,
                         Day_of_Week = c("Sunday",
                                         "Monday",
                                         "Tuesday",
                                         "Wednesday",
                                         "Thursday",
                                         "Friday",
                                         "Saturday")
                         )
dow_df = inner_join(dow_df,
                    dow_name_df,
                    by = "Day_of_Week_index")
dow_df = select(dow_df, -Day_of_Week_index)

items_tbl = inner_join(items_tbl, dow_df, by = "Date")
rm(dow_df, break_points, i, dow_converter, dow_name_df)

# Now, I want a seperate column for time
DT_vec1 = format(as.POSIXlt(items_tbl$DateTime,
                            tz = "UTC"),
                 "%Y%m%d %H:%M:%S"
                 )
DT_vec2 = strsplit(DT_vec1, " ")
DT_vec3 = unlist(DT_vec2)
Time_vec = rep("", length(items_tbl$DateTime))
Date_vec = rep("", length(items_tbl$DateTime))
for (i in 1:length(DT_vec3)) {
  if (i %% 2 == 0) {
    Time_vec[i/2] = DT_vec3[i]
  } else {
    Date_vec[(i-1)/2 + 1] = DT_vec3[i]
  }
}
Time_component_vec = strsplit(Time_vec, ":")
Time_component_vec = unlist(Time_component_vec)
Hours_vec = rep(0,length(items_tbl$DateTime))
Minutes_vec = rep(0,length(items_tbl$DateTime))
Seconds_vec = rep(0,length(items_tbl$DateTime))
for (i in 1:length(Time_component_vec)) {
  if (i %% 3 == 1) {
    Hours_vec[(i-1)/3 + 1] = Time_component_vec[i]
  } else if (i %% 3 == 2) {
    Minutes_vec[(i-2)/3 + 1] = Time_component_vec[i]
  } else {
    Seconds_vec[i/3] = Time_component_vec[i]
  }
}
items_tbl = mutate(items_tbl,
                   Hour = as.numeric(Hours_vec),
                   Minute = as.numeric(Minutes_vec),
                   Second = as.numeric(Seconds_vec)
                   )

rm(Date_vec, DT_vec1, DT_vec2, DT_vec3, first_date, Hours_vec, i, last_date,
   Minutes_vec, Seconds_vec, Time_component_vec, Time_vec)
hour_bin_df = data.frame(Hour = 0:23,
                         Time_Bin = c(3,3,
                                      0,0,0,0,0,0,
                                      1,1,1,1,1,1,
                                      2,2,2,2,2,2,
                                      3,3,3,3)
                         )
items_tbl = inner_join(items_tbl, hour_bin_df, by = "Hour")
rm(hour_bin_df)