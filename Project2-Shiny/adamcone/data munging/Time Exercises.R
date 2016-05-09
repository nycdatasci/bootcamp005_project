dts <- data.frame(day = c("20081101", "20081101", "20081101", "20081101",
                          "20081101", "20081102", "20081102", "20081102",
                          "20081102", "20081103"),
                  time = c("01:20:00", "06:00:00", "12:20:00", "17:30:00",
                           "21:45:00", "01:15:00", "06:30:00", "12:50:00",
                           "20:00:00", "01:05:00"),
                  value = c("5","5", "6", "6", "5", "5", "6", "7", "5", "5")
                  )

dts1 <- paste(dts$day, dts$time)
dts2 <- as.POSIXct(dts1, format = "%Y%m%d %H:%M:%S")
dts3 <- as.POSIXlt(dts1, format = "%Y%m%d %H:%M:%S")
dts.all <- data.frame(dts, ct = dts2, lt = dts3)
str(dts.all)

dts.all <- dts
dts.all$ct <- dts2
dts.all$lt <- dts3
str(dts.all)

unclass(dts.all$ct)