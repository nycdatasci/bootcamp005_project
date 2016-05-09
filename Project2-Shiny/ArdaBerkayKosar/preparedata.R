library(dplyr)
library(ggplot2)
library(ggthemes)
library(leaflet)

NYPD <- read.csv("~/Downloads/NYPD_7_Major_Felony_Incidents.csv", stringsAsFactors=FALSE)


#-----------------SEPERATING LONGITUDE AND LATITUDE DATA------------------#
NYPD_cleaned = NYPD[,-(3:8)] #I cleaned occurence columns because CompStat data seemed more accurate

NYPD_for_map = NYPD_cleaned[,-(1:2)]

NYPD_for_map = NYPD_for_map[-1123465,]
NYPD_for_map$Location.1 = strsplit(NYPD_for_map$Location.1, "\\,", " ")

Longitude = sapply(NYPD_for_map$Location.1, "[[", 2)
NYPD_for_map$Longitude = Longitude
NYPD_for_map$Longitude = gsub('\\)', '', NYPD_for_map$Longitude)


Latitude = sapply(NYPD_for_map$Location.1, "[[", 1)
NYPD_for_map$Latitude = Latitude
NYPD_for_map$Latitude = gsub('\\(', '', NYPD_for_map$Latitude)


options(digits = 20)
NYPD_for_map$Longitude = as.numeric(NYPD_for_map$Longitude)
NYPD_for_map$Latitude = as.numeric(NYPD_for_map$Latitude)












#------------------CLEANINNG THE MISSING DATA----------------#

NYPD_for_map$Borough[NYPD_for_map$Borough == ""] <- NA
NYPD_for_map <- na.omit(NYPD_for_map)
NYPD_for_map = NYPD_for_map[-which(NYPD_for_map$Borough == "(null)"), ]

NYPD_for_map = within(NYPD_for_map, Date <- paste(CompStat.Year,CompStat.Month,CompStat.Day,sep='-'))
NYPD_for_map$Date = as.Date(NYPD_for_map$Date)











                                                  
