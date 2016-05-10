library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(ggthemes)
library(DT)
library(shinythemes)

#NYPD <- read.csv("NYPD_7_Major_Felony_Incidents.csv", stringsAsFactors=FALSE)
NYPD <- readRDS("NYPD")
NYPD_cleaned = NYPD[,-(3:8)] #I cleaned occurence columns because CompStat data seemed more accurate

NYPD_for_map = NYPD_cleaned[,-(1:2)]

NYPD_for_map = NYPD_for_map[-1123465,]
NYPD_for_map$Borough[NYPD_for_map$Borough == ""] <- NA
NYPD_for_map <- na.omit(NYPD_for_map)
NYPD_for_map = NYPD_for_map[-which(NYPD_for_map$Borough == "(null)"), ]

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


NYPD_for_map = within(NYPD_for_map, Date <- paste(CompStat.Year,CompStat.Month,CompStat.Day,sep='-'))
NYPD_for_map$Date = as.Date(NYPD_for_map$Date)

pal = colorFactor(rainbow(7, alpha = 1), domain = NYPD_for_map$Offense)

NYPD_for_map = sample_n(NYPD_for_map, 500000, replace = FALSE) ##changing from this line NYPD_sample to NYPD_for_map

Offense_Borough = as.data.frame(table(NYPD_for_map$Offense, NYPD_for_map$Borough, NYPD_for_map$CompStat.Year))
colnames(Offense_Borough) = c("Offense", "Borough", "Year", "Count")

Population = data.frame(c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND") ,c(1455444, 2636735, 1644518, 2339150, 474558))
colnames(Population) = c("Borough", "Population")

Offense_Borough = left_join(Offense_Borough, Population, by = "Borough")
Offense_Borough = mutate(Offense_Borough, Normalized = (Count / Population))

saveRDS(NYPD, "NYPD")















  
