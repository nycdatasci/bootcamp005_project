library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(ggthemes)
library(DT)
library(shinythemes)



pal = colorFactor(rainbow(7, alpha = 1), domain = NYPD_sample$Offense)

NYPD_sample = sample_n(NYPD_for_map, 200000, replace = FALSE)

Offense_Borough = as.data.frame(table(NYPD_sample$Offense, NYPD_sample$Borough, NYPD_sample$CompStat.Year))
colnames(Offense_Borough) = c("Offense", "Borough", "Year", "Count")

Population = data.frame(c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND") ,c(1455444, 2636735, 1644518, 2339150, 474558))
colnames(Population) = c("Borough", "Population")

Offense_Borough = left_join(Offense_Borough, Population, by = "Borough")
Offense_Borough = mutate(Offense_Borough, Normalized = (Count / Population))














  
