library(maptools)
library(ggplot2)
library(ggmap)
library(rgeos)
library(dplyr)
library(stringi)
library(sp)
library(googleVis)
library(shiny)
library(shinydashboard)
library(leaflet)
#setwd("./Glaciers_Wanda")
load('./Project.RData')  #run this to work 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
               #2008,        2009,      2010,      2011,      2012,       2013,       2014
###fix this... 
gen.data <- select(latlong, NAME, LATITUDE, LONGITUDE) 
gen.data <- na.omit(gen.data) 

