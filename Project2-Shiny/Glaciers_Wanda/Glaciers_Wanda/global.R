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
setwd("/Users/wandawang/Desktop/Glaciers_Wanda")
load('./Project.RData')  #run this to work. this loads up NE.Gr already
cbPalette <- c("#000000", "#000000", "#CC79A7", "#D55E00", "#D55E00", "#D55E00", "#D55E00")
               #2008,        2009,      2010,      2011,      2012,       2013,       2014
###fix this... 
gen.data <- select(latlong, NAME, LATITUDE, LONGITUDE) 
gen.data <- na.omit(gen.data) 

NE.Gr.new <- id.data %>% filter(., YEAR == 2001:2014)
