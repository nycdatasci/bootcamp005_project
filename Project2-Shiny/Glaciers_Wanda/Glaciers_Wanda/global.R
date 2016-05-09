library(maptools)
library(ggplot2)
library(ggmap)
library(rgeos)
library(dplyr)
library(stringi)
library(sp)
library(ggvis)
library(googleVis)
setwd("/Users/wandawang/Desktop/glacierz")

mbal<-read.csv("WGMS-FoG-2015-11-EE-MASS-BALANCE.csv", stringsAsFactors = F) # want the Mass Balance
latlong <-read.csv("WGMS-FoG-2015-11-A-GENERAL-INFORMATION.csv")
id.data <- merge(mbal, latlong, by = "WGMS_ID") 
###years 

#Alaska <-id.data %>% filter(., GEN_LOCATION == "ALASKA RANGE") %>% select(.,NAME.x, YEAR, 
                                         # ANNUAL_BALANCE, LATITUDE, LONGITUDE)

#GULKANA <- Al.Gulkana %>% filter(., NAME.x == "GULKANA") %>% mutate(., CUMULATIVE_BAL = cumsum(ANNUAL_BALANCE))

NE.Gr <- id.data %>% filter(., YEAR == 2008:2014) %>% select(., NAME.x, YEAR, ANNUAL_BALANCE, 
GEN_LOCATION, SPEC_LOCATION, LATITUDE, LONGITUDE,GEO.REGION_CODE)

#isolate FREYA glacier for analysis 
FREYA <- NE.Gr %>% filter(.,GEN_LOCATION == "NE GREENLAND") %>% 
  mutate(., CUMULATIVE_BAL = cumsum(ANNUAL_BALANCE))

#year issue

Line <- gvisLineChart(FREYA, xvar ="YEAR", yvar=c("ANNUAL_BALANCE", "CUMULATIVE_BAL"),
                      options=list(title="Freya Glacier",
                                   titleTextStyle="{color:'blue',fontName:'Courier',fontSize:16}"))  
###### need to edit line chart
#checkox

#Line1 <- gvisLineChart(FREYA, xvar ="YEAR",
 #                     yvar="CUMULATIVE_BAL")

glacP <- readShapePoly(fn = "glims_download_28564/glims_polygons.shp") ##unable to use dplyr on spatial data frames
glacP <- gBuffer(glacP, byid=TRUE, width=0)
glac.NE  <- glacP[glacP$geog_area == "Northeast Greenland",]

glac.choice <- as.data.frame("Northeast Greenland", "Alaska")
maap <- ggplot2::fortify(glac.NE, region = "glac_name")

maap$id = "NE GREENLAND"
MapDf <- merge(maap, NE.Gr, by.x="id", by.y="GEN_LOCATION") 

MapDf %>% ggvis(~long, ~lat) %>% group_by(group, id) %>% layer_paths(strokeOpacity:=0.5, 
stroke:="#7f7f7f") %>% hide_legend("fill") %>% hide_axis("x") %>% hide_axis("y")%>%  set_options(width=400, height=600, keep_aspect=TRUE)

############## refer to server.R
MapDf$colour <- ifelse(MapDf$id == 'NE GREENLAND', 'blue') 
# need to scale it by year tho, sliderinput ?

Glacier<-MapDf %>% group_by(group, id)%>%
  ggvis(~long, ~lat) %>%
  layer_paths(fill:= ~colour)
################
