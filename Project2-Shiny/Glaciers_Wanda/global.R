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
###
NE.Gr <- id.data %>% filter(., YEAR == 2008:2014) %>% select(., NAME.x, YEAR, ANNUAL_BALANCE, 
GEN_LOCATION, SPEC_LOCATION, LATITUDE, LONGITUDE,GEO.REGION_CODE)
FREYA <- NE.Gr %>% filter(.,GEN_LOCATION == "NE GREENLAND") %>% 
  mutate(., CUMULATIVE_BAL = cumsum(ANNUAL_BALANCE))

Line <- gvisLineChart(FREYA)
#plot <-plot(Line) ##Time Series

glacP <- readShapePoly(fn = "glims_download_28564/glims_polygons.shp")
glacP <- gBuffer(glacP, byid=TRUE, width=0)
glac.NE  <- glacP[glacP$geog_area == "Northeast Greenland",]
glac.choice <- as.data.frame(glac.NE$geog_area[1])
#glac.choice.all <- as.data.frame(glacP$geog_area) , distinct 
#Error in as.list.default(data) : 
#no method for coercing this S4 class to a vector
#MAP 
maap <- ggplot2::fortify(glac.NE, region = "glac_name")
#maap<-maap %>% ggvis(~long, ~lat) %>% # turning maap in ggvis conflict 
  #group_by(group, id) %>% 
  #layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f") %>% 
  #hide_legend("fill") %>% hide_axis("x") %>% hide_axis("y") %>%  
  #set_options(width=400, height=600, keep_aspect=TRUE)
maap$id = "NE GREENLAND"
MapDf <- merge(maap, NE.Gr, by.x="id", by.y="GEN_LOCATION") #done

MapDf %>% ggvis(~long, ~lat) %>% group_by(group, id) %>% layer_paths(strokeOpacity:=0.5, 
           stroke:="#7f7f7f") %>% hide_legend("fill") %>% hide_axis("x") %>% hide_axis("y") %>%  set_options(width=400, height=600, keep_aspect=TRUE)
#View(MapDf)
MapDf$colour <- ifelse(MapDf$id == 'NE GREENLAND', 'blue') # need to scale it by year tho, input?
Glacier<-MapDf %>% group_by(group, id)%>%
  ggvis(~long, ~lat) %>%
  layer_paths(fill:= ~colour)

#Connect a ggvis graphic to a shiny app
#state_stat <- data.frame(state.name = rownames(state.x77), state.x77)
#choice <- colnames(state_stat)[-1]
