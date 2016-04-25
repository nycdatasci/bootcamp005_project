setwd("/Users/drscholls303/Desktop/Project1")
# Load the required R packages and specified datasets
library(dplyr)
library(ggplot2)
faostat.palm <-read.csv("Faostats.csv")
faostat.palm <- faostat.palm %>% na.omit()
#Select relevant columns
producers <-select(faostat.palm, AreaName, ElementName, Value, Year)
# Filter for top producers in most recent available year of 2014
producers.year<-filter(producers, Year == 2014) %>% 
  group_by(AreaName, ElementName='Production') %>% 
  summarise(., Production=sum(Value)) 
nrow(producers.year) #46 rows 
#Calculate percentage of world production for each country
producers.year$Percent = round(producers.year$Production[1:46]*100/
                                 sum(producers.year$Production), digits = 2)
producers.year<-tbl_df(producers.year)
top<-top_n(producers.year,5,Production)
top
#top 5 graph
library(scales)
top.plot<-ggplot(top, aes(AreaName, Production, fill=Percent)) + geom_bar(stat="identity") + xlab("Country") + ylab("Oil Production") +
  ggtitle("Palm Oil Production by Country (Top 5)")

firstplot<- top.plot + 
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", math_format(10^.x)))
firstplot

library(maptools)
library(ggplot2)
library(ggmap)
library(rgeos)
library(dplyr)
plotmap <- readShapePoly(fn="TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp")
palmHa <-read.csv("Faostats.csv", stringsAsFactors=F)
palmHa <-filter(palmHa, ElementName == 'Area harvested')
names(plotmap)[5] <- "AreaName"
target <- c("Indonesia", "Malaysia", "Colombia", "Thailand", 
            "Philippines", "Nigeria", "Cameroon", "Ecuador", "Ghana", "Honduras","Guatemala")
#Use foritfy to transform spatial plotmap into dataframe
plotmapDf <- fortify(plotmap, region = "AreaName") %>% 
  filter(id %in% target)
palmHaMapDf <- merge(plotmapDf, palmHa, by.x="id", by.y="AreaName")
backgroundMap <- ggplot(data=palmHaMapDf) + geom_path(aes(x=long, y=lat, group=group), color='grey') + coord_equal() + geom_polygon(aes(x=long, y=lat, group=group, fill=Value))
mapAsia <- get_map(location = 'Indonesia', zoom=4)
mapAfrica <- get_map(location = 'Africa', zoom=4)
mapSouthAm <- get_map(location = 'South America', zoom=4)

ggmapObjAsia <- ggmap(mapAsia)
Asia <- ggmapObjAsia + geom_polygon(aes(x=long, y=lat, group=group, fill=Value), data=palmHaMapDf, alpha=.9) + 
  geom_path(aes(x=long, y=lat, group=group), data=palmHaMapDf, color='black')
Asia

ggmapObjAf <- ggmap(mapAfrica)
Africa <- ggmapObjAf + geom_polygon(aes(x=long, y=lat, group=group, fill=Value), data=palmHaMapDf, alpha=.9) + 
  geom_path(aes(x=long, y=lat, group=group), data=palmHaMapDf, color='black')
Africa

ggmapSouthAm <- ggmap(mapSouthAm)
SouthAm <- ggmapSouthAm + geom_polygon(aes(x=long, y=lat, group=group, fill=Value), data=palmHaMapDf, alpha=.9) + 
  geom_path(aes(x=long, y=lat, group=group), data=palmHaMapDf, color='black')
SouthAm

library(dplyr)
library(ggplot2)
faostat.palm <-read.csv("Faostats.csv")
faostat.palm <- faostat.palm %>% na.omit()
AH<-filter(faostat.palm, ElementName=='Area harvested', AreaName=='Indonesia')

areaGraph<-ggplot(AH, aes(Year, Value, fill=AreaName)) + geom_bar(stat="identity") +
  ylab("Area Harvested") + ggtitle("Area Harvested through Time(Indonesia)")
areaGraph

library(dplyr)
library(leaflet)
mills<-read.csv("mills.csv")
fires<-read.csv("fires.csv")
last.year.fires <-filter(fires, OBJECTID > 11556)#fire-alerts, as of 1/01/2015
#zoom in on SE asia, Circles for mills, Markers for fire alerts
map <- leaflet(mills) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% addCircles(~longitude, ~latitude, popup=mills$type, weight = 3, radius=40, color="#ffa500", stroke = TRUE, fillOpacity = 0.8) %>%
  addMarkers(data = last.year.fires, ~Latitude, ~Longitude, clusterOptions = markerClusterOptions()) %>% 
  addLegend("bottomright", colors= "#ffa500", labels="Mills", title="Legend") 
map

