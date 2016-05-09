#global.R
library(shiny)
library(dplyr) #used to mutate the df
#library(plyr)
#options(java.parameters = '-Xmx5g')
#library(XLConnect)
#library(xlsx) #used to read the data file
library(lubridate) #used for dates
#library(openxlsx)
library(wordcloud)
library(leaflet)


setwd("/Users/AlexandKelly/rkelly")
#x=read.xlsx2("311.xlsx", 1)
x=read.csv("311new.csv")

noise2=select(x, Created.Date, Closed.Date, Complaint.Type,
              Descriptor, Location.Type, Incident.Zip, City, Borough,
              X.Coordinate..State.Plane., Y.Coordinate..State.Plane.,
              Latitude, Longitude, Unique.Key)

#z=read.xlsx2("neighborhoods_byzip_edit.xlsx", 1)
#z=mutate(z, Incident.Zip=ZIP.Codes)
#noise=join(noise2, z, by="Incident.Zip", type="left")

#df=noise

df=noise2

#df=mutate(df, start.date=as.Date(as.numeric(as.character(
#        df$Created.Date)), origin="1900-01-01"),
#        end.date=as.Date(as.numeric(as.character(
#                df$Closed.Date)), origin="1900-01-01"),
#        ANNUAL=year(start.date), end.yr=year(end.date), start.mo=month(start.date),
#        end.mo=month(end.date), MONTH=month(start.date, label=TRUE, abbr=TRUE),
#        end.MMM=month(end.date, label=TRUE, abbr=TRUE), DAY=day(start.date),
#        end.day=day(end.date))


df=mutate(df, start.date=as.Date(Created.Date, "%m/%d/%Y", origin="01/01/1900"),
          end.date=as.Date(Closed.Date, "%m/%d/%Y", origin="01/01/1900"),
          ANNUAL=year(start.date),
          end.yr=year(end.date),
          start.mo=month(start.date),
          end.mo=month(end.date),
          MONTH=month(start.date, label=TRUE, abbr=TRUE),
          end.MMM=month(end.date, label=TRUE, abbr=TRUE),
          DAY=day(start.date),
          end.day=day(end.date)
          )




df2=group_by(df, Borough, Complaint.Type, MONTH, DAY, ANNUAL, Longitude, Latitude)

#ggplot(data = df2, aes(x = Borough)) + geom_bar()

#y=select(filter(df2, Borough=="BROOKLYN"), Complaint.Type, Longitude, Latitude)

#y=select(filter(df2, Borough=="BRONX"), Complaint.Type, ANNUAL, MONTH, DAY, Longitude, Latitude)

#leaflet() %>%
#        setView(-73.94197, 40.73638, zoom = 11) %>% 
#        addTiles() %>%  # Add default OpenStreetMap map tiles
#        addMarkers(lng=na.omit(y$Longitude), lat=na.omit(y$Latitude), popup=y$Complaint.Type)

#ggplot(data = y, aes(x= MONTH, fill=MONTH)) + geom_bar(position="dodge") +facet_grid(~Borough)

#ggplot(df1,aes(x=y2009,fill=Type)) + geom_bar(position="dodge")+facet_grid(~Loc)

#y=select(filter(df2, Borough=="QUEENS"), Complaint.Type, Longitude, Latitude, MONTH, ANNUAL, DAY)


#library(ggmap)
#library(sp)
#library(rgdal)
#coordinates(na.omit(pts))=~Longitude+Latitude
#proj4string(pts)=CRS("+init=epsg:4326") # set it to lat-long
#pts = spTransform(pts,CRS("insert your proj4 string here"))



#pal2 <- brewer.pal(8,"Dark2")
#png("wordcloud_packages.png", width=1280,height=800)
#wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
#          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
