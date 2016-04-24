###########################
#####REQUIRED PACKAGES#####
###########################

library (dplyr)
library (ggplot2)
library (lubridate)
library (ggthemes)
library (chron)
library(devtools)
library (choroplethrZip)
library(choroplethr)
library (googleVis)
library(zipcode)


by_borough = group_by(NYPD_Data, BOROUGH) #Grouped by Borough to analyse number of death according to the
                                          #related Borough
by_borough_2 = filter(by_borough, BOROUGH != '') #I excluded the empty strings in the BOROUGH column
by_borough_2 = group_by(by_borough_2, BOROUGH)   #grouped by BOROUGH

by_borough_2$DATE = as.Date(by_borough_2$DATE, "%m/%d/%Y") #I changed the class type of my DATE column to 'date' from
                                                           #'character' so that I can access to the year, month and day
                                                           #data seperately
by_borough_2 = mutate(by_borough_2, Year = year(DATE))

by_year = mutate(by_year, Year = year(DATE))
by_year = group_by(by_year, Year)
summarise(by_year, Total_Pedestrians_Killed = sum(NUMBER.OF.PEDESTRIANS.KILLED))



##############################################
#####BOROUGH ANALYSIS BY NUMBER OF DEATHS#####
##############################################


Borough_Summary_Year = group_by(by_year, BOROUGH, Year)

Borough_Deaths_Years = summarise(Borough_Summary_Year, total_people_killed = sum(NUMBER.OF.PERSONS.KILLED))



#############TOTAL PEOPLE KILLED IN COLLISIONS BY BOROUGH FOR EACH YEAR#############


Total_Deaths_by_Years = ggplot(data = Borough_Deaths_Years, aes(x = Year, y = total_people_killed)) + 
                        geom_bar(aes(fill = BOROUGH), stat = 'identity') + 
                        theme_bw() + 
                        theme(legend.position = "right") +
                        theme(legend.text=element_text(size=5)) +
                        ggtitle('Total People Killed in Collisions by Year') + 
                        ylab("Number of Total Deaths") +
                        xlab("New York City Boroughs") +
                        theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
                        theme(legend.position = "right") +
                        theme(legend.text=element_text(size=10)) 
Total_Deaths_by_Years



#############TOTAL PEDESTRIANS KILLED IN COLLISIONS BY BOROUGH FOR EACH YEAR#############

Pedesterian_Deaths_by_Years = summarise(Borough_Summary_Year, total_pedestrians_killed = sum(NUMBER.OF.PEDESTRIANS.KILLED))


Total_Pedestrians_Killed_by_Years = ggplot(data = Pedesterian_Deaths_by_Years, aes(x = Year, y = total_pedestrians_killed)) + 
                                    geom_bar(aes(fill = BOROUGH), position = "dodge", stat = 'identity') + 
                                    theme_bw() +
                                    theme(legend.position = "right") +
                                    theme(legend.text=element_text(size=10)) +
                                    ggtitle('Total Pedestrians Killed in Collisions by Year') + 
                                    ylab("Number of Pedestrian Deaths") +
                                    xlab("New York City Boroughs") +
                                    theme(axis.text.x = element_text(angle = 0, hjust = 0))
Total_Pedestrians_Killed_by_Years

#############TOTAL CYCLISTS KILLED IN COLLISIONS BY BOROUGH FOR EACH YEAR#############

Cyclist_Deaths_Years = summarise(Borough_Summary_Year, Total_Cyclist_Killed = sum(NUMBER.OF.CYCLIST.KILLED))


Total_Cyclists_Killed_by_Years = ggplot(data = Cyclist_Deaths_Years, aes(x = Year, y = Total_Cyclist_Killed)) + 
                                 geom_bar(aes(fill = BOROUGH), position = "dodge", stat = 'identity') + 
                                 theme_bw() +
                                 theme(legend.position = "right") +
                                 theme(legend.text=element_text(size=10)) +
                                 ggtitle('Total Cyclists Killed in Collisions by Year') + 
                                 ylab("Number of Cyclist Deaths") +
                                 xlab("New York City Boroughs") +
                                 theme(axis.text.x = element_text(angle = 0, hjust = 0))    
Total_Cyclists_Killed_by_Years




#############TOTAL MOTORISTS KILLED IN COLLISIONS BY BOROUGH FOR EACH YEAR#############

Motorist_Deaths_Years = summarise(Borough_Summary_Year, Total_Motorist_Killed = sum(NUMBER.OF.MOTORIST.KILLED))


Total_Motorists_Killed_by_Years = ggplot(data = Motorist_Deaths_Years, aes(x = BOROUGH, y = Total_Motorist_Killed)) + 
                                  geom_bar(aes(fill = BOROUGH), position = "dodge", stat = 'identity') + 
                                  theme_bw() +
                                  theme(legend.position = "right") +
                                  theme(legend.text=element_text(size=10)) +
                                  ggtitle('Total Motorists Killed in Collisions by Year') + 
                                  ylab("Number of Motorist Deaths") +
                                  xlab("New York City Boroughs") +
                                  theme(axis.text.x = element_text(angle = 0, hjust = 0))    
Total_Motorists_Killed_by_Years






#################################################
########ANALYSIS OF TOTAL DEATHS BY TIME#########
#################################################

Borough_Summary_Year$TIME = sapply(Borough_Summary_Year$TIME, function(x) paste0(x, ":00"))
Borough_Summary_Year$TIME = times(Borough_Summary_Year$TIME)


#############NIGHT TIME TOTAL DEATHS#############


Night_Time_Accidents = filter(Borough_Summary_Year, TIME > as.numeric(times('00:00:00')) & TIME < as.numeric(times('05:00:00')))
Night_Time_People_Deaths = summarise(Night_Time_Accidents, Total_People_Killed_Night = sum(NUMBER.OF.PEDESTRIANS.KILLED, NUMBER.OF.CYCLIST.KILLED, NUMBER.OF.MOTORIST.KILLED))
Total_Deaths_by_Night = ggplot(data = Night_Time_People_Deaths, aes(x = Year, y = Total_People_Killed_Night)) + 
                        #geom_bar(aes(fill = BOROUGH), position = 'dodge', stat = 'identity') + 
                        geom_line(aes(colour = BOROUGH)) +
                        theme_bw() + 
                        theme(legend.position = "right") +
                        theme(legend.text=element_text(size=5)) +
                        ggtitle('Total People Killed in Collisions at Night (00:00 - 04:59)') + 
                        ylab("Number of Total Deaths") +
                        xlab("Year") +
                        theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
                        theme(legend.position = "right") +
                        theme(legend.text=element_text(size=7)) 
Total_Deaths_by_Night





#############MORNING TIME TOTAL DEATHS#############

Morning_Accidents = filter(Borough_Summary_Year, TIME < as.numeric(times('12:00:00')) & TIME >= as.numeric(times('05:00:00')))
Morning_Time_People_Deaths = summarise(Morning_Accidents, Total_People_Killed_Morning = sum(NUMBER.OF.PEDESTRIANS.KILLED, NUMBER.OF.CYCLIST.KILLED, NUMBER.OF.MOTORIST.KILLED))
Total_Deaths_by_Morning = ggplot(data = Morning_Time_People_Deaths, aes(x = Year, y = Total_People_Killed_Morning)) + 
                          geom_bar(aes(fill = BOROUGH), position = "dodge", stat = "identity")+  
                          theme_bw() + 
                          theme(legend.position = "right") +
                          theme(legend.text=element_text(size=5)) +
                          ggtitle('Total People Killed in Collisions in Morning (05:00 - 11:59)') + 
                          ylab("Number of Total Deaths") +
                          xlab("Year") +
                          theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
                          theme(legend.position = "right") +
                          theme(legend.text=element_text(size=10)) 
Total_Deaths_by_Morning


#############TOTAL PEDESTRIAN DEATHS - TIME OF DAY#############

Morning_Time_Pedestrian_Deaths = summarise(Morning_Accidents, Total_Pedestrian_Killed_Morning = sum(NUMBER.OF.PEDESTRIANS.KILLED))
Midday_Pedestrian_Deaths = summarise(Midday_Accidents, Total_Pedestrian_Killed_Midday = sum(NUMBER.OF.PEDESTRIANS.KILLED))
Evening_Pedestrian_Deaths = summarise(Evening_Accidents, Total_Pedestrian_Killed_Evening = sum(NUMBER.OF.PEDESTRIANS.KILLED))
Night_Time_Pedestrian_Deaths = summarise(Night_Time_Accidents, Total_Pedestrian_Killed_Night = sum(NUMBER.OF.PEDESTRIANS.KILLED))


Time_List_Pedestrian = list(Morning_Time_Pedestrian_Deaths, Midday_Pedestrian_Deaths, Evening_Pedestrian_Deaths, Night_Time_Pedestrian_Deaths)
Total_Pedestrian_Death_Compared_by_Time = Reduce(function(x,y) merge(x,y, all = TRUE), Time_List_Pedestrian)
Total_Pedestrian_Death_Compared_by_Time = mutate(Total_Pedestrian_Death_Compared_by_Time, Total_Pedestrians_Killed = apply(Total_Pedestrian_Death_Compared_by_Time[,3:6], 1, sum))

Total_Pedestrian_Death_by_TimeofDay =  
  ggplot(Total_Pedestrian_Death_Compared_by_Time, aes(Year, colour = Time_of_Day)) + 
  ylab("Number of Pedestrian Deaths") +
  xlab("Year") +
  geom_line(aes(y = Total_Pedestrian_Killed_Morning, colour = "Morning- (05:00 - 11:59)")) +
  geom_line(aes(y = Total_Pedestrian_Killed_Midday, colour = "Midday- (12:00 - 16:59)")) +
  geom_line(aes(y = Total_Pedestrian_Killed_Evening, colour = "Evening- (17:00 - 23:59)")) +
  geom_line(aes(y = Total_Pedestrian_Killed_Night, colour = "Night - (00:00 - 04:59)")) +
  facet_grid(.~BOROUGH) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7 , angle = 90, hjust = 0)) +
  theme(axis.text.y = element_text(size = 7 , angle = 0, hjust = 0)) +
  ggtitle('Total Pedestrian Killed in Collisions - Time of Day')


Total_Pedestrian_Death_by_TimeofDay    



#############TOTAL CYCLIST DEATHS - TIME OF DAY#############


Morning_Time_Cyclist_Deaths = summarise(Morning_Accidents, Total_Cyclist_Killed_Morning = sum(NUMBER.OF.CYCLIST.KILLED))
Midday_Cyclist_Deaths = summarise(Midday_Accidents, Total_Cyclist_Killed_Midday = sum(NUMBER.OF.CYCLIST.KILLED))
Evening_Cyclist_Deaths = summarise(Evening_Accidents, Total_Cyclist_Killed_Evening = sum(NUMBER.OF.CYCLIST.KILLED))
Night_Time_Cyclist_Deaths = summarise(Night_Time_Accidents, Total_Cyclist_Killed_Night = sum(NUMBER.OF.CYCLIST.KILLED))


Time_List_Cyclist = list(Morning_Time_Cyclist_Deaths, Midday_Cyclist_Deaths, Evening_Cyclist_Deaths, Night_Time_Cyclist_Deaths)
Total_Cyclist_Death_Compared_by_Time = Reduce(function(x,y) merge(x,y, all = TRUE), Time_List_Cyclist)
Total_Cyclist_Death_Compared_by_Time = mutate(Total_Cyclist_Death_Compared_by_Time, Total_Cyclist_Killed = apply(Total_Cyclist_Death_Compared_by_Time[,3:6], 1, sum))

Total_Cyclist_Death_by_TimeofDay =  
  ggplot(Total_Cyclist_Death_Compared_by_Time, aes(Year, colour = Time_of_Day)) + 
  ylab("Number of Cyclist Deaths") +
  xlab("Year") +
  geom_line(aes(y = Total_Cyclist_Killed_Morning, colour = "Morning- (05:00 - 11:59)")) +
  geom_line(aes(y = Total_Cyclist_Killed_Midday, colour = "Midday- (12:00 - 16:59)")) +
  geom_line(aes(y = Total_Cyclist_Killed_Evening, colour = "Evening- (17:00 - 23:59)")) +
  geom_line(aes(y = Total_Cyclist_Killed_Night, colour = "Night - (00:00 - 04:59)")) +
  facet_grid(.~BOROUGH) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7 , angle = 90, hjust = 0)) +
  theme(axis.text.y = element_text(size = 7 , angle = 0, hjust = 0)) +
  ggtitle('Total Cyclist Killed in Collisions - Time of Day')


Total_Cyclist_Death_by_TimeofDay    




#############MID-DAY TIME TOTAL DEATHS#############


Midday_Accidents = filter(Borough_Summary_Year, TIME >= as.numeric(times('12:00:00'))  & TIME < as.numeric(times('17:00:00')))
Midday_People_Deaths = summarise(Midday_Accidents, Total_People_Killed_Midday = sum(NUMBER.OF.PEDESTRIANS.KILLED, NUMBER.OF.CYCLIST.KILLED, NUMBER.OF.MOTORIST.KILLED))
Total_Deaths_by_Midday = ggplot(data = Midday_People_Deaths, aes(x = Year, y = Total_People_Killed_Midday)) + 
                          geom_bar(aes(fill = BOROUGH), position = 'dodge', stat = 'identity') +  
                          theme_bw() + 
                          theme(legend.position = "right") +
                          theme(legend.text=element_text(size=5)) +
                          ggtitle('Total People Killed in Collisions Midday (12:00 - 16:59)') + 
                          ylab("Number of Total Deaths") +
                          xlab("Year") +
                          theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
                          theme(legend.position = "right") +
                          theme(legend.text=element_text(size=10)) 
Total_Deaths_by_Midday



#############EVENING TIME TOTAL DEATHS#############


Evening_Accidents = filter(Borough_Summary_Year, TIME >= as.numeric(times('17:00:00')))
Evening_People_Deaths = summarise(Evening_Accidents, Total_People_Killed_Evening = sum(NUMBER.OF.PERSONS.KILLED))
Total_Deaths_by_Evening = ggplot(data = Evening_People_Deaths, aes(x = Year, y = Total_People_Killed_Evening)) + 
                          geom_bar(aes(fill = BOROUGH), position = 'dodge', stat = 'identity') +  
                          theme_bw() + 
                          theme(legend.position = "right") +
                          theme(legend.text=element_text(size=5)) +
                          ggtitle('Total People Killed in Collisions Evening (17:00 - 23:59)') + 
                          ylab("Number of Total Deaths") +
                          xlab("Year") +
                          theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
                          theme(legend.position = "right") +
                          theme(legend.text=element_text(size=7)) 
Total_Deaths_by_Evening

Time_List = list(Morning_Time_People_Deaths, Midday_People_Deaths, Evening_People_Deaths, Night_Time_People_Deaths)
Total_Death_Compared_by_Time = Reduce(function(x,y) merge(x,y, all = TRUE), Time_List)
Total_Death_Compared_by_Time = mutate(Total_Death_Compared_by_Time, Total_Persons_Killed = apply(Total_Death_Compared_by_Time[,3:6], 1, sum))

##########################TOTAL DEATHS - TIME OF DAY#####################

Total_Death_by_TimeofDay =  
  ggplot(Total_Death_Compared_by_Time, aes(Year, colour = Time_of_Day)) + 
  ylab("Number of Deaths") +
  xlab("Year") +
  geom_line(aes(y = Total_People_Killed_Morning, colour = "Morning- (05:00 - 11:59)")) +
  geom_line(aes(y = Total_People_Killed_Midday, colour = "Midday- (12:00 - 16:59)")) +
  geom_line(aes(y = Total_People_Killed_Evening, colour = "Evening- (17:00 - 23:59)")) +
  geom_line(aes(y = Total_People_Killed_Night, colour = "Night - (00:00 - 04:59)")) +
  facet_grid(.~BOROUGH) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7 , angle = 90, hjust = 0)) +
  theme(axis.text.y = element_text(size = 7 , angle = 0, hjust = 0)) +
  ggtitle('Total People Killed in Collisions - Time of Day')


Total_Death_by_TimeofDay    

                            
##############################################
########LOCATION ANALYSIS BY ZIPCODES#########
##############################################

install.packages("devtools")
install_github('arilamstein/choroplethrZip@v1.5.0')
library (choroplethrZip)
library(devtools)
library(choroplethr)



##########################TOTAL NUMBER OF DEATHS - BY ZIPCODES#####################

Zipcodes = mutate(by_borough_2, region = as.character(ZIP.CODE))
ZipcodesGrouped = group_by(Zipcodes, BOROUGH, ZIP.CODE)
#Zipcodes_Map_Data = select(ZipcodesGrouped, region, NUMBER.OF.PERSONS.KILLED)
#ZipCodes = summarise(Zipcodes_Map_Data, sum(NUMBER.OF.PERSONS.KILLED))

ZipCodes <- select(ZipcodesGrouped, region, NUMBER.OF.PERSONS.KILLED) %>% 
              summarise(sum(NUMBER.OF.PERSONS.KILLED))
ZipCodes = ZipCodes[-1]
colnames(ZipCodes) = c("region", "value")
ZipCodes$region = clean.zipcodes(ZipCodes$region) ##integer to character
ZipCodes = ZipCodes[-which(is.na(ZipCodes$region)), ]
ZipCodes =ZipCodes[!duplicated(ZipCodes$region), ]
ZipCodes = ZipCodes[!ZipCodes$region %in% c("11249", "10000", "10048", "10281", "11695"), ]
Total_Death_by_Zipcodes <- zip_choropleth(ZipCodes, zip_zoom=ZipCodes$region, title = "Total Deaths by Zip Codes", legend = "Number of Total Deaths", num_colors = 5)
Total_Death_by_Zipcodes



##########################TOTAL NUMBER OF CYCLIST DEATHS - BY ZIPCODES#####################

ZipCodes_Cyclist <- select(ZipcodesGrouped, region, NUMBER.OF.CYCLIST.KILLED) %>% 
  summarise(sum(NUMBER.OF.CYCLIST.KILLED))
ZipCodes_Cyclist = ZipCodes_Cyclist[-1]
colnames(ZipCodes_Cyclist) = c("region", "value")
ZipCodes_Cyclist$region = clean.zipcodes(ZipCodes_Cyclist$region) ##integer to character
ZipCodes_Cyclist = ZipCodes_Cyclist[-which(is.na(ZipCodes_Cyclist$region)), ]
ZipCodes_Cyclist =ZipCodes_Cyclist[!duplicated(ZipCodes_Cyclist$region), ]
ZipCodes_Cyclist = ZipCodes_Cyclist[!ZipCodes_Cyclist$region %in% c("11249", "10000", "10048", "10281", "11695"), ]
Total_Death_by_Zipcodes_Cyclist <- zip_choropleth(ZipCodes_Cyclist, zip_zoom=ZipCodes_Cyclist$region, title = "Total Deaths Cyclists by Zip Codes", legend = "Number of Total Deaths", num_colors = 5)
Total_Death_by_Zipcodes_Cyclist



##########################TOTAL NUMBER OF MOTORIST DEATHS - BY ZIPCODES#####################

ZipCodes_Motorist <- select(ZipcodesGrouped, region, NUMBER.OF.MOTORIST.KILLED) %>% 
  summarise(sum(NUMBER.OF.MOTORIST.KILLED))
ZipCodes_Motorist = ZipCodes_Motorist[-1]
colnames(ZipCodes_Motorist) = c("region", "value")
ZipCodes_Motorist$region = clean.zipcodes(ZipCodes_Motorist$region) ##integer to character
ZipCodes_Motorist = ZipCodes_Motorist[-which(is.na(ZipCodes_Motorist$region)), ]
ZipCodes_Motorist =ZipCodes_Motorist[!duplicated(ZipCodes_Motorist$region), ]
ZipCodes_Motorist = ZipCodes_Motorist[!ZipCodes_Motorist$region %in% c("11249", "10000", "10048", "10281", "11695"), ]
Total_Death_by_Zipcodes_Motorist <- zip_choropleth(ZipCodes_Motorist, zip_zoom=ZipCodes_Motorist$region, title = "Total Deaths Motorists by Zip Codes", legend = "Number of Total Deaths", num_colors = 7)
Total_Death_by_Zipcodes_Motorist



##########################TOTAL NUMBER OF PEDESTRIAN DEATHS - BY ZIPCODES#####################

ZipCodes_Pedestrian <- select(ZipcodesGrouped, region, NUMBER.OF.PEDESTRIANS.KILLED) %>% 
  summarise(sum(NUMBER.OF.PEDESTRIANS.KILLED))
ZipCodes_Pedestrian = ZipCodes_Pedestrian[-1]
colnames(ZipCodes_Pedestrian) = c("region", "value")
ZipCodes_Pedestrian$region = clean.zipcodes(ZipCodes_Pedestrian$region) ##integer to character
ZipCodes_Pedestrian = ZipCodes_Pedestrian[-which(is.na(ZipCodes_Pedestrian$region)), ]
ZipCodes_Pedestrian =ZipCodes_Pedestrian[!duplicated(ZipCodes_Pedestrian$region), ]
ZipCodes_Pedestrian = ZipCodes_Pedestrian[!ZipCodes_Pedestrian$region %in% c("11249", "10000", "10048", "10281", "11695"), ]
  Total_Death_by_Zipcodes_Pedestrian <- zip_choropleth(ZipCodes_Pedestrian, zip_zoom=ZipCodes_Motorist$region, title = "Total Deaths Pedestrians by Zip Codes", legend = "Number of Total Deaths", num_colors = 7)
Total_Death_by_Zipcodes_Pedestrian





##############################################
########SEASONAL (MONTHLY) ANALYSIS###########
##############################################

by_month = mutate(by_year, Month = month(DATE))
by_month = group_by(by_month,BOROUGH, Year, Month)

Spring_Accidents = filter(by_month, Month %in% c(3,4,5))
Spring_Total_People_Deaths = summarise(Spring_Accidents, Total_People_Killed_Spring = sum(NUMBER.OF.PERSONS.KILLED))

Summer_Accidents = filter(by_month, Month %in% c(6,7,8))
Summer_Total_People_Deaths = summarise(Summer_Accidents, Total_People_Killed_Summer = sum(NUMBER.OF.PERSONS.KILLED))

Fall_Accidents = filter(by_month, Month %in% c(9,10,11))
Fall_Total_People_Deaths = summarise(Fall_Accidents, Total_People_Killed_Fall = sum(NUMBER.OF.PERSONS.KILLED))

Winter_Accidents = filter(by_month, Month %in% c(12,1,2))
Winter_Total_People_Deaths = summarise(Winter_Accidents, Total_People_Killed_Winter = sum(NUMBER.OF.PERSONS.KILLED))

Season_List = list(Spring_Total_People_Deaths, Summer_Total_People_Deaths, Fall_Total_People_Deaths, Winter_Total_People_Deaths)
Total_Death_Compared_by_Seasons = Reduce(function(x,y) merge(x,y, all = TRUE), Season_List)
Total_Death_Compared_by_Seasons[is.na(Total_Death_Compared_by_Seasons)] <- 0 
Total_Death_Compared_by_Seasons = mutate(Total_Death_Compared_by_Seasons, Total_Persons_Killed = apply(Total_Death_Compared_by_Seasons[,3:6], 1, sum))

Total_Death_by_Seasons =  ggplot(Total_Death_Compared_by_Seasons, aes(x = Month, y = Total_Persons_Killed)) + 
                          ylab("Number of Deaths") +
                          xlab("Month") +
                          geom_bar(aes(fill = BOROUGH), position = "stack", stat = "identity") +
                          facet_grid(.~Year) +
                          theme_bw() +
                          theme(axis.text.x = element_text(size = 7 , angle = 90, hjust = 0)) +
                          theme(axis.text.y = element_text(size = 7 , angle = 0, hjust = 0)) +
                          ggtitle('Total People Killed in Collisions - Seasonal Analysis') +
                          theme(legend.position = "right") +
                          theme(legend.text=element_text(size=10)) +
                          coord_cartesian(xlim = c(1, 12))

Total_Death_by_Seasons





