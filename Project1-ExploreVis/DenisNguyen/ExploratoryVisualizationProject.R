setwd('~/Desktop')
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
my_data <- read.csv('Water_Quality_complaints-retrieved42016.csv', header = TRUE, stringsAsFactors = F)

# remove unresolved cases
my_data <- subset(my_data, Closed.Date != "") # some cases are still started or open but they have a closed date

# create new data frame, selecting columns of interest and calculating new ones
h2o <- tbl_df(data.frame(Unique.Key = my_data$Unique.Key,
                  Created.Date = as.Date(my_data$Created.Date,format='%m/%d/%Y'),
                  Closed.Date = as.Date(my_data$Closed.Date,format='%m/%d/%Y'),
                  Resolution.Time = as.numeric(as.Date(my_data$Closed.Date,format='%m/%d/%Y')
                                            - as.Date(my_data$Created.Date,format='%m/%d/%Y')),
                  Date.Start.Y = as.numeric(format(as.Date(my_data$Created.Date,format='%m/%d/%Y'),"%Y")),
                  Date.Start.M = as.numeric(format(as.Date(my_data$Created.Date,format='%m/%d/%Y'),"%m")),
                  Date.End.Y = as.numeric(format(as.Date(my_data$Closed.Date,format='%m/%d/%Y'),"%Y")),
                  Date.End.M = as.numeric(format(as.Date(my_data$Closed.Date,format='%m/%d/%Y'),"%m")),
                  Descriptor = my_data$Descriptor,
                  Incident.Zip = my_data$Incident.Zip,
                  Address.Type = my_data$Address.Type,
                  City = my_data$City,
                  Community.Board = my_data$Community.Board,
                  Borough = my_data$Borough,
                  X.Coordinate..State.Plane. = my_data$X.Coordinate..State.Plane.,
                  Y.Coordinate..State.Plane. = my_data$Y.Coordinate..State.Plane.,
                  Latitude = my_data$Latitude,
                  Longitude = my_data$Longitude,
                  Location = my_data$Location,
                    stringsAsFactors = F))

# remove bad records - 6 year cases closed within 4 minutes of each other
h2o <- h2o[(!h2o$Unique.Key %in% c(15636014,18952730,16654188)),]

# Number/Type Of Cases Per Year
type_yr <- h2o %>% group_by(Type = Descriptor, Year = Date.Start.Y) %>% summarise(Complaints = n())
ggplot(data = type_yr, aes(x=Year, y=Complaints)) +
  geom_bar(stat="identity", aes(fill=factor(Type)), position = "stack") +
  theme_gdocs() + scale_fill_gdocs() +
  ggtitle('Type Of Cases Per Year') +
  ylab('Number of Cases') + xlab('Years') +
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016)) +
  guides(fill=guide_legend(title="Case Descriptions"))


# Number Of Cases Over Time
total_mo <- h2o %>% group_by(Year = Date.Start.Y, Month = Date.Start.M) %>% summarise(Complaints = n()) %>%
mutate(new.month = 12*(Year-2010)+Month)
ggplot(data = total_mo, aes(x=new.month, y=Complaints, color=(Month))) +
  geom_line() + xlab("Seasons") + ylab("Number of Cases") +
  ggtitle('Number Of Cases Over Time') +
  scale_x_continuous(breaks=seq(from = 3, to = 76, by = 3), 
                     labels=rep(c("Spring", "Summer", "Fall", "Winter"), length.out=25)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position='none')


# Number/Type of Cases Per Borough
total_b <- h2o %>% group_by(Borough, Type=Descriptor) %>% summarise(Complaints = n())
ggplot(data = total_b, aes(x=Borough, y=Complaints)) +
  geom_bar(stat="identity", aes(fill=(Type)), position = "dodge") +
  theme_gdocs() + scale_fill_gdocs() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Type Of Cases Per Borough') +
  ylab('Number of Cases') + xlab('Boroughs') +
  guides(fill=guide_legend(title="Case Descriptions"))


# Average Response Time Per Year
res_yr <- h2o %>% group_by(Year = Date.End.Y) %>% summarise(Time = mean(Resolution.Time))
ggplot(data = res_yr, aes(x=Year, y=Time)) +
  geom_bar(stat="identity", aes(fill=factor(Year))) +
  theme_gdocs() + scale_fill_gdocs() + 
  ggtitle('Average Response Time Per Year') +
  ylab('Response Time (Days)') + xlab('Years') +
  scale_x_continuous(breaks=c(2010,2011,2012,2013,2014,2015,2016)) +
  guides(fill=guide_legend(title="Years"), legend.position='none')


# Average Response Time Per Case Type
res_type <- h2o %>% group_by(Type = Descriptor) %>% summarise(Time = mean(Resolution.Time))
ggplot(data = res_type, aes(x=Type, y=Time)) +
  geom_bar(stat="identity", aes(fill=(Type))) +
  theme_gdocs() + scale_fill_gdocs() + 
  ggtitle('Average Response Time Per Case Type') +
  ylab('Response Time (Days)') + xlab('Case Description') +
  guides(fill=guide_legend(title="Case Descriptions")) +
  theme(axis.text.x = element_blank())


# Average Response Time Per Borough
res_b <- h2o %>% group_by(Borough = Borough) %>% summarise(Time = mean(Resolution.Time))
ggplot(data = res_b, aes(x=Borough, y= Time)) +
  geom_bar(stat="identity", aes(fill=(Borough))) +
  theme_gdocs() + scale_fill_gdocs() + 
  ggtitle('Average Response Time Per Borough') +
  ylab('Response Time (Days)') + xlab('Boroughs') +
  guides(fill=guide_legend(title="Boroughs")) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position='none')