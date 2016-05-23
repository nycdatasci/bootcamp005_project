#####By Ruonan Ding#########
############################

setwd("~/offshore_leaks_csvs")
entities <- read.csv("Entities.csv", header = T)
intermediaries <- read.csv("Intermediaries.csv", header= T)

#valid until, country_codes, sourceID, status
library(dplyr)
library(googleVis)

entities.append1 <- filter(entities, sourceID =="Panama Papers")
entities.append2 <- select(entities.append1, country_codes, countries, status, jurisdiction_description,
                          jurisdiction, incorporation_date, struck_off_date, internal_id)
sum(is.na(entities.append2$struck_off_date))
#entities.append <- group_by(entities.append, country_codes, sourceID, status, jurisdiction, incorporation_date) %>% summarise(count = n())
#entities.append["category"] <- rep("entities", nrow(entities.append))

inter.append1 <- filter(intermediaries, sourceID =="Panama Papers")
inter.append2 <- select(inter.append1, country_codes, countries, status, internal_id)

#inter.append <- group_by(inter.append, country_codes, sourceID, status) %>% summarise(count = n())
#inter.append["category"] <- rep("intermediaries", nrow(inter.append))
#inter.append[c("jurisdiction", "incorporation_date")] <- NA

####Data Munging#################################################################
#scrub the date format for the entities tables and add the year column
entities.append2$incorporation_date <- as.Date(entities.append2$incorporation_date, format = "%d-%b-%Y")
entities.append2$struck_off_date <- as.Date(entities.append2$struck_off_date, format = "%d-%b-%Y")
entities.append2$active.year <- ifelse(is.na(entities.append2$struck_off_date)==TRUE, "", 
     as.integer(as.numeric(difftime(entities.append2$struck_off_date,entities.append2$incorporation_date, units = "days"))/365 +1))
entities.append2$incorp.year <- as.numeric(format(entities.append2$incorporation_date, "%Y"))   
entities.yearready <-subset(entities.append2, select = -c(incorporation_date,struck_off_date) )

#scrub for cities for entities tables
for (i in 1:nrow(entities.yearready)) {
  if (entities.yearready[i,1] == "") {
    entities.yearready[i,1] <- "XXX"
  }
}

entities_finaltable <- entities.yearready

#############country for intemediry#######################################
iData <- inter.append2
fctr.cols <- sapply(iData, is.factor)
iData[,fctr.cols] <- sapply(iData[,fctr.cols], as.character)

countryList <- list()
for (i in 1:nrow(inter.append2)) {
  if (iData[[i,1]] == "") {
    iData[[i,1]] <- "XXX"
  }
  countryCodes <- strsplit(iData[[i,1]], ";")[[1]]
  for (oneCountry in countryCodes) {
    if (!(oneCountry %in% names(countryList))) {
      countryList[[oneCountry]] <- c(countryList[[oneCountry]], oneCountry=data.frame())
      #colnames(countryDataList[[oneCountry]][,1]) <- "country_codes"
    }
    countryList[[oneCountry]] <- rbind(countryList[[oneCountry]], data.frame(c(oneCountry, iData[i,-1])))
  }
}

######join in country and intermediary table##########################################
entities_finaltable$internal_id <-as.factor(entities_finaltable$internal_id)
join_table <- left_join(inter_finaltable, entities_finaltable, "internal_id")
colnames(join_table) <- c("inter_country_codes", "inter_countries", "inter_status","internal_id",
                          "entity_country_codes", "entity_countries", "entity_status", "jurisdiction_description",
                          "jurisdiction", "active.year", "incorp.year")

###################################################################################
status_table <- group_by(entities_finaltable, status, active.year) %>%
  summarise(count =  n())
#percentage formula
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
status_tablefinal <- group_by(status_table, status) %>% summarise(number_of_entities = sum(count)) %>% 
  mutate(percentage = percent(number_of_entities/sum(number_of_entities)))

#for the ones that are no longer active, the average length of existence.
status_table$active.year <-as.numeric(status_table$active.year)
a <- group_by(status_table, active.year) %>% summarise(sum(count))

StatusTable <- gvisTable(arrange(status_tablefinal, desc(number_of_entities)))
plot(StatusTable)

##########MAP PLOTS############################
library(countrycode)
b <- left_join(entities_finaltable, countrycode_data,c("country_codes"="iso3c"))[,1:9]
geochart.data<- filter(group_by(b, country.name)%>%summarise(number_of_entites = n()), country.name!= "XXX")
top20.geochart.data<- head(arrange(geochart.data, desc(number_of_entites)), 20)

#GeoChar <- gvisGeoChart(top20.geochart.data, "country.name", "number_of_entites",
#                 options=list(width=600, height=500,showTip=TRUE,
#                              mapType='normal'))
#plot(GeoChar)

GeoVis <- gvisMap(top20.geochart.data, "country.name", "number_of_entites",
                       options=list(width=600, height=500,
                                    showTip=TRUE,
                                    mapType='normal'))
plot(GeoVis)


PopTable <- gvisTable(mutate(top20.geochart.data, "% of All Entities" = number_of_entites/nrow(entities_finaltable)),
                      formats=list('% of All Entities'='#.#%'),
                      options=list(page='enable'))
plot(PopTable)
##############################################################
#plot the time line for the overall entities/jurisdiction
#  output$TestPlot1 <- renderPlot({
timeline_overview<- ggplot(filter(entities.append2,incorp.year > 1975), aes(incorporation_date))+
  geom_density()+
  theme_bw()+
  xlab("Incorp. Year")+
  ylab("Number of Entities")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

timeline_adjustment<- ggplot(filter(filter(entities.append2,incorp.year > 1975), jurisdiction_description == "Bahamas"), aes(incorporation_date))+
  geom_density(fill = "input")+
  theme_bw()+
  xlab("Incorp. Year")+
  ylab("Number of Entities")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

unique(entities.append2$jurisdiction_description)
###########################################################'
###########################################################'
###########################################################'
###########################################################'
head(join_table)

#USA entities with regarding intermedias
usa.entities <- filter(join_table, entity_country_codes == "USA")
c <- levels(usa.entities$inter_country_codes)
d <-as.data.frame(cbind("inter" = c, "index" = rep(1,length(c))))
e <- left_join(d, countrycode_data,c("inter"="iso3c"))[, c("country.name", "index")]
GeoLayer1 <- gvisGeoChart(e, "country.name", "index",
               options=list(width=800,height=450, colors= "['green']",
                            title = "The Intermediaries Countries that faciliated USA Entities",
                            legend = 'none'))
plot(GeoLayer1)

#USA with jurisdiction
f <- levels(usa.entities$jurisdiction_description)
g <-as.data.frame(cbind("juris" = f, "index" = rep(1,length(f))))

GeoLayer2 <- gvisGeoChart(g, "juris", "index",
                       options=list(width=800,height=450, colors= "['red']",
                                    title = "The Final Jurisdition Countries of USA Entities"))
plot(GeoLayer2)

GeolayerFinal <- gvisMerge(GeoLayer1, GeoLayer2, horizontal = FALSE)
plot(GeolayerFinal)



###########################################

group_by(status_table, active.year) %>% summarise(sum(count))
         
         
      
complete.table <- rbind(officer_ready, entities_ready, inter_ready)

source.options<- levels(complete.table$sourceID)

country.options <- levels(complete.table$country_codes)

aaa<-group_by(complete.table, jurisdiction) %>% summarise('total' = sum(count)) %>% arrange(desc(total))
bbb<- entities_ready[as.character(entities_ready$jurisdiction) %in% jurisdiction.options, ]
jurisdiction.options <- c('BVI', 'PMA', 'BAH', 'SEY', 'SAM', 'NIUE', 'ANG', 'HK', 'NEV', 'CAYMN', 'SGP')

ccc <- group_by(complete.table, country_codes) %>% summarise('total' = sum(count)) %>% arrange(desc(total))
country.options.new <- c('VGB', 'HKG', 'CHE', 'CHN', 'GBR', 'PAN','TWN', 'JEY', 'RUS', 'USA')
ddd <- inter_ready[as.character(inter_ready$country_codes) %in% country.options.new, ]

