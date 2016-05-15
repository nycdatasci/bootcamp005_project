#####By Ruonan Ding#########
############################

#data from: 
#https://cloudfront-files-1.publicintegrity.org/offshoreleaks/data-csv.zip#_ga=1.150637622.1225731120.1462845567

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

################
#USA entities with regarding intermedias
usa.entities <- filter(join_table, entity_country_codes == "USA")
c <- levels(usa.entities$inter_country_codes)
d <-as.data.frame(cbind("inter" = c, "index" = rep(1,length(c))))
e <- left_join(d, countrycode_data,c("inter"="iso3c"))[, c("country.name", "index")]

#USA with jurisdiction
f <- levels(usa.entities$jurisdiction_description)
g <-as.data.frame(cbind("juris" = f, "index" = rep(1,length(f))))

###########################################
jurisdiction.options <- c('BLZ', 'PMA', 'BAH', 'SEY', 'SAM', 'NIUE')


