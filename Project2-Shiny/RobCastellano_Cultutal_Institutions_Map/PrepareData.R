library(plyr)
library(dplyr)
library(reshape2)
library(leaflet)
library(RDSTK)
library(htmltools)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(googleVis)
library(rgdal) 
library(rgeos)
library(zipcode)

#Data available from NYC Open Data: https://data.cityofnewyork.us/Recreation/DCLA-Cultural-Organizations/u35m-9t32

#Read in data
ci = read.csv("DCLA_Cultural_Organizations.csv", header = T, stringsAsFactors = F)


###################################
##########CLEANING
######################################
ci = ci[-which(ci$Zip.Code == 12405),]         #Remove one institution with an upstate address
ci = ci[which(ci$State == "NY"),]              #Remove one institution with a MN address
ci = ci[-which(ci$Zip.Code == 6705),]           #Remove one institution in CT
ci$Zip.Code[which(ci$Zip.Code == 7307)] = 10016 #Fix incorect zipcode for one institution

ci$Discipline[which(ci$Discipline == "")] = "Not Listed"
ci$Discipline = factor(ci$Discipline)

#Rename some factors without abbreviations
ci$Discipline = mapvalues(ci$Discipline, to = c("Multi-Discipline, Non-Perform", "Multi-Discipl, Perf & Non-Perf"),
              from = c("Multi-Discipline, Non-Performing", "Multi-Discipline, Performing and Non-Performing"))

# Remove Community Board and Council District info
ci = select(ci, -Council.District, -Community.Board)

#Put all zip codes in 5-digit format
ci$Zip.Code = sapply(ci$Zip.Code, function(x) substr(x, 1, 5))

##################################################################
##################################################################

################################################################
###################CREATING MAIN DATA THAT WILL BE USED
###############################################################

##################################
### Contains number of institutions of each type in each zipcode
discipline.zip = ci %>% group_by(Discipline, Zip.Code) %>% dplyr::summarise(count = n())

##################################

### ci.po contains institutions whose only address is  PO Box or c/o
### THese will be treated seperately when mapping.
ci.po = filter(ci, (grepl('PO', ci$Address) & (grepl('Box', ci$Address) | grepl('BOX', ci$Address)))| 
                 (grepl('P.O.', ci$Address) & grepl('Box', ci$Address) | grepl('BOX', ci$Address)) | 
                 (grepl('P. O.', ci$Address) & grepl('Box', ci$Address) | grepl('BOX', ci$Address)) |
                 (grepl('Po', ci$Address) & grepl('Box', ci$Address) | grepl('BOX', ci$Address)) |
                 grepl('c/o', ci$Address))

ci.po = rbind(ci.po, ci[1124,])    #Adds one more row with a PO Box that was listed under "POB ____"

##################################

## ci.no.po contains the complement of ci.po
ci.no.po = ci[-1124,]            #Removes "POB ___" mentioned above

ci.no.po = filter(ci.no.po, !((grepl('PO', ci.no.po$Address) & (grepl('Box', ci.no.po$Address) | grepl('BOX', ci.no.po$Address)))| 
                    (grepl('P.O.', ci.no.po$Address) & grepl('Box', ci.no.po$Address) | grepl('BOX', ci.no.po$Address)) | 
                    (grepl('P. O.', ci.no.po$Address) & grepl('Box', ci.no.po$Address) | grepl('BOX', ci.no.po$Address)) |
                      (grepl('Po', ci.no.po$Address) & grepl('Box', ci.no.po$Address) | grepl('BOX', ci.no.po$Address)) |
                    grepl('c/o', ci.no.po$Address)))

##################################
#################################

################################
#### ADDING LATITUDE AND LONGITUDE TO INSTITUTIONS WITH ADRESSES (ci.no.po)
#############################

#Function that returns a numeric vector of length 2, corresponding to the latitude and longitude of the
# street address x. If the function street2coordinates() unable to find the address x, it returns
# latitude = longitude = 1 (we will deal with these later).

latlng <- function(x)
{
  location = street2coordinates(x)
  if(is.null(location$latitude))
  {
    return(c(1,1))
  } else
  {
    return(c(location$latitude, location$longitude))
  }
}

#trystreet2coordinates attempts to find the latitude and longitude of an address x. Sometimes this function
# fails because x is not an address (e.g. if x is a PO Box). In this case street2coordinates throw an
# exception, which we catch and set lat = lng = NaN. (We will deal with these later)
trystreet2coordinates <- function(x)
{
  tryCatch(
    return(latlng(x)), 
    error = function(e) {
      c(lat = NaN, lng = NaN)})
}

#Adds latitude and longitude variables to ci.no.po (some will be NanN and some will be 1)
templatlng = sapply(apply(ci.no.po, 1, function(x) paste(x["Address"], 
                                                     x["City"], 
                                                     "NY", 
                                                     x["Zip.Code"], 
                                                     sep = ", ")),          #Address 
       trystreet2coordinates)
ci.no.po$latitude = templatlng[1,]
ci.no.po$longitude = templatlng[2,]

########################################################
#####Dealing with the latitude and longitude we set to 1 (these are the addresses street2coordinates
##### didn't throw an error, but couldn't find address)
######################################################
#There are 125 such observations
#By observation they all appear to have "#" in the address and this interferes with street2coordinates()
#Therefore, we will everything after '#' in the address (it is a unit/apt number) and retry function

templatlng = sapply(apply(filter(ci.no.po, latitude == 1), 1, 
                       function(x) paste(substr(x["Address"], 1, regexpr('#', x["Address"]) - 1),  
                       #Removes '#' and everything that comes after it from adress
                                                        
                        x["City"], 
                        "NY", 
                        x["Zip.Code"], 
                        sep = ", ")),          #Then make full address as above
               trystreet2coordinates)

# There is only one observation that this doesn't work for (because it had 'Unit #' in address).
# We add this one by hand.
templatlng[,'360 Court St. unit , Brooklyn, NY, 11231'] = latlng('360 Court St., Brooklyn, NY, 11231')

ci.no.po$latitude[which(ci.no.po$latitude == 1)] = templatlng[1,]
ci.no.po$longitude[which(ci.no.po$longitude == 1)] = templatlng[2,]

#############################################################
######## Dealing with NaN latitude and longitude (these were entries for which street2coordinats
######## threw an exception).
#########################################################
#There are 30 such entries

lat.is.nan = filter(ci.no.po, is.nan(latitude))      #These are the 30 entries we need to deal with
                                                     #We try to fix these entries as we did with the
                                                     #latitude and longitudes there were '1' above.
                                                     #Above we just elimated things after '#'. We will
                                                     #do the same things for other punctuation.

for(punctuation in c(',', 'Box', 'Ste.', 'Apt.'))
{
    temp = sapply(apply(filter(lat.is.nan, is.nan(latitude)), 1, 
                              function(x) paste(substr(x["Address"], 1, regexpr(punctuation, x["Address"]) - 1),  
                                                #Removes '#' and everything that comes after it from adress
                                                
                                                x["City"], 
                                                "NY", 
                                                x["Zip.Code"], 
                                                sep = ", ")),          #Then make full address as above
                        trystreet2coordinates)
    
    lat.is.nan[which(is.nan(lat.is.nan$latitude)),'latitude'] = temp[1,]
    lat.is.nan[which(is.nan(lat.is.nan$longitude)),'longitude'] = temp[2,]
}

####################################
##DEALING WITH THE REST MANUALLY
###################################
ci.no.po[which(ci.no.po$Organization.Name == "Brooklyn Music School"),c("latitude", "longitude")] = latlng("126 St Felix St, Brooklyn, NY 11217")
ci.no.po[which(ci.no.po$Organization.Name == "Causa Artium Limited"),c("latitude", "longitude")] = lat.is.nan[which(lat.is.nan$Organization.Name == "Causa Artium Limited"),c("latitude", "longitude")]
ci.no.po[which(ci.no.po$Organization.Name == "Queen's Theatre Company, Inc."),c("latitude", "longitude")] = c(40.744483, -73.844431)
ci.no.po[which(ci.no.po$Organization.Name == "Riverside Opera Company, Inc."),c("latitude", "longitude")] = lat.is.nan[which(lat.is.nan$Organization.Name == "Riverside Opera Company, Inc."),c("latitude", "longitude")]
ci.no.po[which(ci.no.po$Organization.Name == "Seaside Summer Concert Series, Inc."),c("latitude", "longitude")] = lat.is.nan[which(lat.is.nan$Organization.Name == "Seaside Summer Concert Series, Inc."),c("latitude", "longitude")]
ci.no.po[which(ci.no.po$Organization.Name == "Standby Program, Inc."),c("latitude", "longitude")] = latlng("143-62 Sanford Ave, Queens, NY 11355")
ci.no.po[which(ci.no.po$Organization.Name == "Culturehub, Inc."),c("latitude", "longitude")] = lat.is.nan[which(lat.is.nan$Organization.Name == "Culturehub, Inc."),c("latitude", "longitude")]
ci.po = rbind(ci.po, ci.no.po[which(ci.no.po$Organization.Name == "Treehouse Shakers, Inc."),1:8])
ci.no.po = ci.no.po[which(ci.no.po$Organization.Name != "Treehouse Shakers, Inc."),]
ci.no.po[which(ci.no.po$Organization.Name == "Trustees of Columbia University in the City of New York"),c("latitude", "longitude")] = latlng("2960 Broadway, New York, NY 10027")
ci.no.po[which(ci.no.po$Organization.Name == "Variations Theatre Group, Inc."),c("latitude", "longitude")] = latlng("21-28 45th Rd, Queens, NY 11101")
ci.no.po[which(ci.no.po$Organization.Name == "Education Through Music, Inc."),c("latitude", "longitude")] = c(40.751660, -73.976544)
ci.no.po[which(ci.no.po$Organization.Name == "Electronic Music Foundation, Inc."),c("latitude", "longitude")] = latlng("307 Seventh Ave, New York, NY 10001")
ci.no.po[which(ci.no.po$Organization.Name == "York Theatre Company, Inc."),c("latitude", "longitude")] = latlng("619 Lexington Avenue, New York, NY 10022")
ci.no.po[which(ci.no.po$Organization.Name == "Young Concert Artists, Inc."),c("latitude", "longitude")] = c(40.766488, -73.982254)
ci.no.po[which(ci.no.po$Organization.Name == "thingNY, Inc."),c("latitude", "longitude")] = latlng("21-38 Crescent Street, Astoria, NY")
ci.no.po[which(ci.no.po$Organization.Name == "Godwin Ternbach Museum"),c("latitude", "longitude")] = c(40.736205, -73.817314)
ci.no.po[which(ci.no.po$Organization.Name == "Hell's Kitchen, NY Chapter of SPEBSQSA, Inc."),c("latitude", "longitude")] = latlng("410 W 40th St, New York, NY 10018")
ci.po = rbind(ci.po, ci.no.po[which(ci.no.po$Organization.Name == "IndyKids"),1:8])
ci.po[which(ci.po$Organization.Name == "IndyKids"), "Address"] = "P.O. Box 2281"
ci.no.po = ci.no.po[which(ci.no.po$Organization.Name != "IndyKids"),]
ci.no.po[which(ci.no.po$Organization.Name == "International Arts Movement, Inc."),c("latitude", "longitude")] = latlng("38 W 39th St, New York, NY 10018")
ci.no.po[which(ci.no.po$Organization.Name == "Marilyn Horne Foundation, Inc."),c("latitude", "longitude")] = c(40.766488, -73.982254)
ci.po = rbind(ci.po, ci.no.po[which(ci.no.po$Organization.Name == "Migrating Forms Festival, Inc"),1:8])
ci.no.po = ci.no.po[which(ci.no.po$Organization.Name != "Migrating Forms Festival, Inc"),]
ci.no.po[which(ci.no.po$Organization.Name == "No Longer Empty, Inc."),c("latitude", "longitude")] = c(40.761308, -73.967818)
ci.no.po[which(ci.no.po$Organization.Name == "Opera Company of Brooklyn Association"),c("latitude", "longitude")] = latlng("33 Indian Road, New York, NY 10034")
ci.no.po[which(ci.no.po$Organization.Name == "Peccadillo Theater Company, Inc."),c("latitude", "longitude")] = latlng("23 W 46th St, New York, NY 10036")
ci.no.po[which(ci.no.po$Organization.Name == "American Museum of Natural History"),c("latitude", "longitude")] = c(40.781275, -73.973473)
ci.no.po[which(ci.no.po$Organization.Name == "America SCORES New York"),c("latitude", "longitude")] = latlng("520 8th Ave, New York, NY 10018")
ci.no.po[which(ci.no.po$Organization.Name == "Astoria Performing Arts Center"),c("latitude", "longitude")] = latlng("34-12 36th St, Queens, NY 11106")
ci.no.po[which(ci.no.po$Organization.Name == "Variety Boys & Girls Club of Queens, Inc."),c("latitude", "longitude")] = lat.is.nan[which(lat.is.nan$Organization.Name == "Variety Boys & Girls Club of Queens, Inc."),c("latitude", "longitude")]
ci.no.po[which(ci.no.po$Organization.Name == "Westbeth Artists Residents Council"),c("latitude", "longitude")] = lat.is.nan[which(lat.is.nan$Organization.Name == "Westbeth Artists Residents Council"),c("latitude", "longitude")]
ci.no.po[which(ci.no.po$Organization.Name == "Martin Luther King, Jr. Concert Series, Inc."),c("latitude", "longitude")] = lat.is.nan[which(lat.is.nan$Organization.Name == "Martin Luther King, Jr. Concert Series, Inc."),c("latitude", "longitude")]
##################################
##################################
##################################

#############################################
###### GETTING ZIPCODE GEOSPACTIAL DATA
############################################
#read in shape data, its big

dat=readOGR(dsn=path.expand("cb_2014_us_zcta510_500k"),layer="cb_2014_us_zcta510_500k")
data(zip_codes)

#First filter to NYS (10004 is incorrectly listed as in NJ)
nyc_zips=filter(zip_codes, state == "NY" | zip == 10004)

#Filter to NYC zipcodes
nyc_zips = filter(nyc_zips, city == "New York" | zip == 10004 |
                    city == "Brooklyn" | city == "Staten Island" | city == "Bronx" | 
                    (zip < 11500 & zip > 11100) | zip < 11700 & zip >= 11690)

#filter to nyc zipcodes
subdat=dat[dat$GEOID10 %in% nyc_zips$zip,]

#######################################
#Transformation of data
#Taken from: https://github.com/nycdatasci/bootcamp004_project/tree/master/Project2-Shiny/chris_redino

# ----- Transform to EPSG 4326 - WGS84 (required)
subdat=spTransform(subdat, CRS("+init=epsg:4326"))

# ----- save the data slot
subdat_data=subdat@data[,c("GEOID10", "ALAND10")]

# ----- to write to geojson we need a SpatialPolygonsDataFrame
subdat=SpatialPolygonsDataFrame(subdat, data=subdat_data)
writeOGR(subdat,dsn=path.expand("nyc_zip_mapping"),"nyc_zip_data",driver="ESRI Shapefile")#this wsrites a much smaller polygon shape file so that I don't need to upload as much.



####################



#nyc contains the geospactial data for zipcodes in nyc
nyc <- readOGR("nyc_zip_mapping/nyc_zip_data.shp",
               layer = "nyc_zip_data", verbose = FALSE)

##############################################################
#############################################################
#############################################################

###################################
#Add discipline data to nyc (the geospatial data of nyc)

#convert discipline.zip (this was count of each discipline in each zipcode) to short form.
# If a zipcode doesn't have a row for a given discipline, it means that it has no institutions of that
# discipline, so we fill in a zero.
dzl = dcast(discipline.zip, formula = Zip.Code ~ Discipline)
dzl = sapply(dzl, function(x) ifelse(is.na(x), 0, as.numeric(x)))
dzl = as.data.frame(dzl)
dzl$Zip.Code = as.character(dzl$Zip.Code)

#Join to discipline data and the geospactial data
tempdata = nyc@data
temp = left_join(tempdata, dzl, by = c("GEOID10" = "Zip.Code"))
temp = as.data.frame(sapply(temp, function(x) ifelse(is.na(x), 0, as.numeric(x))))


#####################
#Add deographic data to geospatial dataframe

# zipdemo contains demographic infomation for each zipcode.
# Filter to nyc zipcodes and fix population column (it was a string with columns)
zipdemo = read.csv("ZCTADemographics.csv", stringsAsFactors = F)
zipdemo = filter(zipdemo, ZCTA5 %in% nyc_zips$zip)
zipdemo$POPULATION = as.numeric(gsub(",", "", zipdemo$POPULATION))

#Only add columns with zipcode, landarea, housing units, population, latitude, and longitude.
# I don't use the housing units data
#Changing here you could add other info
zipdemo = zipdemo[,c(1,4,6,7,8,9)]

#Join demographic data and geospatial data
zipdemo$ZCTA5 = as.numeric(zipdemo$ZCTA5)
temp2 = left_join(temp, zipdemo, by = c("GEOID10" = "ZCTA5"))
nyc@data = temp2

#Color palette for the mapping of institutions
pal <- colorFactor("Set1", domain = ci.no.po$Discipline)

save.image('PreparedData.RData')

