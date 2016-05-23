install.packages("leaflet")
install.packages("rgdal")
install.packages("maptools")
install.packages("dplyr")
library(dplyr)
library(rgdal)
library(leaflet)
library(maptools)

#NYC Open Data w/ coordinates:
cdshp_nyc = readShapePoly('/Users/zacharyescalante/Desktop/Shiny/NYC\ Open\ Data/geo_export_cf1204bd-a6cb-4ca0-ad78-698ffb949d1f.shp')
#Zillow w/ neighborhoods:
cdshp_zillow = readShapePoly('/Users/zacharyescalante/Desktop/Shiny/Zillow/ZillowNeighborhoods-NY.shp')
cdshp_zip = readShapePoly('/Users/zacharyescalante/Desktop/Shiny/Zip\ Codes/ZIP_CODE_040114.shp')
df <- read.csv('/Users/zacharyescalante/Desktop/test.csv', stringsAsFactors = FALSE)  #Read in the data which I scrappd from Streeteasy


m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()

m <- leaflet() %>% 
  addTiles() %>%
  setView(-73.944911, 40.732839, zoom = 11) %>%
  addProviderTiles("Stamen.Toner") %>%
  addPolygons(data = cdshp_nyc)
m

cdshp_nyc$ntaname
str(cdshp_nyc)

data <- filter(df, Neighborhood == "West Village")
sum(data$Price[is.na(data$sq_feet)==FALSE])/sum(data$sq_feet, na.rm = TRUE)
data$bed
nrow(data[data$bed=='studio'])

unique(df$Neighborhood)
#Soho, Little Italy, Tribeca
#SoHo-TriBeCa-Civic Center-Little Italy, Civic Center
df$Neighborhood[df$Neighborhood == 'Soho']
df$Neighborhood[df$Neighborhood=='Soho'] = 'SoHo-TriBeCa-Civic Center-Little Italy'
df$Neighborhood[df$Neighborhood == 'SoHo-TriBeCa-Civic Center-Little Italy']
count(df$bed[df$bed == 'studio'])
sum(df$bed == 'studio')
