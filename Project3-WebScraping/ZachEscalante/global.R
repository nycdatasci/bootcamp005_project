# install.packages("leaflet")
#install.packages("shiny")
#install.packages("rgdal")
#install.packages("RColorBrewer")
#install.packages("ggplot2")
#install.packages("dplyr")

library(maptools)
library(shiny)
library(leaflet)
library(rgdal)
# library(RColorBrewer)
# library(dplyr)
# library(ggplot2)

### loading community district polygons
cdshp_nyc = readShapePoly('/Users/zacharyescalante/Desktop/Shiny/NYC\ Open\ Data/geo_export_cf1204bd-a6cb-4ca0-ad78-698ffb949d1f.shp')
cdshp_nyc$ntaname= as.character(cdshp_nyc$ntaname)
df <- read.csv('/Users/zacharyescalante/Desktop/test.csv', stringsAsFactors = FALSE)  #Read in the data which I scrappd from Streeteasy

#Set Soho, Tribeca, Little Italy and Civic Center to = 'SoHo-TriBeCa-Civic Center-Little Italy'
df$Neighborhood[df$Neighborhood=='Soho'] = 'SoHo-TriBeCa-Civic Center-Little Italy'
df$Neighborhood[df$Neighborhood=='Tribeca'] = 'SoHo-TriBeCa-Civic Center-Little Italy'
df$Neighborhood[df$Neighborhood=='Little Italy'] = 'SoHo-TriBeCa-Civic Center-Little Italy'
df$Neighborhood[df$Neighborhood=='Civic Center'] = 'SoHo-TriBeCa-Civic Center-Little Italy'


str(df)


