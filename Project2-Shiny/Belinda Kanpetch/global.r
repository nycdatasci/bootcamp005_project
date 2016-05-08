library(shiny)
library(leaflet)
library(CartoDB)
library(RColorBrewer)
library(dplyr)
library(markdown)
library(ggplot2)
library(DT)
library(shinythemes)
library(rgdal)
library(maptools)


###############################

###### files for mapping ######
### reading street tree database
all_trees = readRDS('no_missing.rds')

# creating random sample for testing22
randsamp = sample_n(all_trees, 10000)

### loading community district polygons
cdshp = readShapePoly('~/DS/bootcamp/prj2/Assets/geo_export_20829db0-f8c3-4f3f-bab4-fc4ee79f9ccd.shp')

# defining a color palette
pal = colorFactor(palette = topo.colors(168, alpha = 1),
                  domain = all_trees$new_species)

###############################
###### files for graphing #####

### creating table with the rows needed for 
trees = as.data.frame(table(all_trees$new_species, all_trees$borough,
                            all_trees$NEW_CD, all_trees$treecondit))
colnames(trees) = c('Species', 'Borough','Community_District','Tree_Condition', 'Freq')
