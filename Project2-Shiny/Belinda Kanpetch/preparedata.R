library(rgdal)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(maptools)
library(ggmap)
library(gpclib)
library(rgeos)
library(tidyr)
library(foreign)
library(rgdal)
library(leaflet)
library(colorRamps)
library(grDevices)


#loading in all of the data
m_tree = read.csv('Census/ManhattanTree.csv')
bx_tree = read.csv('Census/BronxTree.csv')
bk_tree = read.csv('Census/BrooklynTree.csv')
qn_tree = read.csv('Census/QueensTree.csv')
si_tree = read.dbf('Census/Street Tree Census (Bronx)/geo_export_95ae580e-feaf-4459-bc0a-772394371e50.dbf')
tolerance = read.csv('Assets/nyc-dpr-tree-species-tolerance-guide_12-1-14.csv')

# reading in RDS
all_trees = readRDS('DS/bootcamp/prj2/all_trees.rds')

#making sure all of the column names match & are in the same order before binding
colnames(m_tree)
colnames(bk_tree)
colnames(qn_tree)
colnames(bx_tree)
colnames(si_tree)
# si_tree is in a different order and named differently
# I need to reorder the columns but this dataset does not have geompoints

# Looking specifically at Staten Island.
# The data from NYC Open Data came in with geospatial data and a .dbf.
# When I read in the dbf file there are 65048 observations but it's missing the geoms
# or long/lat of each tree location.
# When I read in the shape file there are 101839 observations and the coordinates are
# not anything that look related to a typical long/lat.
# !!!!!! I will table this for now and think about how to include data from SI.!!!!!!
#changing the column names to lowercase


colnames(m_tree) = tolower(colnames(m_tree))
colnames(bk_tree) = tolower(colnames(bk_tree))
colnames(qn_tree) = tolower(colnames(qn_tree))
colnames(bx_tree) = tolower(colnames(bx_tree))

colnames(bx_tree)[1] = colnames(bk_tree)[1]
colnames(m_tree)[1] = colnames(bk_tree)[1]

# combining all data frames into one
all_trees = rbind(m_tree, 
                  bk_tree,
                  bx_tree,
                  qn_tree)


saveRDS(all_trees, file = 'all_trees.rds')

# Looing for NA's or weird missing values.
# In species col: 31960 empty vectors; 273 with species 0
# zipcode: 20637 NAs
# treepit: 3 NAs
# columns to be changed into factors:
# commdist, zipcode, treecondit, treepit, site, treelocati, treeid, objectid_1

# munging the species tolerance dataset
names(tolerance)
# removing columns that are not needed
tolerance=subset(tolerance, select = c(1:22))
n_distinct(tolerance$CODE)
#251 distinct species
n_distinct(all_trees$species)
#168 distince species


##################THIS CHUNK DEALS WITH MISSING VALUES ##################
################# Community Districts ############# 
# create new logical col vector if COMMDIST = 0 or 164
all_trees$missing_cd = (all_trees$commdist == '0') | (all_trees$commdist == '164')

# copy COMMDIST and create NEW_CD
all_trees$NEW_CD = all_trees$commdist

# if $missing == T then put 0 in NEW_CD where 100 == missing
all_trees$NEW_CD[ all_trees$missing_cd ] = 100

###################### Species ###################
# Adding a new logical vector if SPECIES is anomoly
all_trees$missing_species = (all_trees$species == '') | (all_trees$species == '0')

# Create new col NEW_SPECIES and copy data from SPECIES
all_trees$new_species = all_trees$species

# if $missing == T then put 0 in NEW_SPECIES
all_trees$new_species[ all_trees$missing_species ] = 0

# converting the level from '0' to 'missing'.  I can't add a level but I can change it.
levels(all_trees$new_species)[2] = 'MISSING'


############ Addressing the long / lat column
############ Spliting the character string into long / lat columns

# this function splits the character string
longlatsplt = function(string){
  split = unlist(strsplit(gsub("[)]","",gsub("POINT [(]", "", string)), ' '))
  return(split)
}
# applying that function to the column
all_trees$the_geom = sapply(all_trees$the_geom, longlatsplt)


# adding the long and lat cols
for (i in 1:nrow(all_trees)) {
  all_trees$long[i] = all_trees$the_geom[[i]][1]
  all_trees$lat[i] = all_trees$the_geom[[i]][2]
}



###########
# taking out all of obs with missing long/lat.
all_trees = all_trees[(complete.cases(all_trees$long) &
                         complete.cases(all_trees$lat)), ]
all_trees$lat <- as.numeric(all_trees$lat)
all_trees$long <- as.numeric(all_trees$long)


saveRDS(save_to_no_missing_rds, file = 'no_missing.rds')
saveRDS(tolerance, file = 'tolerance.rds')

all_trees = readRDS('no_missing.rds')

#################################################
##### cross referencing genus, species, cultivar
##### not working.
length(unique(tolerance$CODE))
#251 unique species codes

# returns vector of positions
all_trees$match = match(all_trees$new_species, tolerance$CODE, nomatch = NA, incomparables = NULL)

# seperate the code character string into two characters and put into respective column
for (i in 1:nrow(all_trees)){
  if (all_trees$new_species[i] == 'MISSING'){
    all_trees$genus_cd[i] = NA
    all_trees$specs_cd[i] = NA
    all_trees$cultivar_cd[i] = NA
  } else {
    div_code = substring(all_trees$new_species[i], seq(1,5,2), seq(2,6,2))
    all_trees$genus_cd[i] = div_code[1]
    all_trees$specs_cd[i] = div_code[2]
    all_trees$cultivar_cd[i] = div_code[3]
  }
}

for (i in nrow(all_trees)) {
  if (!is.na(all_trees$match[i])) {
    all_trees$genus[i] = tolerance$Genus[all_trees$match[i]]
    all_trees$species_t[i] = tolerance$Species[all_trees$match[i]]
    all_trees$cultivar[i] = tolerance$Cultivar[all_trees$match[i]]
    all_trees$urbtol[i] = tolerance$Proven.Urban.Tolerance[all_trees$match[i]]
    all_trees$drought_tol[i] = tolerance$Drought.Tolerance[all_trees$match[i]]
    all_trees$aerosol_tol[i] = tolerance$Aerosol.Salt.Tolerance[all_trees$match[i]]
    all_trees$soil_tol[i] = tolerance$Soil.Salt.Tolerance[all_trees$match[i]]
    all_trees$approved_nyc[i] = tolerance$Approved.For.NYC.Flood.Zone[all_trees$match[i]]
  }
}
