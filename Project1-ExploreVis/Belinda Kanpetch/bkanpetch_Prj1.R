#### ------ NOTES ON VARIABLES ------###
# OBJECTID_1:
# Sequential unique whole numbers that are
# automatically generated.

# TREEID:
# Original ID given when record was entered
# From paper data sheets.

# ONSTREET:
# Name of street tree is located on.

# PARITY :
# Describes the address range of the block.
# An address range of odd parity consits of all odd house
# numbers along the ONSTREET. An even-parity range
# consits of all even house numbers along the ONSTREET.

# CROSSTREE:
# Cross Street at low address end.

# CROSSSTR_1:
# Cross street at high address end.

# BUILDINGNU
# Building number the closest building to the tree.

# BUILDINGST
# Street the building closest to the tree is on.

# TREELOCATI
# Describes the tree in relation to BUILDINGNU.
# Possible values are 1 - Front, 2 - Side,
# 3 - Rear, 4 - Across, 5 - Adjacent, 6 - Median,
# 7 - Side/Across, 8 - Side/Median, 9 Assigned.

# SITE
# Number tree at address, in order of ascending address

# TREEPIT
# Pit type. Possible values are:
# 1 - sidewalk, 2 - continuous, 3 - lawn.

# TREECONDIT
# Condition of tree. Possible values are:
# 1 - Excellent. Full, well balanced crown and limb structure;
# leaves normal size color; no dead or broken branches;
# trunk solid, bark intact.

# 2 - Good. Crown uneven or misshapen; some mechanical damage
# to bark or trunk; some signs of insects or disease;
# leaves somewhat below normal size and quantity;
# some dead or broken branches (less than half of the tree).

# 3 - Poor: large dead limbs with over one-half of the tree
# already dead or removed; large cavities; drastic deformities;
# leaves significantly below normal size and quantity;
# severe insect or disease damage.

# 4 - Dead. Dead tree; leaves absent; twigs brittle.

# 5 - Shaft. All branches removed; trunk left standing;
# sprouts may or may not be evident.

# 6 - Stump. Stump shorter than breast height;
# leaves entirely absent or present only on stump sprouts.

# 7 - Empty Tree Pit. Nothing there at all.


# DIAMETER
# Diameter at Breast Height. Diameter of tree at 4.5 feet
# from the ground. For trees with multiple trunks that
# divided below 3 feet from the ground, the diameters of
# each stem were added together. Stumps were measured
# across the face of the stump at its widest point.

# OBJECTID
# Legacy ID created during geocoding process.

# COMMDIST
# community board
### --------------- End notes on trees dataset ----------###



### --------------- Loading libraries ----------###

setwd('~/DS/BootCamp/prj1/')
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(maptools)
library(ggmap)
library(gpclib)
library(rgeos)
library(tidyr)

### --------------- Loading files ----------###

# loading the Manhattan Tree census 
m_tree = read.csv('ManhattanTree.csv')

# loading the species tolerance csv
species_tol = read.csv('./nyc-dpr-tree-species-tolerance-guide_12-1-14.csv')



#????????????????????????????????????????????????????????????????????????????????????????????#
#?????????????????????? WHAT IS THE DISTRIBUTION OF TREES PER SPECIES? ??????????????????????#
# check levels
levels(m_tree$SPECIES)
# undefinable species level "0" and ""

#number of observations by species
table(m_tree$SPECIES)
# anomolies are '0' = 64, "" = 2221


# ---------- THIS CHUNK DEALS WITH ANOMOLY VALUES IN SPECIES ----------#
### Trying what I worked out with Luke
#Coping file in case I f-it up.
m_tree_2 = m_tree
class(m_tree_2$SPECIES)
levels(m_tree_2$SPECIES)

# Adding a new logical vector if SPECIES is anomoly
m_tree_2$missing_species = (m_tree_2$SPECIES == '') | (m_tree_2$SPECIES == '0')
head(m_tree_2)

# Create new col NEW_SPECIES and copy data from SPECIES
m_tree_2$NEW_SPECIES = m_tree_2$SPECIES

# if $missing == T then put 0 in NEW_SPECIES
m_tree_2$NEW_SPECIES[ m_tree_2$missing_species ] = 0

# converting the level from '0' to 'missing'.  I can't add a level but I can change it.
levels(m_tree_2$NEW_SPECIES)[2] = 'MISSING'


# ------------------ THIS CHUNK MANIPULATES THE DATA FRAME --------------------#
by_species = group_by(m_tree_2, NEW_SPECIES) %>%
  summarise(TOTAL = n()) %>%
  arrange(TOTAL)

by_species$ordered = reorder(by_species$NEW_SPECIES, by_species$TOTAL)

by_species = mutate(by_species, percent = (by_species$TOTAL/nrow(m_tree_2))*100)

# --------------------------- VISUALIZING ----------------------------#
all_specs = ggplot(by_species, aes(x = ordered, y = TOTAL)) +
  geom_bar(stat = 'identity') + coord_flip() + theme(axis.text = element_text(size=9)) +
  labs(title = 'Distribution of Tree Species in Manhattan', x = 'Species', y = 'Total')

all_specs
# This barplot shows the dramatic spread between species

## ggplot(by_species, aes(x=1)) + geom_bar(aes(fill = 'TOTAL'

summary(by_species)
# looking for the inter-quartile range (IQR) to select the most common species
#  NEW_SPECIES     TOTAL         
# MISSING: 1   Min.   :    1.00  
# AC     : 1   1st Qu.:    3.75  
# ACCA   : 1   Median :   35.00  
# ACGI   : 1   Mean   :  561.52  
# ACNE   : 1   3rd Qu.:  181.75  
# ACPA   : 1   Max.   :11529.00  
# (Other):86             


# use the values >= 1st and <= 3rd to filter out common species
# use min > 1st and 3rd > max to filter out outliers
common_species = filter(by_species, (TOTAL >= 3.75) & (TOTAL <= 181.54))
nrow(common_species)
#ordering by count
common_species$ordered = reorder(common_species$NEW_SPECIES, common_species$TOTAL)

#plotting common species
plot_cmn_spec = ggplot(common_species, aes(x = ordered, y = TOTAL)) +
  geom_bar(stat = 'identity') + coord_flip() +
  labs(title = "Distribution of Common Tree Species in Manhattan (IQR)", x = 'Species', y = 'Total')
plot_cmn_spec

# plotting above 3rd quartile
abv3rdQ = filter(by_species, (TOTAL > 181.75))
nrow(abv3rdQ)
# ordering by count
abv3rdQ$ordered = reorder(abv3rdQ$NEW_SPECIES, abv3rdQ$TOTAL)

plot_abv3rd = ggplot(abv3rdQ, aes(x = ordered, y = TOTAL)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Distribution of Tree Species in Manhattan (4th Quartile)',
       x = 'Species', y = 'Total')
plot_abv3rd


# plotting below 1st Quartile
blw_1stQ = filter(by_species, (TOTAL < 3.75))
nrow(blw_1stQ)
blw_1stQ$ordered = reorder(blw_1stQ$NEW_SPECIES, blw_1stQ$TOTAL)

plot_blw_1st = ggplot(blw_1stQ, aes(x = ordered, y = TOTAL)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Distribution of Tree Species in Manhattan (1st Quartile)', x = 'Species', y = 'Total')
plot_blw_1st



#????????????????????????????????????????????????????????????????????????????????????????????#
#?????????????????????? WHAT IS THE DISTRIBUTION OF TREES PER COMMUNITY BOARD? ??????????????#
# WHAT IS THE DISTRIBUTION OF TREES PER CONDITION BY COMM DIST?
# WHAT'S THE OVERALL HEALTH OF TREES IN MANHATTAN?2
# WHAT NEIGHBORHOODS ARE ASSOCIATED WITH WHICH CB?

# looking at the data there are two outlier CBs, 164 and 0
# looking at all the observations COMMDIST = 164
table(m_tree_2$COMMDIST)
#'0' = 466, '164' = 21

#-------- THIS CHUNK DEALS WITH ANOMOLY COMMDIST --------------#
# create new logical col vector if COMMDIST = 0 or 164
m_tree_2$missing_cd = (m_tree_2$COMMDIST == '0') | (m_tree_2$COMMDIST == '164')
                                                    
# copy COMMDIST and create NEW_CD
m_tree_2$NEW_CD = m_tree_2$COMMDIST

# if $missing == T then put 0 in NEW_CD where 100 == missing
m_tree_2$NEW_CD[ m_tree_2$missing_cd ] = 100

# reorder by total
by_cd = group_by(m_tree_2, NEW_CD, TREECONDIT, NEW_SPECIES) %>%
  summarise(TOTAL = n()) %>%
  arrange(TOTAL)

by_cd$ordered = reorder(by_cd$NEW_CD, by_cd$TOTAL)
by_cd$TREECONDIT = as.factor((by_cd$TREECONDIT))

#----------------- VISUALIZING TREE CONDITION BY CD -------------------#
bar_cond_by_cd = ggplot(by_cd, aes(x = NEW_CD, y = TOTAL)) +
  geom_bar(stat = 'identity', aes(fill = TREECONDIT)) +
  labs(title = 'Distribution of Tree Species by Community Board in Manhattan',
       x = 'Community Board', y = 'Total') +
  scale_fill_brewer(palette = 'Spectral',
                    labels = c('Excellent', 'Good',
                               'Poor', 'Dead', 'Shaft', 
                               'Stump', 'Empty Tree Pit'))
bar_cond_by_cd



bar_species_cd =
  ggplot(by_cd, aes(x = NEW_SPECIES, y = TOTAL)) +
  geom_bar(stat = 'identity', aes(fill = NEW_SPECIES)) +
  facet_grid(NEW_CD~.) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5),
        legend.position = 'none') +
  labs(title = 'Street Tree Distribution by Community District and Species',
       x = 'Species', y = 'Total')

bar_species_cd



#????????????????????????????????????????????????????????????????????????????????????????????#
# ????????????????? SPECIES AND PER CAPITA INCOME BY CD OF MANHATTAN? ???????????????????????#
mh_eco = read.csv('Demographics_20and_20profiles_20at_20the_20Public_20Use_20Microdata_20Area_20_PUMA_20__20subarea_20level/MH_ECO.csv')
percapita = mh_eco[112, c(2,6,10,14,18,22,22,26,30,34,38) ] 
percapita = percapita[-7]
percapita = rename(percapita, cd_1_2 = X,
                   cd_3 = X.4,
                   cd_4_5 = X.8,
                   cd_6 = X.12,
                   cd_7 = X.16,
                   cd_8 = X.20,
                   cd_9 = X.24,
                   cd_10 = X.28,
                   cd_11 = X.32,
                   cd_12 = X.36)

percapita = gather(percapita, cd, percap)




#????????????????????????????????????????????????????????????????????????????????????????????#
# ??????????????????????????? VISUALIZE POINTS ON A CD MAP? ?????????????????????????????????#

# selecting the variables that are useful
species_tol_2 = subset(species_tol, select = c(1:22))

m_tree_2 = rename(m_tree_2, CODE = SPECIES)


# need to seperate long, lat into seperate cols
# write a function that splits the string
longlatsplt = function(string){
    split = unlist(strsplit(gsub("[)]","",gsub("POINT [(]", "", string)), ' '))
    return(split)
}
# applying that function to the column
m_tree_2$the_geom = sapply(m_tree_2$the_geom, longlatsplt)
# adding the long and lat cols

for (i in 1:nrow(m_tree_2)) {
  m_tree_2$long[i] = m_tree_2$the_geom[[i]][1]
  m_tree_2$lat[i] = m_tree_2$the_geom[[i]][2]
}

#------------ trying to plot points in scatterplot---------------# NOT WORKING
testset = m_tree_2[ ,24:25]
trpts = ggplot(data = testset, aes(x = long, y = lat)) +
  geom_point()

trpts

qplot(m_tree_2$long, m_tree_2$lat)



#------------ plotting 5 BOROUGH MAP ------------------#
cdshp = readShapePoly('geo_export_20829db0-f8c3-4f3f-bab4-fc4ee79f9ccd.shp')
cdshp.f = fortify(cdshp)

map_borough = ggplot(cdshp.wgs84, aes(long, lat, group = group)) +
  geom_polygon(aes(colour = 'green')) +
  coord_map() +
  theme_bw() +
  xlab('') + ylab('') +
  ggtitle('Community District Map of Manhattan') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color='none')

map_manhtn = ggplot(cdshp.wgs84, aes(long, lat, group = group)) +
  geom_polygon(aes(colour = 'green')) +
  coord_map(xlim = c(-74.05, -73.9), 
            ylim = c(40.68, 40.9)) +
  theme_bw() +
  xlab('') + ylab('') +
  ggtitle('Community District Map of Manhattan') +
  scale_fill_brewer(palette = 'Greens') +
  guides(color='none')

map_manhtn 

#### What would I look at next?
# WHAT ARE THE COMMON NAMES FOR EACH CODE?
# WHAT IS THE DISTRIBUTION OF TREE SIZES?

# CROSS REFERENCE TREE DENSITY BY CB AND MEDIAN INCOME?
# WHAT ARE THE COMPARISONS BETWEEN NEIGHBORHOOD?

# WHAT IS THE MOST COMMON TREEPIT TYPE?

# I attempted this in various ways
# I downloaded the CD shapefile and was able to get
# cd map of all 5 boroughs
# using these libraries
# library(maptools)
# library(ggmap)
# library(gpclib)
# library(rgeos)

# I was not able to get the tree observations onto the
# cd map

# leaflet didn't work for me... not sure if it was
# because of my computer or if it was because 
# the df has over 50k observations

# qplot also would not plot more than 500 points





