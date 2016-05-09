library(shiny)
library(maps)
library(mapproj)
library(dplyr)
library(plotly)
library(googleVis)
library(car)
data("county.fips")
#Join data by county indentification numbe (fips) to the county data in the maps package
flipjoin = function(x) {
    y = read.csv(x)
    left_join(county.fips, y, by = c("fips" = "FIPS"))
}


health2 = flipjoin("data/health.csv")
socioeconomic2 = flipjoin("data/socioeconomic.csv")
socioeconomic2$POVRATE10 = as.numeric(as.character(socioeconomic2$POVRATE10))

##read data in for non map data
socioeconomic = read.csv("data/socioeconomic.csv")
socioeconomic$POVRATE10 = as.numeric(as.character(socioeconomic$POVRATE10))
socioeconomic$MEDHHINC10 = as.numeric(as.character(socioeconomic$MEDHHINC10))
health = read.csv("data/health.csv")
access = read.csv("data/access.csv")
stores = read.csv("data/stores.csv")
restaurants = read.csv("data/restaurants.csv")
insecurity = read.csv("data/insecurity.csv")
local = read.csv("data/local.csv")

#Joining all the datatables together by the FIPS, id number of counties
fulldb = full_join(socioeconomic, health, by = "FIPS") %>% 
    full_join(., access, by = "FIPS") %>% full_join(., stores, by = "FIPS") %>%
    full_join(., restaurants, by = "FIPS") %>% full_join(., insecurity, by = "FIPS") %>%
    full_join(., local, by = "FIPS")

#Selecting just the columns of interest.
fulldb = fulldb[, c("FIPS", "State", "County", "PCT_LACCESS_POP10", "GROCPTH12",
                    "CONVSPTH12", "FFRPTH12", "FOODINSEC_10_12", 
                    "DIRSALES_FARMS07", "FMRKTPTH13", "VEG_FARMS07", "PCT_DIABETES_ADULTS10",
                    "PCT_OBESE_ADULTS10", "PCT_HSPA09", "RECFACPTH12", "MEDHHINC10", 
                    "POVRATE10", "PCT_65OLDER10", "PCT_18YOUNGER10")]

#Creating a table just to be displayed in data tab
tabledb = fulldb
names(tabledb) = c("County ID", "State", "County", "Population low access to store percent 2010",
                   "Grocery stores div 1000 pop 2012", "Convenience stores div 1000 pop 2012",
                   "Fastfood restaurants div 1000 pop 2012", "Household food insecurity percent 201012",
                   "Farms with direct sales 2007", "Farmers markets div 1000 pop 2013", 
                   "Vegetable farms 2007", "Adult diabetes rate 2010", "Adult obesity rate 2010",
                   "High schoolers physically active percent 2009", "Recreation and fitness facilities div 1000 pop 2012",
                   "Median household income 2010", "Poverty rate 2010", "Population percent 65 years or older 2010",
                   "Population percent under age 18 2010")

#Generate County map with a few var of interest
percent_map <- function(var, color, legend.title, min = 0, max = 100, name = "") {
    # generate vector of fill colors for map
    shades <- colorRampPalette(c("white", color))(100)
    # constrain gradient to percents that occur between min and max
    var <- pmax(var, min)
    var <- pmin(var, max)
    percents <- as.integer(cut(var, 100,
                               include.lowest = TRUE, ordered = TRUE))
    fills <- shades[percents]
    
    # plot choropleth map
    map("county", fill = TRUE, col = fills, resolution = 0,
        lty = 0, projection = "polyconic",
        myborder = 0, mar = c(0,0,0,0), width = 5, height =4)
    
    # overlay state borders
    map("state", col = "black", fill = FALSE, add = TRUE,
        lty = 1, lwd = 1, projection = "polyconic",
        myborder = 0, mar = c(0,0,0,0), width = 5, height =4)
    title(name)
    
    # add a legend
    inc <- (max - min) / 4
    legend.text <- c(paste0(min, " % or less"),
                     paste0(min + inc, " %"),
                     paste0(min + 2 * inc, " %"),
                     paste0(min + 3 * inc, " %"),
                     paste0(max, " % or more"))
    legend("right",
           legend = legend.text,
           text.font = 12,
           fill = shades[c(1, 25, 50, 75, 100)],
           title = legend.title,
           cex = 1.1)
}

data = fulldb[,-c(1:3)]
complete.data = data[complete.cases(data),]
model.saturated = lm(PCT_OBESE_ADULTS10 ~ ., data = complete.data)
model.empty = lm(PCT_OBESE_ADULTS10 ~ 1, data = complete.data)
scope = list(lower = formula(model.empty), upper = formula(model.saturated))
backwardAIC = step(model.saturated, scope, direction = "backward", k = 2)

predfunc <- function(GROC, Conv, FF, LACCESS, MEDHHIN, PCT18, FOODINS, FARMRT, 
                     VEGFARM, DIABETE, HSACT,POVRT, PCT65, RECFAC) {
    newdata = data.frame(PCT_LACCESS_POP10 = LACCESS, GROCPTH12 = GROC,
                         FFRPTH12 = FF, FOODINSEC_10_12 = FOODINS, VEG_FARMS07 = VEGFARM,
                         FMRKTPTH13 = FARMRT,  PCT_DIABETES_ADULTS10 = DIABETE,
                         PCT_HSPA09 = HSACT, MEDHHINC10 = MEDHHIN, POVRATE10 = POVRT,
                         PCT_65OLDER10 = PCT65, PCT_18YOUNGER10 = PCT18, RECFACPTH12 = RECFAC)
    predict(backwardAIC, newdata, interval = "prediction")
}
    