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

#Data wrangling for map plot
health2 = flipjoin("data/health.csv")
socioeconomic2 = flipjoin("data/socioeconomic.csv")
socioeconomic2$POVRATE10 = as.numeric(as.character(socioeconomic2$POVRATE10))

##read data in for non map data
#Data can be found at http://www.ers.usda.gov/data-products/
#food-environment-atlas/data-access-and-documentation-downloads.aspx
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
                    "CONVSPTH12", "FFRPTH12", "FOODINSEC_10_12", "FSRPTH12",
                    "DIRSALES_FARMS07", "FMRKTPTH13", "VEG_FARMS07", "PCT_DIABETES_ADULTS10",
                    "PCT_OBESE_ADULTS10", "PCT_HSPA09", "RECFACPTH12", "MEDHHINC10", 
                    "POVRATE10", "PCT_65OLDER10", "PCT_18YOUNGER10")]

#Creating a table just to be displayed in data tab
tabledb = fulldb
names(tabledb) = c("County ID", "State", "County", "Population low access to store percent 2010",
                   "Grocery stores div 1000 pop 2012", "Convenience stores div 1000 pop 2012",
                   "Fastfood restaurants div 1000 pop 2012", "Household food insecurity percent 201012",
                   "Full-service restaurants/1,000 pop, 2012", "Farms with direct sales 2007", "Farmers markets div 1000 pop 2013", 
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

#Stepwise regression was used to predict obesity rate. Starting from saturated model
#variables not found significant were removed using stepwise regression.
data = fulldb[,-c(1:3)]
complete.data = data[complete.cases(data),]
model.saturated = lm(PCT_OBESE_ADULTS10 ~ ., data = complete.data)
model.empty = lm(PCT_OBESE_ADULTS10 ~ 1, data = complete.data)
scope = list(lower = formula(model.empty), upper = formula(model.saturated))
backwardAIC = step(model.saturated, scope, direction = "backward", k = 2)

#Used to predict obesity rate with multiple linear regression
predfunc <- function(GROC, Conv, FF, LACCESS, MEDHHIN, PCT18, FOODINS, FARMRT, 
                     VEGFARM, DIABETE, HSACT,POVRT, PCT65, RECFAC, Full) {
    newdata = data.frame(PCT_LACCESS_POP10 = LACCESS, GROCPTH12 = GROC, CONVSPTH12 = Conv,
                         FFRPTH12 = FF, FOODINSEC_10_12 = FOODINS, VEG_FARMS07 = VEGFARM,
                         FMRKTPTH13 = FARMRT,  PCT_DIABETES_ADULTS10 = DIABETE, FSRPTH12 = Full,
                         PCT_HSPA09 = HSACT, MEDHHINC10 = MEDHHIN, POVRATE10 = POVRT,
                         PCT_65OLDER10 = PCT65, PCT_18YOUNGER10 = PCT18, RECFACPTH12 = RECFAC)
    predict(backwardAIC, newdata, interval = "prediction")
}

#Used for slider bar for prediction, low bounds is mean - 3*standard dev 
rlow <- function(xinput) {
    if (mean(xinput, na.rm = T) > 3*sd(xinput, na.rm = T)) {
        round(mean(xinput, na.rm = T)-3*sd(xinput, na.rm = T),2)
    } else {
        print(0)
    }
}   
#Used for slider bar for prediction, high bounds is mean + 3*standard dev 
rhi <- function(xinput) {
    round(mean(xinput, na.rm = T)+3*sd(xinput, na.rm = T),2)
}   
#Used for slider bar for prediction, starting value is mean 
valme <- function(xinput) {
    round(mean(xinput, na.rm = T),2)
}   


coef = summary(backwardAIC)$coefficients
coef = as.data.frame(coef)
coefnames = c("Intercept", "Percent low access to store", 
                   "Convenience stores/1,000 pop, 2012", "Fast-food restaurants/1,000 pop, 2012",
                   "Household food insecurity (%)", "Full-service restaurants/1,000 pop, 2012",
                   "Farmers' markets/1,000 pop, 2013", "Vegetable farms, 2007", "Adult diabetes 
                   rate, 2010", "High schoolers physically active (%), 2009", "Rec & fitness 
                   facilities/1,000 pop, 2012", "Median household income, 2010","Poverty rate, 2010", 
                   "% Population 65 years or older, 2010", "% Population under age 18, 2010")
coef = mutate(coef, signif = c("***", "**", ".", "***","***","***", ".", "***","***","***",
                               "*", "***","***","***","***"))
coef = cbind(coefnames, coef)
coef[,2:4] = round(coef[,2:4], 5)
sigcodes = "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"
sumreg = "Residual standard error: 2.549 on 2499 degrees of freedom
Multiple R-squared:  0.6671,	Adjusted R-squared:  0.6652 
F-statistic: 357.6 on 14 and 2499 DF,  p-value: < 2.2e-16"

vifs = car::vif(backwardAIC)

printvifs = as.data.frame(cbind(coefnames[-1], round(vifs, 2)))
names(printvifs) = c("Coefficients", "Variance inflation factors")
summodel = "lm(formula = PCT_OBESE_ADULTS10 ~ PCT_LACCESS_POP10 + CONVSPTH12 + 
    FFRPTH12 + FOODINSEC_10_12 + FSRPTH12 + FMRKTPTH13 + VEG_FARMS07 + 
PCT_DIABETES_ADULTS10 + PCT_HSPA09 + RECFACPTH12 + MEDHHINC10 + 
POVRATE10 + PCT_65OLDER10 + PCT_18YOUNGER10, data = complete.data)

Residuals:
Min      1Q  Median      3Q     Max 
-8.9805 -1.6477 -0.0649  1.7023  9.7435 

Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)           27.6055935  1.2158095  22.706  < 2e-16 ***
PCT_LACCESS_POP10      0.0075410  0.0027659   2.726  0.00645 ** 
CONVSPTH12             0.3373637  0.1887602   1.787  0.07402 .  
FFRPTH12              -0.7974693  0.2033631  -3.921 9.04e-05 ***
FOODINSEC_10_12       -0.3187697  0.0254451 -12.528  < 2e-16 ***
FSRPTH12              -0.6352471  0.1104807  -5.750 1.00e-08 ***
FMRKTPTH13             1.2272627  0.6546975   1.875  0.06097 .  
VEG_FARMS07           -0.0048975  0.0011995  -4.083 4.59e-05 ***
PCT_DIABETES_ADULTS10  1.4571508  0.0327182  44.536  < 2e-16 ***
PCT_HSPA09             0.1058446  0.0212671   4.977 6.90e-07 ***
RECFACPTH12           -1.5409978  0.7541411  -2.043  0.04112 *  
MEDHHINC10            -0.0001268  0.0000102 -12.433  < 2e-16 ***
POVRATE10             -0.1101828  0.0160075  -6.883 7.37e-12 ***
PCT_65OLDER10         -0.3221218  0.0197679 -16.295  < 2e-16 ***
PCT_18YOUNGER10        0.1113533  0.0195692   5.690 1.42e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.549 on 2499 degrees of freedom
Multiple R-squared:  0.6671,	Adjusted R-squared:  0.6652 
F-statistic: 357.6 on 14 and 2499 DF,  p-value: < 2.2e-16"