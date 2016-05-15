########## Load all required libraries ##########
library(plyr)
library(dplyr)
library(shiny)
library(googleVis)
library(datasets)
library(shinythemes)
library(dygraphs)
library(Hmisc)


########## Load data sets ##########
dat2014 <- readRDS("data/dat2014.rds")
dat2012 <- readRDS("data/dat2012.rds")
dat2008 <- readRDS("data/dat2008.rds")
spop <- readRDS("data/spop.rds")


########## Define variables to be used in lists for choices ##########
race <- c(
  "All" = 0,
  "White" = 1, 
  "Black" = 2,
  "Asian" = 4,
  "American Indian/Alaska Native" = 3,
  "Native Hawaiian/Other Pacific Islander" = 5,
  "Two or more" = 6,
  "Not Hispanic" = 7,
  "Hispanic" = 8
)

sex <- c(
  "Both" = 0,
  "Male" = 1,
  "Female" = 2
)
  
population <- c(
  "Total" = "total population",
  "Births" = "births",
  "Deaths" = "deaths",
  "Net Migration" = "migration"
)

change <- c(
  "Percent Difference" = "percent",
  "Total Difference" = "total"
)