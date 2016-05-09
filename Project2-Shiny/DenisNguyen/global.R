# Load all libraries used

library(dplyr)
library(shiny)
library(googleVis)
library(datasets)
library(shinythemes)
library(dygraphs)

dat2014 <- readRDS("data/dat2014.rds")

race <- c(
  "All" = 0,
  "White" = 1, 
  "Black" = 2,
  "Asian" = 4,
  "American Indian/Alaska Native" = 3,
  "Native Hawaiian/Other Pacific Islander" = 5,
  "Two or more" = 6
)

sex <- c(
  "All" = 0,
  "Male" = 1,
  "Female" = 2
)
  
population <- c(
  "Total" = "total population",
  "Births" = "births",
  "Deaths" = "deaths",
  "Migrants" = "migrants"
)