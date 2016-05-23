library(shiny)

setwd("~/Desktop/shiny_project/")
countrylist <- c("France", "Germany", "Spain", "United Kingdom", "United States",
                 "Australia", "Belgium", "Switzerland", "Portugal", "Italy", "Austria")
country.table <- read.csv("country.table.csv", header = T)
country.table <- country.table[,-1]

category.table <- read.csv("category.table.csv", header = T)
category.table <- category.table[-1]

category <- as.character(category.table[,1])
nutrition <- colnames(country.table)[-1]
nutrition2 <-colnames(topcountry.food.facts)[4:12]
