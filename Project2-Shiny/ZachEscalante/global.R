library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)


bond.data <- read.csv("clean.data.csv")
#Group the data by tranche and month/year
#clean.data <- group_by(bond.data, Tranche, variable)


