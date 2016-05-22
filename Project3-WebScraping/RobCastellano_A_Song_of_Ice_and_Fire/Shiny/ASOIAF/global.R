library(shiny)
library(ggplot2)
library(plotly)
library(shinythemes)

load("LoadedData.RData")      #This is just all the data from all_chapters.csv and a color palette.

#Rename ChapterName column so that it displays nicely in plotly
names(asoiaf)[which(names(asoiaf) == "ChapterName")] = "Chapter"
