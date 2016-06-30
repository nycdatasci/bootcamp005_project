library(shiny)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(DT) # library for datatables
library(shinythemes)
library(recommenderlab)

# loading files
load('itembased_model.rda')
objects_info = readRDS('objects_info.rds')
object_info = readRDS('objects_info.rds')
vect_names <- dimnames(visitsCF)
vect_names <- as.character(vect_names[[2]]) #list of all unique objects scanned
objects_info <- objects_info[match(vect_names, as.character(objects_info$id), nomatch = 0), ]
# vCF = read.csv('visits.csv', header = T)