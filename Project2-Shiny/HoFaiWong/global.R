########################################
#### Project 2 - Shiny Visualization####
#### Ho Fai Wong - May 8, 2016      ####
########################################

library(VIM)
library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)
library(googleVis)
library(leaflet)
library(shinythemes)
library(htmltools)

## global.R ##
setwd("~/Desktop/Project 2/world-university-ranking")
source('helpers.r')


#################
####Data load####
#################

#Load interim files for data cleanup
school_and_country_table = read.csv("./data/school_and_country_table.csv", stringsAsFactors = F) %>%
  rename(., university_name = school_name) #Map schools to countries for Shanghai data (provided by Kaggle)
cleanupCountry = read.csv("./data/cleanupCountry.csv", stringsAsFactors = F) #Map schools to countries with missing countries still
cleanupRename = read.csv("./data/cleanupRename.csv", stringsAsFactors = F) #Rename university names for consistency

#Load raw data, filter for 2015 and impute missing data
shanghaiData.src = read.csv("./data/shanghaiData.csv", stringsAsFactors = F)
shanghaiData = shanghaiData.src %>%
  left_join(., rbind(school_and_country_table,cleanupCountry), by = c('university_name')) %>%
  mutate(., university_name = ifelse (grepl('^The ', university_name), 
                                      gsub('The ','',university_name), 
                                      university_name)) %>%
  left_join(., cleanupRename, by = c('university_name')) %>%
  mutate(., new_name = ifelse(is.na(new_name), university_name, new_name)) %>%
  filter(., year == 2015)
#Missingness check: total_score is missing for universities ranked >100 (do not use; take rank instead); use kNN for imputing ns NAs (sqrt(n) ~ 70)
shanghaiData.i = kNN(shanghaiData, k = 70)
shanghaiData = shanghaiData.i[,1:13]

timesData.src = read.csv("./data/timesData.csv", stringsAsFactors = F)
timesData = timesData.src %>%
  mutate(., university_name = ifelse (grepl('^The ', university_name), 
                                      gsub('The ','',university_name), 
                                      university_name)) %>%
  left_join(., cleanupRename, by = c('university_name')) %>%
  mutate(., new_name = ifelse(is.na(new_name), university_name, new_name),
         international = as.numeric(international),
         income = as.numeric(income),
         total_score = as.numeric(total_score)) %>%
  filter(., year == 2015)
#Missingness check: total_score is missing for universities ranked >200 (do not use; take rank instead); use kNN for imputing NAs (sqrt(n) ~ 51)
timesData.i = kNN(timesData, k = 51)
timesData = timesData.i[,1:15]

cwur.src = read.csv("./data/cwurData.csv", stringsAsFactors = F)
cwur = cwur.src %>%
  rename(., university_name = institution, total_score = score) %>%
  mutate(., university_name = ifelse (grepl('^The ', university_name), 
                                      gsub('The ','',university_name), 
                                      university_name)) %>%
  left_join(., cleanupRename, by = c('university_name')) %>%
  mutate(., new_name = ifelse(is.na(new_name), university_name, new_name)) %>%
  filter(., year == 2015)
#Missingness check: no NAs for 2015
#cwur[,c(5:12)] = sapply(cwur[,c(5:12)], ranktoscore) #Convert CWUR ranks to scores


##########################
####Data consolidation####
##########################

#Data frame of unique universities ranked in 2015
rankings = unique(rbind(cwur[,c('new_name','country','year')],             #List unique universities to fix overlap and redundancy
                        timesData[,c('new_name','country','year')],
                        shanghaiData[,c('new_name','country','year')]) %>%
                    mutate(., country = gsub('Republic of Ireland', 'Ireland', country)) %>% #Rename countries for consistency
                    mutate(., country = gsub('USA','United States of America', country)) %>%
                    mutate(., country = gsub('Unisted States of America','United States of America', country)) %>%
                    mutate(., country = gsub('Unted Kingdom', 'United Kingdom', country)) %>%
                    mutate(., country = gsub('UK', 'United Kingdom', country)) %>%
                    mutate(., country = gsub('Slovak Republic', 'Slovakia', country)) %>%
                    mutate(., country = gsub('Russian Federation', 'Russia', country))) %>%
  left_join(., cwur[,c(1,4:15)], by = c('new_name', 'year')) %>%           #Recombine unique universities with ranks and scores
  left_join(., timesData[,c(1,4:15)], by = c('new_name', 'year')) %>%
  left_join(., shanghaiData[,c(1,3:11,13)], by = c('new_name', 'year')) %>%
  rename(., rank_cwur = world_rank.x,
         national_rank_cwur = national_rank.x,
         citations_cwur = citations.x,
         total_score_cwur = total_score.x,
         rank_times = world_rank.y,
         citations_times = citations.y,
         total_score_times = total_score.y,
         rank_shanghai = world_rank,
         national_rank_shanghai = national_rank.y,
         total_score_shanghai = total_score) %>%
  mutate(., rank_times = gsub('=','',rank_times)) %>%                     #Treat equal ranks as ranks
  mutate(., rank_times = as.integer(sapply(rank_times, split_rank)),      #Treat middle of rank ranges as ranks
         rank_shanghai = as.integer(sapply(rank_shanghai, split_rank))) %>%
  arrange(., desc(new_name))

countries = sort(unique(rankings$country))
universities = sort(unique(rankings$new_name))

#Quick way to get the means of the scoring criteria
df.2015.mean = rankings[,c(2,4:21,26,28:34)] %>%
  group_by(country) %>% 
  summarise_each(funs(f = round(mean(., na.rm=TRUE))))

#Data frame for mapping countries
df.2015.country = rankings %>%
  group_by(., country) %>%
  summarise(., top_cwur = min(rank_cwur, na.rm=T), 
            top_times = min(rank_times, na.rm=T),
            top_shanghai = min(rank_shanghai, na.rm=T),
            median_cwur = round(median(rank_cwur, na.rm=T)),
            median_times = round(median(rank_times, na.rm=T)),
            median_shanghai = round(median(rank_shanghai, na.rm=T)),
            count_cwur = sum(!is.na(rank_cwur)),
            count_times = sum(!is.na(rank_times)),
            count_shanghai = sum(!is.na(rank_shanghai))) %>%
  left_join(., df.2015.mean, by = 'country') #Adding columns for the means of scoring criteria

