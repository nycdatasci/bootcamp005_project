library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)

###########################################################
###########################################################
#This project explores the data from the A Song of Ice and Fire books.
#I gathered this data from http://towerofthehand.com/books/guide.html
#This is an "encylopedia" of the book with fan gathered data on each chapter of the series.
#I scraped this data, exported it to MongoDB, and then wrote this to a csv file: all_chapters.csv that
#I will use here. The code to scrap the data can be found here: https://github.com/nycdatasci/bootcamp005_project/tree/master/Project3-WebScraping/RobCastellano_A_Song_of_Ice_and_Fire
#####################################
#The narrative structure of the A Song of Ice and Fire books is that each chapter has a "POV character"
# from whom we experience the events of the chapter.
#I was interested in examining the structure to find who has the most popular POV chapters.
#Also, what books are the most popular?
#The visualizations below attempt to examine these questions
######################################
#####################################

#Read in data
asoiaf = read.csv("all_chapters.csv", stringsAsFactors = F)
asoiaf$POV = factor(asoiaf$POV)
asoiaf$Book = factor(asoiaf$Book)
asoiaf$Book = relevel(asoiaf$Book, 'ADWD') %>% relevel('AFFC') %>% 
              relevel('ASOS') %>% relevel('ACOK') %>% relevel('AGOT')

###################################################
######## WHOLE SERIES SCORE DIST ##################
####################################################
nrow(asoiaf)
#344 chapters

ggplot(asoiaf, aes(x = Score)) + geom_histogram(aes(#y =..density.., 
  fill = factor(2)), color = "black", bins = 35) + #geom_density() + 
  theme_minimal() + guides(fill = F) + scale_x_continuous(limits = c(5.5, 10)) + 
  ggtitle("Distribution of Scores\n (344 Chapters Total)") + xlab("Score") + ylab("Number of Chapters") + scale_fill_brewer(palette = "Set1")

summary(asoiaf)

###################################################
############### BOOK RATINGS #######################
####################################################
ggplot(asoiaf, aes(x = Book, y = Score)) + geom_violin(aes(fill = Book), draw_quantiles = .5) + guides(fill = F) +
  theme_minimal() + ylab("Score") + ggtitle("Distribution of Scores by Book") + 
  scale_y_continuous(limits = c(5.5, 10)) + scale_fill_brewer(palette = "Set1")

#EDA for Scores 
asoiaf %>% group_by(Book) %>% summarise(Median = median(Score), Mean = mean(Score), sd(Score))
# Book   Median  Mean   sd(Score)
# (fctr)  (dbl)  (dbl)     (dbl)
#  AGOT  8.330 8.219041 0.5668925
#  ACOK  7.760 7.745000 0.7093062
#  ASOS  8.055 8.031220 0.6513526
#  AFFC  7.530 7.570000 0.5208156
#  ADWD  8.010 8.003699 0.7064416

#####################################################
############### COUNT BY POV ######################
##################################################
povcounts = asoiaf %>% group_by(POV) %>% summarise(count = n())

#31 POV Characters

############# All POV characters ##################
ggplot(povcounts, aes(reorder(x = POV, -count), y = count)) + geom_bar(stat = "identity", aes(fill = POV),
                                                                       fill = colorRampPalette(brewer.pal(9, "Set1"))(31)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())  + 
  xlab("Character") + ylab("Number of POV Chapters") + ggtitle("POV Chapters")

############## Main POV characters ####################
mainpov = povcounts[povcounts$count > 4, ]$POV
filter(povcounts, POV %in% mainpov)

ggplot(filter(povcounts, POV %in% mainpov), aes(reorder(x = POV, -count), y = count)) + 
  geom_bar(stat = "identity", aes(fill = POV),
           fill = colorRampPalette(brewer.pal(9, "Set1"))(length(mainpov))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())  + 
  xlab("Character") + ylab("Number of POV Chapters") + ggtitle("POV Chapters")

###################################################
############### RATINGS BY POV CHARACTERS##########
##################################################

############ ALL CHARACTERS #######################
ggplot(asoiaf, aes(x = reorder(POV, -Score, FUN = "median"), y = Score)) + 
  geom_boxplot(aes(fill = POV), 
               fill = colorRampPalette(brewer.pal(9, "Set1"))(length(levels(asoiaf$POV))), 
               outlier.shape = NA) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())  + 
  xlab("POV Character") + ylab("Chapter Score") + ggtitle("Chapter Scores by POV Character")

############# MAIN CHARACTERS ####################
ggplot(filter(asoiaf, asoiaf$POV %in% mainpov), 
       aes(x = reorder(POV, -Score, FUN = "median"), y = Score)) + 
  geom_boxplot(aes(fill = POV), fill = colorRampPalette(brewer.pal(9, "Set1"))(length(unique(filter(asoiaf, asoiaf$POV %in% mainpov)$POV))), 
               outlier.shape = NA) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())  + 
  xlab("POV Character") + ylab("Chapter Score") + ggtitle("Chapter Scores by POV Character")

###############################################
vec = strsplit(substr(asoiaf$Appearing,3, nchar(asoiaf$Appearing) - 2), "\",\"")
characters = c()
for(v in vec) {
  characters = c(characters, v)
}
unique(characters)
#1111 unique characters appearing

################################################
###############################################

