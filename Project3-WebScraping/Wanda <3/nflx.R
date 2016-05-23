library(dplyr)
setwd("/Users/wandawang/Desktop")
date.title <- read.csv("nflx.csv", header = FALSE)
date.title<-date.title %>% mutate(., Date = V1, Title = V2)
date.title <- date.title[,3:4]
#save the table.
#2797 viewings from 09/21/10 to 
#alot of missing data on the showdetails table
#if nchar(date.title$Date)  
#if there is only one digit after '/' for Date, for month 10/xx, 11, 12 
#look at first string in Title, remove it and add it onto Date. 
#group them, and clean
#date.title[1,1] #5/15/16 
#new column counting number of characters in each date cell 
##
trim <- function (x) gsub("^\\s+|\\s+$", "", x) #function 
date.title$Date <- trim(date.title$Date)
paste(date.title$Date)
###
date.title$Datechars <- nchar(datechar) #10/31/1 and 1/10/16 have 7. #5/8/16 has 6is ok 
date.title$Titlechars <- nchar(as.character(date.title$Title))
datechar <- as.character(date.title$Date)
date.title$Title <- as.character(date.title$Title)
date.title$Date <- as.character(date.title$Date)
View(date.title)
#cleaning up 
octobers = grep('10/', date.title$Date) 
tenths = grep('/10/', date.title$Date)
octrows <- setdiff(octobers, tenths) #row numbers to clean
octs<-date.title[octrows[1:length(octrows)],1] #what they look like
#remove overlap. it's better if there's whitespace actually. 
novembers = grep('11/', date.title$Date)
elevenths = grep('/11/', date.title$Date)
novrows <- setdiff(novembers, elevenths)
novs<-date.title[novrows[1:length(novrows)],1] 
decembers = grep('12/', date.title$Date)
twelths = grep('/12/', date.title$Date) 
decrows<-setdiff(decembers, twelths)
decs<-date.title[decrows[1:length(decrows)],1]

date.to.clean<-grep("/1 ",date.title$Date) ## yes! 660 rows identified
titles.to.clean <-date.title[date.to.clean[1:660],2] #tbl_df

#buzz<-as.character(titles.to.clean[1:660,]) #character vector
#660 titles to clean 

titles.to.clean[1,] # first one is 5 Mushi-Shi: Next Passage: The Warbling Sea Shell
substring(titles.to.clean[1,],3) #" Mushi-Shi: Next Passage: The Warbling Sea Shell"
#substring of all data frame elements, pop off the number. but need to keep the number. 
sapply(titles.to.clean[1:660,], substring, 3)

# how to loop through each row 
#Date cleaning - group 10, 11, 12 months
#count number of chars after 2nd '/'
#Title cleaning 
x <- date.title[238,2] # " 5 Mushi-Shi: Next Passage: Banquet at the Forest's Edge"
xx<-strsplit(x, '')[[1]][2] #this works. #5
dt <- "12/21/1" #date.title[237,1] 
paste(dt,sep ="", xx)



#whitespace cleaning for date column
trim <- function (x) gsub("^\\s+|\\s+$", "", x) #function 
date.title$Date <- trim(date.title$Date)

#subs<-as.data.frame(substring(titles.to.clean[1:660,],3))

test <-date.title[238,2] #... need to steal 5 from ,2 and put in , 1
test #" 5 Mushi-Shi: Next Passage: Banquet at the Forest's Edge"
substring(date.title[238,2],3)#" Mushi-Shi: Next Passage: Banquet at the Forest's Edge"
substring(test,3)

#function, for each date row if 10, 11, 12 
#check if /xx is true. 

#if not, do the following
#date.title<-tbl_df(date.title)
#date.title[grep("/1 ", Date)] #does not work??
#as.data.frame.table(date.title)


unlist(strsplit(x, ''))[2] #5
paste0(strsplit(x, '')[[1]][-2], collapse = '') # "  Mushi-Shi: Next Passage: Banquet at the Forest's Edge"
#returns string w/o leading or trailing whitespace


#director and cast are both person types
#find the first 3 of the string
#substring to replace the cell 
#append to date string 
View(date.title[237,])
#date.title[237,2] <- 'Mushi-Shi: Next Passage: The Warbling Sea Shell' #date.title[237,1] <- '12/21/15'

#remove the first character
#x <- 'hello stackoverflow',
#substring(x, 2)

#how to match similar titles together- url title data
#did 63 and stopped..unlikely animal friends doesn't have person info. skip it
#3/6/16
#remove row 64 later 
dte.title <- read.csv("nflxx.csv", header = FALSE)
dte.title <-dte.title %>% mutate(., Title = V1, Director = V2, Genre = V3)
dte.title <- dte.title[,4:6]
View(dte.title)
str(dte.title)
#ignoring empty cells... 

#group by title, director, genre
test2 <- '5 asdfsdf'
pos = grep('5', test2)#...#position of 1 #pos = gregexpr('a', test2) pos1 = grep('5', test2)#...index

#consecutive dates #consecutive titles #consecutive genres #consecutive director
#missing b/c of logo -> more popular show?
