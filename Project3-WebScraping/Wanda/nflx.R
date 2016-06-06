library(dplyr)
setwd("/Users/wandawang/Desktop")
#Netflix Details
setwd("/Users/wandawang/Documents")
netflix.detail <-read.csv("netflicks.csv", header = FALSE,
                          stringsAsFactors = FALSE) #761 need more 

netflix.details<-netflix.detail %>% rename(., Title = V1, Person = V2, 
                 Genre = V3, Series = V4) %>% mutate(Number = c(1:761))
View(netflix.details)
viewing.history <- read.csv("nflixgg.csv", header = FALSE) #8/6/11 missing after
#%>% select(., mutate(Number = c(1:2774))
viewing.history <- viewing.history[,1:2] %>% rename(., Date = V1, Title = V2)%>% mutate(Number = c(1:2774))
View(viewing.history)

new.table %>% group_by(new.table) %>% summarise(count=sum(new.table[,2]))
#####
new.table <- select(binge.num, Date, Series) # good
title.table <- as.data.frame(select(titlenames, Date, Series, NewTitle))
person.table <- select(peronz, Date, Series, Person)
person.table <- as.data.frame(person.table)

new.table <- as.data.frame(new.table)
View(new.table)

count <- rep(c(1), times = 628)
new.table <- cbind(new.table, count)
person.table <- cbind(person.table, count)
title.table <- cbind(title.table, count)

new.table <- new.table %>% group_by(Date, 
      Series) %>% summarise(., count = sum(count))

person.table <- person.table%>% group_by(Date, 
      Series, Person) %>% summarise(., count = sum(count))

title.table <- title.table%>% group_by(Date, 
      Series, NewTitle) %>% summarise(., count = sum(count))

t.binged <- title.table %>% arrange(., desc(count))

p.binged <- person.table %>% arrange(., desc(count))
View(p.binged)
#most binge-watched 70153404 Friends// 70136141 Sunny
m.binged <- new.table %>% arrange(., desc(count))
View(m.binged)
str(p.binged) # messed up 
#month?
t.dates <- t.binged$Date
person.dates <-p.binged$Date
dates <- m.binged$Date
b.Dates <- as.Date(dates,format = "%m/%d/%y")
t.Dates <- as.Date(t.dates,format = "%m/%d/%y")
p.Dates <- as.Date(person.dates,format = "%m/%d/%y")
b.Dates
p.binged$Date <- p.Dates
m.binged$date <- b.Dates
t.binged$Date <- t.Dates

m.binged <- select(m.binged, date, Series, count)
p.binged <- select(p.binged, Date, Series, count, Person)
t.binged <- select(t.binged, Date, Series, count, NewTitle)

m.binged <- m.binged[,2:4]
#netflix.details.. row 258 blank, kill Number = 258
#View(new.table)
#new.table <- as.data.frame(new.table)
### graph
plot(p.binged$count, axes=T, ylim=c(0,150), typ='l', ann=T)
library(ggplot2)

#TV Show Seris
ggplot(m.binged, aes(x = date, y = count, group = Series)) + geom_line(aes(colour = Series))
#TV Show Person/Director
ggplot(p.binged, aes(x = Date, y = count, group = Person)) + geom_line(aes(colour = Person))
#Title
str(tv$Title.y)
tv$Title.y<-sapply(tv$Title.y[1:628], function(x) as.character(x))

numbers <- function(x) {
  for (i in 1:628) {
    return(strsplit(x,' ')[[i]][1:2])
    #print(paste(dt, sep="",strsplit(ex,'')[[i]][2]))
  }
}
numberz <- function(x) {
  for (i in 1:628) {
    return (paste(x, collapse = " "))
    #return(strsplit(x,' ')[[i]][1:2])
    #print(paste(dt, sep="",strsplit(ex,'')[[i]][2]))
  }
}

splitz <- lapply(tv$Title.y, numbers)
splits <- lapply(tv$NewTitle, numberz)
tv$NewTitle <- splitz
tv$NewTitle <- splits

lapply(splitz, paste(c("Breaking", "Bad:"), collapse = " "))
paste(c("Breaking", "Bad:"), collapse = " ")
tv$NewTitle<-paste(splitz[1:628], collapse = " ")
#Genre


p.binged$Person = as.factor(p.binged$Person)
m.binged$Series = as.factor(m.binged$Series) ### make Series a factor
library(googleVis)
####

datez = m.binged$date
df = data.frame(datez, mbinged$Series)
Line <- gvisLineChart(df)
plot(Line)

new <- netflix.details[1:257,]
neww <- netflix.details[259:761]
newww <- rbind(new, neww)   
newww<-newww %>% mutate(.,Numbers = c(1:760))
View(newww)
bew <- newww[1:284,]
beww <- newww[286:760,]
bewww <- rbind(bew, beww)
bewww <- bewww %>% mutate(., Num = c(1:759))
View(bewww)
kew <- bewww[1:725,]
keww <- bewww[727:759,]
kewww <- rbind(kew, keww)
kewww <- kewww %>% mutate(., Num = c(1:758))
pew<- kewww[1:666,]
peww <- kewww[668:759,]
pewww <- rbind(pew, peww)
pewww <- pewww %>% mutate(., Num = c(1:758))
View(pewww)
pewww<- pewww[1:757,]
netflix.details <- pewww
history <- history[1:757,]
history<-viewing.history[1:761,] 
View(history)

merge <- merge(netflix.details, history, by.x = "Num", by.y = "Number") 
View(merge)

tv <- merge %>% filter(., Genre ==" TV Shows ") %>% arrange(desc(Num)) 
View(tv)
unique(tv$Series) #65, 1 empty
str(unique(m.binged$Series)) #64

personz <-  tv %>% group_by(Date, Series)
peronz <-select(tv, Num, Series, Date, Person) %>% group_by(Series)
View(peronz)

titlenames <- select(tv, Num, Series, Date, NewTitle) %>% group_by(Series)

binge <- tv %>% group_by(Date, Series) 
binge.num<-select(tv, Num, Series, Date) %>% group_by(Series)
View(binge.num)

binge.num$Series
#look for consecutive strings
#stack problem. 

binge.array <- as.array(binge.num$Series)
N <- length(binge.num$Date)-1
for (i in 1:N){
  if (binge.num$Series[i] == 70136141) {
    binge.num$test[i] = 3
  }
  else {
    binge.num$test[i+1] <- 1
  }
}
#class(binge.num$Date)
#[1] "factor"
#binge.num$Date[1] == 
#  + "5/17/15"
#[1] TRUE

#unique number for each unique series? 65 times...
binge.num$Series <- as.numeric(binge.num$Series)
binge.num$Series[1] - binge.num$Series[2]

class(binge.num$Series) #character or #integer

has.consecutives <- function(draw) any(diff(draw)==1)
unique.titles<-unique(binge.num$Series) #65....628/65 
#count for each unique one 

binge.num$Diff <- 
#for each Series, is the Num diff = 1?

#%>% summarise(., count = count(Series))
#for each in Number column, difference between current and next should == 1, otherwise not binging 
tv.shows$Number
#just loop through the array, comparing the current value to the previous value to see 
#if the values are consecutive, while keeping track of the current consecutive count. 
#Consecutive numbers should be appended to an array, and non-consecutive numbers should 
#reinitialize the array. 
#When you reach the desired count of consecutive numbers, you can return the result.

tv.shows <- netflix.details %>% filter(., Genre ==" TV Shows ") %>% arrange(Series)
View(tv.shows)
has.consecutives <- function(draw) any(diff(draw)==1)
#group by each date, check if the corresponding row numbers are consecutive. 
#len(number of date rows) = 5
#if i watched 5 things in one day, were they the same show? 
# check if series is equal for all? series has to be integer type
has.series <- function(draw) any(diff(draw)==0)
tv.shows$Series= as.integer(tv.shows$Series)
has.series(tv.shows$Series[8:9]) #[1] TRUE
tv.shows$Number[1:5]
has.consecutives(tv.shows$Number[1:7]) #[1] FALSE

tv.array = as.array(tv.shows$Number)
#keep feeding it different arrays


trim(netflix.details$Genre)
str(netflix.details$Genre[1])

sapply(netflix.details, class)

#Missing values#TV Shows
tv.shows <- netflix.details %>% arrange(Genre)

netflix.details$Title[1]

View(netflix.details %>%
  group_by(Genre) %>%
  arrange(desc(Number)))

for (i in friends[1,2]) {
  return <- 1
}

#length(friends[1,2])
#[1] 1

View(Person<-netflix.details %>%
       arrange(Person)) 
View(Person<-Person[1:21,])
Person$Person = "empty" ##? doesn't see empty cell as empty? convert to character?
Person$Person = as.character(Person$Person)

#> Person$Person[1]
#[1] ""

netflix.details[netflix.details$Number == 6,]

View(tv.shows)
#%>% summarise(count = count(Genre)) %>% arrange(count)

mw.title<-netflix.details %>% arrange(Title) %>% summarise(., Total = count(Title))



#Column headers: Title, Person, Genre, Series ID, Date
#Pull in Date column from other file - merge tables?
#Missing Values - Title, Person

str(netflix.details)
str(viewing.history)
#Most watched show

mw.genre<-netflix.detail %>% group_by(., Genre) %>% summarise(., Total = count(Genre) )
#Most binge-watched TV shows - consecutive 3 or more - Series ID
#group_by(., Date, Genre = 'TV Show' or Series ID) , for each
 #if cell matches next cell in column. If yes, return "binging"
#length of title...Plot.ly..Sources: Netflix, The Diffusion Group (US Averages), IMDB (movie & TV lengths)
#why the uptick in viewing in 2015? Change in lifestyle, job or relationship?
#notes: logo-popular shows, missing fields.
#trends over time 
#total daily viewing time compared to us avg
#google search trend
#when iwas hooked?
#hrs spent per wk
#genre
#% watched first wk of release date of show
#weather/weekend
#tv has no duration info
#get workign on this!






############################################################
library(dplyr)
setwd("/Users/wandawang/Desktop")
date.title <- read.csv("nflx.csv", header = FALSE)
date.title<-date.title %>% mutate(., Date = V1, Title = V2)
date.title <- date.title[,3:4]
#save the table.

###second step 
date.title$Datechars <- nchar(datechar) #10/31/1 and 1/10/16 have 7. #5/8/16 has 6is ok 
date.title$Titlechars <- nchar(as.character(date.title$Title))
datechar <- as.character(date.title$Date)
date.title$Title <- as.character(date.title$Title)
date.title$Date <- as.character(date.title$Date)
View(date.title)

clean <- function(x){
  if (grep('[0-9]', x)){
    print ('yes')
  } 
}

sapply(date.title$Title, clean)

#cleaning up months
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

date.to.clean<-grep("/1 ",date.title$Date) #returns row numbers, integers
titles.to.clean <-date.title[date.to.clean[1:660],2] #2 is Title column
View(cbind(date.to.clean, titles.to.clean))
t.t.c <- cbind(date.to.clean, titles.to.clean) #adding , c(1:660), make a copy of date.to.clean?

## Whitespace, use later on 
new.date.title <- date.title
trim <- function (x) gsub("^\\s+|\\s+$", "", x) #function 
new.date.title$Date <- trim(new.date.title$Date)
paste(new.date.title$Date) #recount chars for new.date.title!!!!
#####

date.title[237,2] #" 5 Mushi-Shi: Next Passage: The Warbling Sea Shell"
# corresponds with date.to.clean[1:length(date.to.clean)][1]

ex <- t.t.c[1:660,2]
exe<-t.t.c$titles.to.clean

############
again <- date.title[dtez,2]
dirtydates <- date.title[dtez,1]
#[1] "12/21/1 " "12/21/1 " "12/21/1 " "12/21/1 " "12/20/1 " "12/20/1 " "12/17/1 " "12/14/1 " "12/13/1 " "12/13/1 " "12/12/1 "
 #date.to.clean[1:2] #237 238th row of date.title original dataset
dtez<-date.to.clean[1:length(date.to.clean)] #integers
#[1]  237  238  239  240  241  242  243  244  245  246  247  248  249  250  251  252  253  254  255
detez <- as.character(dtez)
dirtydata<-cbind(dirtydates,again,detez)
#matrix
dirtydata<-as.data.frame(dirtydata)
#remove whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x) #function 
dirtydata$dirtydates <- trim(dirtydata$dirtydates)
paste(dirtydata$dirtydates) 
#####
#pick off first character of each dirtydata$again string

t.t.c <- as.data.frame(t.t.c)

t.t.c[1,]
substring(t.t.c[1,2],3)
t.t.c$new <- sapply(t.t.c[1:660,2], substring, 3) #lops off all the first numbers
View(t.t.c) #ALL GOOD
#need to keep titles.to.clean to chop off the first digit. 
as.character(tv$Title.y) #character object... splittable or not. 

#move it to end of corresponding dirtydata$dirtydates
exey<-as.character(t.t.c[1:660,2])





t.t.c$test <- numbers(exey) #### need help cleaning this - CHOPPED OFF FIRST DIGIT TO APPEND TO DATE STRING 

t.t.c$date.to.clean <- as.integer(t.t.c$date.to.clean)
new.date.title$entry <- as.integer(new.date.title$entry )
t.t.c$titles.to.clean <- as.character(t.t.c$titles.to.clean)
new.date.title$Title <- as.character(new.date.title$Title)
df <- dplyr::right_join(t.t.c, new.date.title, by = c("date.to.clean" = "entry", "titles.to.clean" = "Title"))


t<- capture.output(numbers(exey))
t<- strsplit(capture.output(numbers(exey)),'')
#t<-as.data.frame(t)
#t<-t[6,]
#t.t.c$numbers <-t #NOT NEEDED? 
#t.t.c$numbers <- sapply(t.t.c[1:660,4], substring, 4) #character type ""
#str(t.t.c$numbers)
#trans<-as.data.frame(t(t)) #transpose
#View(trans[,1]) #give up 
#View(strsplit(t.t.c$numbers[1],'')[[1]][3])
#remove quotes on numbers.

###############################
#merging t.t.c and new.date.title....   NEW.DATE.TITLE$ENTRY MATCHES T.T.C$DATE.TO.CLEAN
##t.t.c<-t.t.c %>% select(entry=date.to.clean,numbers,Title=new)
#################
#cleaning <-cbind(t.t.c$date.to.clean, t.t.c$new, #t.t.c$numbers) #first column is wrong

new.date.title$entry = c(1:2797) # entry tracks each one 

#t.t.c$entry <- as.integer(t.t.c$entry)
#new.date.title$Titl <- new.date.$Title
#remove t.t.c$Titl

#> class(t.t.c$entry)
#[1] "factor"
#> class(new.date.title$entry)
#[1] "integer"

#consecutive string pattern matching

#t<-numbers(as.character(dirtydata$again))

#sapply(exey, numbers)
#sapply(exey, class)
#sapply(as.character(t.t.c$titles.to.clean), function(x) strsplit(x,'')[[c(1:660)]])

#calling the function on t.t.c. column  
 #or exe works too

#exe<-as.character(t.t.c$titles.to.clean)

#ex<- t.t.c[1:660] #not sure if its feeding through to the above
 #error

trim <- function (x) gsub("^\\s+|\\s+$", "", x) #function 
new.date.title$Date <- trim(new.date.title$Date)


titles.to.clean[1:660,]
titles.to.clean[22,]
class(titles.to.clean) #tbl_df
titles.to.clean[1:length(titles.to.clean),]
# write some loop

#lop off the again title. 
#for (i in 1:660) {
 # print(strsplit(ex,'')[[i]][2])
  #print(paste(dt, sep="",strsplit(ex,'')[[i]][2]))
#}

#ex<- t.t.c[1:660] #dirty still
exx<-strsplit(ex,'')[[1]][2] # first cell 
exxx<-strsplit(ex,'')[[660]][2]
strsplit(ex,'') #splits each the strings by character, for each row #[[1]] first row [2] chops off the number
dt <- date.title[date.to.clean[1:length(date.to.clean)],1] # date.title[###,1] dirty dates
paste(dt,sep ="", exx) ##is only adding 5.. 

asd <- 'hello stackoverflow'
strsplit(asd, '')[[1]][1]
paste0(strsplit(asd, '')[[1]][-1], collapse = '')
#replace wonky row with the fixed xx date after whitespace is trimmed
#############################################################
good<-paste(dt,sep ="", xx) #remove whitespace beforehand

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
