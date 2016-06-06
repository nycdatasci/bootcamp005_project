# 2014 Fannie Mae File, Enterprise Code is 1
craig_try=read.table("~/web_scrappin/craig_new_full_data/craig_friday_10_may20.txt",header=FALSE,sep="\t",stringsAsFactors = FALSE)
craig_car3[,"price"] <- lapply(data[,"price"], function(x) gsub("$", "", x ))


                     
craig_short_full=read.table("~/classnotes/mongodb/Debugging/mongodb_demo/craig_short.txt",header=FALSE,sep="\t",stringsAsFactors = FALSE)
#/Users/radheyshyam/#

craig_short_new[1]=lapply(craig_short_new[1], function(x) as.numeric(gsub("[,$]", "",x)))
library(ggplot2)
g=ggplot()+ geom_density(data=temp,aes(x=price),fill=price)

g


g1=ggplot()+ geom_freqpoly(data=temp,aes(x=price))

g1

g2=ggplot(data=temp,aes(price))+ geom_histogram()

g2

g3=ggplot(data=craig_cars,aes(price))+ geom_freqpoly(fill)
s
g3



library(gdata)
craig_cars=read.xlsx(file = "~/classnotes/mongodb/Debugging/mongodb_demo/craig_short_xls.xlsx",sheetIndex = 1,stringsAsFactors=FALSE,header = FALSE)

# removes rows having price less than 300
str(craig_cars)
dim(craig_cars)
library(dplyr)
craig_cars_300=dplyr::filter(craig_cars, craig_cars$price > 299)

craig_cars_300_lt_=dplyr::filter(craig_cars, craig_cars$price > 299)


dim(craig_cars_300)

# now again cheking frequency poly 
g4=ggplot(data=craig_cars_300,aes(price))+ geom_freqpoly() + coord_cartesian(xlim = c(0,30000))

g4

g5=ggplot(data=temp,aes(price,fill="#FF9999"))+ geom_histogram(binwidth =500)+coord_cartesian(xlim = c(0,25000))
g5


g6=ggplot(data=temp,aes(price))+ geom_freqpoly()+coord_cartesian(xlim = c(0,10000))
g6+ggtitle("Count Vs. Price Frequency")

g7=ggplot(data=craig_cars_300,aes(price))+ geom_density()+coord_cartesian(xlim = c(0,25000))
g7+ggtitle("Geom_Density")






# Try to read the full details file withh body with csv with multile spaces
craig_try_full=read.csv2("~/classnotes/mongodb/Debugging/mongodb_demo/craig_long.txt",header=FALSE,sep="\t",stringsAsFactors = FALSE)
names(craig_try_full)=c("price","title","post_date","body")
count=0
for (i in 1:(nrow(craig_try_full)-1)) {
  if (craig_try_full[i,"body"] == "" ) {
      #count=count+1
    craig_try_full[i,"body"] = craig_try_full[i+1,"price"]
    craig_try_full[i + 1,"price"] = ""
                                        }
}



#converting post_date to dates
#First make a copy of 

temp=craig_cars_300
temp_date=strftime(temp$post_date,"%u")
temp_Monday=strftime(temp$post_date,"%A")

time_12h=strftime(temp$post_date,"%r")
#next add one colunm to temp
temp[,"week_day"]=strftime(temp$post_date,"%A")

#time of the day


temp[,"time"]=strftime(temp$post_date,"%r")

#hour of the day from 0 to 23
temp[,"time"]=strftime(temp$post_date,"%H %")
temp[,"time_am"]=strftime(temp$post_date,"%p")

temp[,"full_time"]=strftime(temp$post_date,"%I:%M %p")
am= 1:2215
i=0
for (row in temp$post_date){
  i = i+1
  if (grepl("am",row)){
    am[i]= 0
    } else {
      am[i]=12
    }
  }
  

time_copy=temp$time
time_24 = as.numeric(time_copy)+ am

#ggplot(temperatures) +
#geom_bar(aes(x = hour, fill = l.h.temp)) 

h1=ggplot(temp)+geom_bar(aes (x=time,fill=week_day))
h1
h2=ggplot(temp)+geom_bar(aes(x=time_24,fill="#FF9999"))+
  coord_cartesian(xlim = c(0,24))+xlab("Time of posting Ad(round to hour)")+facet_wrap( ~week_day,ncol(2))
h2


t=ggplot(data=temp)+ geom_bar(aes(time),binwidth = 1)
t

t1 = ggplot(data=temp)+ geom_point(aes(x=week_day,y=time))
t1

# let us try violin plot

t2 = ggplot(data = temp, aes(x =reorder(week_day,week_day,function(x)length(x)), y = time_24,fill=week_day)) + geom_violin()+xlab("")+ylab("count")+ scale_fill_discrete(name ="")
t2

t3 = ggplot(data = temp, aes(x =reorder(week_day,week_day,function(x)length(x)), y = time_24,fill=week_day)) + geom_boxplot()
t3



#x=reorder(Position,Position,
#function(x)-length(x)))
w=ggplot(data=temp)+geom_bar(aes(x=reorder(week_day,week_day,function(x)length(x))),fill="#FF9999", colour="black") + ggtitle("Weekday of Posting Craiglist Ad")+ labs(y="Count",x="")
#w=ggplot(data=temp)+geom_bar(aes(x=reorder(week_day,week_day,function(x)-length(x) )))
w

r=ggplot(data=temp)+geom_bar(aes(x=reorder(week_day,week_day,function(x)length(x))),fill=week_day) + ggtitle("Weekday of Posting Craiglist Ad")+ labs(y="Count",x="")
r

m=ggplot(data=temp)+geom_bar(aes(x=reorder(week_day,length(week_day),max )))
m
# Word cloud 
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)

# jeopCorpus <- Corpus(VectorSource(jeopQ$Question))
# Next, we will convert the corpus to a plain text document.
# jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
# Then, we will remove all punctuation and stopwords. Stopwords are commonly used words in the English language such as I, me, my, etc. You can see the full list of stopwords using stopwords('english').
# 
# jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
# jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
# Next, we will perform stemming. This means that all the words are converted to their stem (Ex: learning -> learn, walked -> walk, etc.). This will ensure that different forms of the word are converted to the same form and plotted only once in the wordcloud.
# 
# jeopCorpus <- tm_map(jeopCorpus, stemDocument)
# Now, we will plot the wordcloud.
# 
# wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)
# 
# title=Corpus(VectorSource(temp$title))
# 
# title_text=tm_map(title,PlainTextDocument)
# title_text=tm_map(title_text,removePunctuation)
# title_text=tm_map(title_text,removeNumbers)
# library("tm")
# 
# title_text=tm_map(title_text,removeWords,stopword("english"))
# title_text=tm_map(title_text, removeWords, c('the', 'this', stopwords('english')))
# 



title_without_num=gsub("[0-9]","",temp$title)
title_without_num=gsub("!*","",title_without_num)

#Years only

title_years=sub('.*(\\d{4}).*', '\\1', temp$title)

#other option
library(stringi)
title_years2=stri_extract_last_regex(temp$title, "\\d{4}")

wordcloud(title_years2, scale=c(4,2),min.freq=10,max.words=100, random.order=FALSE, random.color=FALSE, rot.per=0.15,colors= brewer.pal(8, "Paired"))

# ,vfont=c("sans serif","bold")
wordcloud(title_without_num, scale=c(10,3),min.freq=5,max.words=100, random.order=FALSE, random.color=FALSE, rot.per=0.15,colors= brewer.pal(8, "Paired"))


saveRDS(temp,"~/web_scrappin/craig_shiny/temp.RDS")
saveRDS(craig_cars,"~/web_scrappin/craig_shiny/craig_cars.RDS")
saveRDS(craig_cars_300,"~/web_scrappin/craig_shiny/craig_cars_300.RDS")
saveRDS(craig_try_full,"~/web_scrappin/craig_shiny/craig_try_full.RDS")
saveRDS(crg_cars,"~/web_scrappin/craig_shiny/crg_cars.RDS")
saveRDS(title_without_num,"~/web_scrappin/craig_shiny/words_cloud.RDS")
saveRDS(title_years2,"~/web_scrappin/craig_shiny/title_years2.RDS")

