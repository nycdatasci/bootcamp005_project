#setwd("C:/Users/Joseph/Desktop/NY/Project2")
library(shiny)
library(openxlsx)
data=read.xlsx("data.xlsx",sheet=1)
colnames(data)=c('Urban.Area','Composite.Index','Grocery.Items','Housing,Utilities','Transportation',
                 'Health.Care','Miscellaneous','Goods.and.Services')
tdf<-t(data)
Place = as.character(data$Urban.Area)
Data.salary2016<- data.frame(city=c("San, Jose, CA","San, Francisco, CA","Seattle, WA","New, York, (Manhattan), NY","San, Diego, CA","Boston, MA",
                                    "Los, Angeles-Long, Beach, CA","Austin, TX","Chicago, IL","Atlanta, GA",
                                    "Minneapolis, MN","Washington-Arlington-Alexandria, DC-VA"),salary=c(121537 ,117047,102380,101593,95968,92957,89991,88989,86786,86265,83246,81011))
                      