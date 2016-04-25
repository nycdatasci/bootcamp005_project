library(dplyr)
library(ggplot2)
getwd()
setwd("~/Desktop/Project1")
Data1<-read.csv("HedgeFundData1.csv", stringsAsFactors = FALSE)

Data_1<- Data1 %>%
  filter(., SHARPE>0, X..POSITIONS>10, Average.Weight>10, X..WIN.PER>50)

Sector_1<- ggplot(data=Data1, aes(x= NAME, y= X2015.Return)) + geom_point(aes(color= SECTOR)) + facet_grid(SECTOR ~ .) 
benchmark<- data.frame(SECTOR= c("Consumer Discretionary", "Consumer Staples", "Energy", "Financials", "Health Care", "Industrials", "Information Technology", "Materials"), X2015.Return= c(9.92, 6.89, -21.47, -1.74, 6.84, -4.3, 5.49, -8.67))
Overview<- Sector_1 + geom_hline(aes(yintercept= X2015.Return), benchmark) + xlab("Fund") + ylab("Return") + ggtitle("Fund Performance by Sector Overview")
Overview



zoom_3<- coord_cartesian(ylim = c(-50, 50))
Sector_1 + geom_hline(aes(yintercept= X2015.Return), benchmark) + xlab("Fund") + ylab("Return") + ggtitle("Fund Return by Sector") + zoom_3





zoom_1<- coord_cartesian(xlim = c(-100, 100))
Distribution_1<- ggplot(data=Data1, aes(x=X2015.Return)) + geom_density(aes(color=SECTOR)) + zoom_1 + xlab("Return") + ggtitle("Sector Return Distribution")

zoom_2<- coord_cartesian(xlim = c(-20, 50), ylim = c(0, 0.2))
Distribution_2<- ggplot(data=Data_1, aes(x=X2015.Return)) + geom_density(aes(color=SECTOR)) + zoom_2 + xlab("Return") + ggtitle("Sector Winner Return Distribution")
#vs benchmark 


#Data2
Data2<-read.csv("HedgeFundData2.csv", stringsAsFactors = FALSE)

Distribution_3<- ggplot(data=Data2, aes(x=X2015.Total.Fund.Return)) + geom_density() + xlab("Total Fund Return") + ggtitle("Total Fund Return Distribution")




#dim(Data1)
#summary(Data1)
#head(Data1)
#nrow(Data1)


#Consumer Discretionary
ConsumerDisc<- Data_1 %>%
  filter(., SECTOR=="Consumer Discretionary")

ConsumerDiscWinner<- Data_1 %>%
  filter(., SECTOR=="Consumer Discretionary", X2015.Return>9.92) %>%
  mutate(., Excess.Return=X2015.Return-9.92) %>%
  arrange(., desc(Excess.Return))

ConsumerDiscWinner

#Consumer Staples
ConsumerStap<- Data_1 %>%
  filter(., SECTOR=="Consumer Staples")

ConsumerStapWinner<- Data_1 %>%
  filter(., SECTOR=="Consumer Staples", X2015.Return>6.89) %>%
  mutate(., Excess.Return=X2015.Return-6.89) %>%
  arrange(., desc(Excess.Return))

ConsumerStapWinner

#Energy
Energy<- Data_1 %>%
  filter(., SECTOR=="Energy")

EnergyWinner<- Data_1 %>%
  filter(., SECTOR=="Energy", X2015.Return>-21.47) %>%
  mutate(., Excess.Return=X2015.Return-(-21.47)) %>%
  arrange(., desc(Excess.Return))

EnergyWinner

#Financials
Financials<- Data_1 %>%
  filter(., SECTOR=="Financials")

FinancialsWinner<- Data_1 %>%
  filter(., SECTOR=="Financials", X2015.Return>-1.74) %>%
  mutate(., Excess.Return=X2015.Return-(-1.74)) %>%
  arrange(., desc(Excess.Return))

FinancialsWinner

#Health Care
Health<- Data_1 %>%
  filter(., SECTOR=="Health Care")

HealthWinner<- Data_1 %>%
  filter(., SECTOR=="Health Care", X2015.Return>6.84) %>%
  mutate(., Excess.Return=X2015.Return-6.84) %>%
  arrange(., desc(Excess.Return))

HealthWinner

#Industrials
Industrials<- Data_1 %>%
  filter(., SECTOR=="Industrials")

IndustrialsWinner<- Data_1 %>%
  filter(., SECTOR=="Industrials", X2015.Return>-4.3) %>%
  mutate(., Excess.Return=X2015.Return-(-4.3)) %>%
  arrange(., desc(Excess.Return))

IndustrialsWinner

#Information Technology
IT<- Data_1 %>%
  filter(., SECTOR=="Information Technology")

ITWinner<- Data_1 %>%
  filter(., SECTOR=="Information Technology", X2015.Return>5.49) %>%
  mutate(., Excess.Return=X2015.Return-5.49) %>%
  arrange(., desc(Excess.Return))

ITWinner

#Materials
Materials<- Data_1 %>%
  filter(., SECTOR=="Materials")

MaterialsWinner<- Data_1 %>%
  filter(., SECTOR=="Materials", X2015.Return>-8.67) %>%
  mutate(., Excess.Return=X2015.Return-(-8.67)) %>%
  arrange(., desc(Excess.Return))

MaterialsWinner




'''
XLY 9.92
XLP 6.89
XLE -21.47
XLF -1.74
XLV 6.84
XLI -4.3
XLK 5.49
XLB -8.67
'''