setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 3")
library(ggplot2)
library(dplyr)
boardgames = read.csv('boardgames.csv')

str(boardgames)
View(boardgames)

boardgames$age = factor(boardgames$age, levels = c("[]", "[3+]", "[4+]","[5+]","[6+]",
                                                   "[7+]","[8+]","[9+]","[10+]",
                                                   "[11+]","[12+]","[13+]","[14+]",
                                                   "[15+]","[16+]","[17+]","[18+]",
                                                   "[25+]"))

#Age Graph

ggplot(data = boardgames, aes(x= age)) + 
  geom_bar(aes(fill = Game.Rank), position ="fill")+
  ggtitle("Proportion of Boardgames by Suggested age") +
  ylab(label = "Proportion" )+ scale_fill_brewer(palette = "Set1")

ggplot(data = boardgames, aes(x= age)) + 
  geom_bar(aes(fill = Game.Rank), position ="stack")+
  ggtitle("Number of Boardgames by Suggested age") +
  ylab(label = "Count" )+ scale_fill_brewer(palette = "Set1")



unique(boardgames$age)

#Mechanics Graph

ggplot(data = boardgames, aes(x= factor(Number.of.mechanisms))) + 
  geom_bar(aes(fill = Game.Rank), position ="stack")+
  ggtitle("NUmber of Boardgames by number of mechanisms") +
  ylab(label = "Count" )+ xlab(label = "Number of Game Mechanics" )+
  scale_fill_brewer(palette = "Set1")

ggplot(data = boardgames, aes(x= factor(Number.of.mechanisms))) + 
  geom_bar(aes(fill = Game.Rank), position ="fill")+
  ggtitle("Proportion of Boardgames by number of mechanisms") +
  ylab(label = "Proportion" )+ xlab(label = "Number of Game Mechanics" )+
  scale_fill_brewer(palette = "Set1")



#Category Graph

ggplot(data = boardgames, aes(x= factor(Number.of.categories))) + 
  geom_bar(aes(fill = Game.Rank), position ="fill")+
  ggtitle("Number of Boardgames by number of Themes") +
  ylab(label = "Proportion" )+ xlab(label = "Number of Themes" )+
  scale_fill_brewer(palette = "Set1")

#time graphs

filter(boardgames, timemin < 480)%>%
  ggplot(data = . , aes(x= timemin)) + 
  geom_density(aes(color = Game.Rank))+
  ggtitle("Density of Boardgames by Time (Min)") +
  ylab(label = "Density" )+ xlab(label = "Time (Min)" )+
  scale_fill_brewer(palette = "Set1")

ggplot(data = boardgames, aes(x= factor(timemin))) + 
  geom_bar(aes(fill = Game.Rank), position ="fill")+
  ggtitle("Proportion of Boardgames by Minimum Time") +
  ylab(label = "Proportion" )+ xlab(label = "Minimum time to play" )+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data = boardgames, aes(x= factor(timemin))) + 
  geom_bar(aes(fill = Game.Rank), position ="stack")+
  ggtitle("Number of Boardgames by Minimum Time") +
  ylab(label = "Count" )+ xlab(label = "Minimum time to play" )+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



boardgames$timemin  


#year graph

filter(boardgames, year > 1950)%>%
ggplot(data = ., aes(x= year)) + 
  geom_density(aes(color = Game.Rank))+
  ggtitle("Density of Boardgames by Year") +
  ylab(label = "Density" )+ xlab(label = "Minimum time to play" )+
  scale_fill_brewer(palette = "Set1")
boardgames$year

#Price graph

filter(boardgames, price < 100)%>%
  ggplot(data = . , aes(x= price)) + 
  geom_density(aes(color = Game.Rank))+
  ggtitle("Density of Boardgames by Price ($)") +
  ylab(label = "Density" )+ xlab(label = "Price ($)" )+
  scale_fill_brewer(palette = "Set1")

#dificulty

ggplot(data = boardgames, aes(x= dificulty)) + 
  geom_density(aes(color = Game.Rank))+
  ggtitle("Density of Boardgames by Dificulty") +
  ylab(label = "Density" )+ xlab(label = "Dificulty to Understand" )+
  scale_fill_brewer(palette = "Set1")

#language Graph, seems not much to tell

ggplot(data = boardgames, aes(x= language)) + 
  geom_bar(aes(fill = Game.Rank), position ="fill")+
  ggtitle("Proportion of Boardgames by Language Requirement") +
  ylab(label = "Proportion" )+ scale_fill_brewer(palette = "Set1")
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





