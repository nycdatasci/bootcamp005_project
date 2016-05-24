library(dplyr)
library(ggplot2)
library(car)
setwd('~/Desktop/Project3')
co = read.csv('~/Desktop/Project3/fundco.csv', stringsAsFactors = FALSE)
acc = read.csv('~/Desktop/Project3/accelerators.csv', stringsAsFactors = FALSE)
summary(co)
str(co)
summary(acc)
str(acc)
#remove dollor sign and comma, covert char to numeric 
co$Fundings = sapply(co$Fundings, function(x) return(gsub("[,$]", "", x)))
co$Fundings = as.numeric(co$Fundings)

#density Fundings 
ggplot(data=co, aes(x=log(Fundings))) + 
  geom_density(aes(color='red')) +
  ggtitle('Company Funding Density')

acc$Accelerator = sapply(acc$Accelerator, function(x) return(gsub(" Link", "", x)))
acc$Funding = sapply(acc$Funding, function(x) return(gsub("[,$]", "", x)))
acc$Funding = as.numeric(acc$Funding)
acc$Average = sapply(acc$Average, function(x) return(gsub("[,$]", "", x)))
acc$Average = as.numeric(acc$Average)

#acc$Ranking <- scale(acc$Average, center=FALSE, scale=TRUE)
#acc$Ranking[acc$Dead == 'Dead'] <- 0

acc <- acc %>% 
  mutate(AccRank = dense_rank(Average)) %>%
  mutate(AccRank = replace(AccRank, Dead=='Dead', NA)) %>%
  mutate(AccRank = dense_rank(AccRank))

acc <- acc[order(acc$AccRank), ]

#merge two data frames
coacc <- merge(x = co, y = acc, by = 'Accelerator', all.x = TRUE)

coacc$CoFund <- scale(coacc$Fundings, center=FALSE, scale=TRUE)
#normalize to 0 to 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
coacc$CoFund <- range01(coacc$CoFund)

coacc$AccRank <- scale(coacc$AccRank, center=FALSE, scale=TRUE)
coacc <- coacc[!is.na(coacc$AccRank), ]
coacc$AccRank <- range01(coacc$AccRank)

#Company CoFund vs Rounds
ggplot(data=coacc, aes(x=Rounds, y=log(CoFund))) + 
  geom_point(size=0.5, alpha=0.5) + 
  geom_smooth(method="lm") +
  ggtitle('Seed Company Fudning vs. Rounds')
#linear relationship: take CoFund

#AccRank vs CoFund
#ggplot(data=coacc, aes(x=AccRank, y=log(CoFund))) + 
#  geom_point(size=0.5, alpha=0.5) + 
#  geom_smooth(method="lm")


#formula 
coacc$Score = 0.5*coacc$AccRank + 0.5*coacc$CoFund

coacc$Score[coacc$State == 'Dead'] <- 0


#density Score
ggplot(data=coacc, aes(x=Score)) + geom_density(aes(color=State)) +
  ggtitle('Score Density Distribution')



#boxplot Score vs State 
ggplot(data=coacc, aes(x=reorder(State, Score, median), y=Score)) + 
  geom_boxplot(aes(fill=State)) +
  ggtitle('Score by State')

top <- coacc[ ,c('Company.Name', 'Score', 'Accelerator', 'State', 'Fundings' )]


#Company Name vs Score
#ggplot(data=coacc, aes(x=Company.Name, y=Score)) + 
#  geom_point(position='jitter', size=0.5, alpha=0.5, aes(color=State))


#density CoFund
#ggplot(data=coacc, aes(x=log(CoFund))) + geom_density()

