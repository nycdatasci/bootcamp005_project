#Project 2 exploration

#Updated data as of May 3, 2016 & added tables for Kaggle Dataset & Kaggle Scripts 
#and added data on forums (three new tables: Forums, ForumMessages, and ForumTopics).

#need to turn off firewall to share app locally

library(dplyr)
library(ggplot2)

#open the location of my data

setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 2")

#load up my data sets

competitions = read.csv("Competitions.csv", stringsAsFactors = FALSE)

teams = read.csv("Teams.csv", stringsAsFactors = FALSE)

team.mem = read.csv("TeamMemberships.csv", stringsAsFactors = FALSE)

users = read.csv("Users.csv", stringsAsFactors = FALSE)

#join my data to create my main data set.

kteams = left_join( teams, team.mem,by = c("Id" = "TeamId"))%>%
  left_join( ., users,by = c("UserId" = "Id"))%>%
  left_join( ., competitions,by = c("CompetitionId" = "Id"))

#remove users who did not compete in competitions

team.group = filter(kteams, Points != NA | Points != 0.0)

#create leader data set

team.comp = left_join( teams, competitions,by = c("CompetitionId" = "Id"))

#add varaibles that count teamsize

team.group2 = group_by(team.group,Id,Title)%>%
  summarise(., TeamSize = n())%>%
  left_join(team.group,.,by = c("Id" = "Id") )

#make team size a factor

team.group2$TeamSize = as.factor(team.group2$TeamSize)

#finally account for the population of each teamsize in the dataset

tg.final = group_by(team.group2,TeamSize)%>%
  summarise(., TeamSizePop = n())%>%
  left_join(team.group2,.,by = c("TeamSize" = "TeamSize") )%>%
  mutate(., NewTeamSize = 1/TeamSizePop)%>%
  select(.,Id, TeamName, CompetitionId, TeamLeaderId, Ranking.x, Title.x, 
         BriefDescription, DateEnabled, CompetitionHostSegmentId, TeamSize, TeamSizePop,
         NewTeamSize)

#add varaibles that count number of competions participated in

team.comp2 = group_by(team.comp,TeamLeaderId)%>%
  summarise(., "NumberOfCompetitions" = n())%>%
  left_join(team.comp,.,by = c("TeamLeaderId" = "TeamLeaderId") )

#group those variable to be more legible.

t.c = mutate(team.comp2, CompGroup = ifelse(team.comp2$NumberOfCompetitions < 11,"1-10",
                                           ifelse(team.comp2$NumberOfCompetitions < 21,"11-20",
                                                  ifelse(team.comp2$NumberOfCompetitions < 31,"21-30",
                                                         ifelse(team.comp2$NumberOfCompetitions < 41,"31-40",
                                                                ifelse(team.comp2$NumberOfCompetitions < 51,"41-50",
                                                                       ifelse(team.comp2$NumberOfCompetitions < 61,"51-60",
                                                                              ifelse(team.comp2$NumberOfCompetitions < 71,"61-70",
                                                                                     ifelse(team.comp2$NumberOfCompetitions < 81,"71-80", 
                                                                                            ifelse(team.comp2$NumberOfCompetitions < 91,"81-90",
                                                                                                   "91-100"))))))))))

#account for the population of each group in the data set

tc.final = group_by(t.c,CompGroup)%>%
  summarise(., CompetitionsePop = n())%>%
  left_join(t.c,.,by = "CompGroup")%>%
  mutate(., NewNComp = 1/CompetitionsePop)%>%
  select(.,Id, TeamName, CompetitionId, TeamLeaderId, Ranking, Title, BriefDescription, DateEnabled,
         CompetitionHostSegmentId, NumberOfCompetitions, CompGroup,CompetitionsePop,
         NewNComp)

#selecting only certain variables has made some duplicate rows

tg.final = distinct(tg.final)

tc.final = distinct(tc.final)

#Save data to be used later

setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 2/HayesCozart/Data")
save(tg.final, file = "tg.final.RData")
save(tc.final, file = "tc.final.RData")



