getwd()
setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 1")

library(ggplot2)
library(dplyr)

NYC_Death = read.csv('New_York_City_Leading_Causes_of_Death.csv')

NYC_Death2 = distinct(NYC_Death) # remove duplicate values

View(NYC_Death)# 2,880 entires

View(NYC_Death2)# 960 entries

str(NYC_Death2)
summary(NYC_Death2)

unique(NYC_Death2$Cause.of.Death) # causes of Death

#Make categories more legiable

levels(NYC_Death2$Sex)

levels(NYC_Death2$Sex)= c("Female", "Male")

levels(NYC_Death2$Ethnicity)

levels(NYC_Death2$Ethnicity) = c("Asian & Pacific Islander","Hispanic",
                                 "Black Non-Hispanic","White Non-Hispanic")

levels(NYC_Death2$Cause.of.Death) = c("Accidents","Alzheimers","Anemias",
                                      "Aortic Aneurysm & Dissection",
                                      "Homicide", "Atheroscerosis",
                                      "Benign & Uncertain Neoplasms",
                                      "Cardiovascular Disorder in Perinatal period",
                                      "Cerebrovascular Disease",
                                      "Disorders of the Gallbladder",
                                      "Chronic Liver Disease",
                                      "Chronic Lower Respiratory Disease",
                                      "Congenital Malformations",
                                      "Diabetes","Heart Diseases",
                                      "Hypertension and Kidney Diseases",
                                      "Immunodeficiency Virus",
                                      "Influenza & Pneumonia",
                                      "Suicide","Cancer",
                                      "Mental Disorder due to Alcohol",
                                      "Kidney Disease","Parkinsons",
                                      "Peptic Ulcer","Pnumonitis",
                                      "Pregnancy & Childbirth",
                                      "Accidental Drug Poisoning",
                                      "Respiratory Distress of Newborn",
                                      "Blood Poisoning","Short Gestation/LBW",
                                      "Tuberculosis","Hepatitis")

arrange(NYC_Death2, Count)%>%
ggplot(data = ., aes(x= Cause.of.Death, y=Count)) + 
  geom_bar(aes(fill = factor(Year)),stat = "identity")+
             scale_fill_brewer(palette = "Dark2")

#need to summarise by year or sex or cause of death

#example of how to reorder
g <- ggplot(data = mpg, aes(x = reorder(class, hwy), y = hwy))


#population would heavly affect these results


#Count of Deaths by Year
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(stat = "identity")

#Count of Deaths by Sex
ggplot(data = NYC_Death2, aes(x= Sex,y=Count)) + 
  geom_bar(stat = "identity")

#Count of Deaths by Cause
ggplot(data = NYC_Death2, aes(x= reorder(Cause.of.Death,Count),y=Count)) + 
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))


#Count of Deaths by Sex by Year
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")

#Count of Deaths by Ethnicity by Year
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")

#Count of Deaths by Cause of death by Year
ggplot(data = NYC_Death3, aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = reorder(Cause.of.Death,Count)), position ="dodge",stat = "identity")

#Count of Deaths by Cause of death by Sex
ggplot(data = NYC_Death2, aes(x= reorder(Cause.of.Death,Count),y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))

#Count of Deaths by Cause of death by Ethnicity
ggplot(data = NYC_Death2, aes(x= reorder(Cause.of.Death,Count),y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))


#Count of Deaths by Sex by Year by Ethnicity
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")+ 
  facet_wrap( ~ Ethnicity)

#Count of Deaths by Sex by Year by Cause
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")+ 
  facet_wrap( ~ Cause.of.Death)

#Count of Deaths by Ethnicity by Year by Cause
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")+ 
  facet_wrap( ~ Cause.of.Death)

#Count of death of females by cause
filter(NYC_Death2,Sex == 'FEMALE')%>%
  ggplot(data = ., aes(x= reorder(Cause.of.Death,Count),y=Count)) + 
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))

#top ten causes of Death
TOP = c("DISEASES OF HEART","MALIGNANT NEOPLASMS",
  "INFLUENZA AND PNEUMONIA","DIABETES MELLITUS",
  "CHRONIC LOWER RESPIRATORY DISEASES","CEREBROVASCULAR DISEASE",
  "HUMAN IMMUNODEFICIENCY VIRUS DISEASE","ACCIDENTS EXCEPT DRUG POISONING",
  "ESSENTIAL HYPERTENSION AND RENAL DISEASES",
  "PSYCH. SUBSTANCE USE & ACCIDENTAL DRUG POISONING")

filter(NYC_Death2, Cause.of.Death %in% TOP)%>%
  ggplot(data = ., aes(x= reorder(Cause.of.Death,Count),y=Count)) + 
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle = 90))

#Count of Deaths by TOP by Year
filter(NYC_Death2, Cause.of.Death %in% TOP)%>%
  ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = reorder(Cause.of.Death,Count)), position ="dodge",stat = "identity")

#Count of Deaths by TOP by Sex
filter(NYC_Death2, Cause.of.Death %in% TOP)%>%
ggplot(data = ., aes(x= reorder(Cause.of.Death,Count),y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))

#Count of Deaths by TOP by Ethnicity
filter(NYC_Death2, Cause.of.Death %in% TOP)%>%
ggplot(data = ., aes(x= reorder(Cause.of.Death,Count),y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))

#Count of Deaths by Sex by Year by TOP
filter(NYC_Death2, Cause.of.Death %in% TOP)%>%
ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")+ 
  facet_wrap( ~ Cause.of.Death)

#Count of Deaths by Ethnicity by Year by TOP
filter(NYC_Death2, Cause.of.Death %in% TOP)%>%
ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")+ 
  facet_wrap( ~ Cause.of.Death)

#Heart Attacks by ethnicity and sex.
filter(NYC_Death2, Cause.of.Death == "DISEASES OF HEART")%>%
  ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")+ 
  facet_wrap( Cause.of.Death ~ Sex)

#top 2 Causes by race and sex over time
filter(NYC_Death2, Cause.of.Death %in% c("DISEASES OF HEART","MALIGNANT NEOPLASMS"))%>%
  ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")+ 
  facet_wrap( Cause.of.Death ~ Sex)

#top 2 Causes by race and sex over time
filter(NYC_Death2, Cause.of.Death %in% c("DISEASES OF HEART","MALIGNANT NEOPLASMS"))%>%
  ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")+ 
  facet_wrap( Cause.of.Death ~ Ethnicity)

#Story to tell?

#Count of Deaths by Year
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(stat = "identity")

#Count of Deaths by Sex by Year
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")

#Count of Deaths by Ethnicity by Year
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")

#Count of Deaths by TOP by Year
filter(NYC_Death2, Cause.of.Death %in% TOP)%>%
  ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = reorder(Cause.of.Death,Count)), position ="dodge",stat = "identity")

#Heart Attacks by ethnicity and sex.
filter(NYC_Death2, Cause.of.Death == "DISEASES OF HEART")%>%
  ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")+ 
  facet_wrap( Cause.of.Death ~ Sex)

#top 2 Causes by race and sex over time
filter(NYC_Death2, Cause.of.Death %in% c("DISEASES OF HEART","MALIGNANT NEOPLASMS"))%>%
  ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Ethnicity), position ="dodge",stat = "identity")+ 
  facet_wrap( Cause.of.Death ~ Sex)

#top 2 Causes by sex and race over time
filter(NYC_Death2, Cause.of.Death %in% c("DISEASES OF HEART","MALIGNANT NEOPLASMS"))%>%
  ggplot(data = ., aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")+ 
  facet_wrap( Cause.of.Death ~ Ethnicity)



#maybe

#Count of Deaths by Cause of death by Sex
ggplot(data = NYC_Death2, aes(x= reorder(Cause.of.Death,Count),y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")+
  theme(axis.text.x=element_text(angle = 90, hjust = 0))

#Count of Deaths by Sex by Year by Ethnicity
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(aes(fill = Sex), position ="dodge",stat = "identity")+ 
  facet_wrap( ~ Ethnicity)

#Count of Deaths by Year by Ethnicity
ggplot(data = NYC_Death2, aes(x= Year,y=Count)) + 
  geom_bar(position ="dodge",stat = "identity")+ 
  facet_wrap( ~ Ethnicity)


#tests

#Count of Deaths by Cause by Year
group_by(NYC_Death2 ,Year,Cause.of.Death)%>%
  summarise(., Deaths = sum(Count))%>%
  arrange(., Deaths)%>%
  ggplot(data = ., aes(x= Year,y= Deaths)) + 
  geom_bar(aes(fill = reorder(Cause.of.Death, Deaths)), 
           position ="fill",stat = "identity")

#Count of Deaths by Cause by Year TOP Ten
filter(NYC_Death2, Cause.of.Death %in% TOP)%>%
group_by(. ,Year,Cause.of.Death)%>%
summarise(., Deaths = sum(Count))%>%
arrange(., Deaths)%>%
  ggplot(data = ., aes(x= Year,y= Deaths)) + 
  geom_bar(aes(fill = reorder(Cause.of.Death, Deaths)),
           position ="fill",stat = "identity") +
  ggtitle("Top Ten Causes of Death by Year") +
  ylab(label = "Percent of Deaths" )+
  scale_fill_discrete(name = "Cause of Death",
                      labels=c("Accidental Drug Poisoning",
                               "Hypertension and Kidney Diseases",
                               "Immunodeficiency Virus",
                               "Accidents",
                               "Stroke", 
                               "Chronic lower respiratory disease",
                               "Diabetes",
                               "Influenza & Pneumonia",
                               "Cancer",
                               "Heart Diseases"))

NYC_Death3 = read.csv('New_York_City_Leading_Causes_of_Death.csv')

levels(NYC_Death3$Sex)

levels(NYC_Death3$Sex)= c("Female", "Male")

levels(NYC_Death3$Ethnicity)

levels(NYC_Death3$Ethnicity) = c("Asian & Pacific Islander","Hispanic",
                                 "Black Non-Hispanic","White Non-Hispanic")

levels(NYC_Death3$Cause.of.Death) = c("Accidents","Alzheimers","Anemias",
                                      "Aortic Aneurysm & Dissection",
                                      "Homicide", "Atheroscerosis",
                                      "Benign & Uncertain Neoplasms",
                                      "Cardiovascular Disorder in Perinatal period",
                                      "Cerebrovascular Disease",
                                      "Disorders of the Gallbladder",
                                      "Chronic Liver Disease",
                                      "Chronic Lower Respiratory Disease",
                                      "Congenital Malformations",
                                      "Diabetes","Heart Diseases",
                                      "Hypertension and Kidney Diseases",
                                      "Immunodeficiency Virus",
                                      "Influenza & Pneumonia",
                                      "Suicide","Cancer",
                                      "Mental Disorder due to Alcohol",
                                      "Kidney Disease","Parkinsons",
                                      "Peptic Ulcer","Pnumonitis",
                                      "Pregnancy & Childbirth",
                                      "Accidental Drug Poisoning",
                                      "Respiratory Distress of Newborn",
                                      "Blood Poisoning","Short Gestation/LBW",
                                      "Tuberculosis","Hepatitis")

head(NYC_Death3)

