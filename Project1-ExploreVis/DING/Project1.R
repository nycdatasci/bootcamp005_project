########################################
## Prensentation1: Data Visualization ##
##    Ruonan Ding                     ##
########################################
##  HealthCare Carrier Information    ##
##  Data Source: www.cms.gov/CCIIO/Resources/Data-Resources/Downloads/
##  2015 information only             ##
#######################################

setwd("/Users/ruonanding/Desktop/DataFiles/")

library(ggplot2)
library(dplyr)
library(maps)
library(scales)
library(ggthemes)
library(MASS)

#load the raw datafile##############################################
RawRateFile<- read.csv('Rate.csv', header = T, sep= ',', stringsAsFactors = F)
rawbenefit <- read.csv('PlanAttributes.csv', header = T, sep = ',', stringsAsFactors = F)

##create subset of the 2015 dataset individul rate###################
rate2015 <- subset(RawRateFile, BusinessYear == "2015")

##There are seudo 9999 numbers for the ones that didn't have the individual rate
IndividualOption <- subset(rate2015, (rate2015$Age != "Family Option" & rate2015$IndividualRate < "9000"),
                             select = c(BusinessYear:IndividualTobaccoRate))

write.csv(IndividualOption, file="IndividualOption.csv", col.names = T, row.names = FALSE)
IndividualOption <- read.csv("IndividualOption.csv")
head(IndividualOption)

##Insurance companies may use different staff or the same staff, 
##but any health insurance company will have one issuer per state in which they are licensed to do business. 
##Registering an entity as an Issuer within HIOS will generate the unique Issuer ID. 

####################################################################
#Section1: How many players (insurance company) and plans available by state?
#This is to evaluate the accessibility of the state. 
bystate <- IndividualOption %>%
            select(StateCode, IssuerId, PlanId, IndividualRate)
bystatecount<-bystate %>% 
              group_by(StateCode) %>%
              summarize(Carriers = length(unique(IssuerId)), 
                        PlanOffered = length(unique(PlanId)),
                        MinIndRate= min(IndividualRate),
                        MedianIndRate = median(IndividualRate)) %>%
              arrange(desc(PlanOffered))

##Graph1 - Carriers vs. Plan Available By State
bystatecount$Carriers <- ifelse(bystatecount$Carriers<15, "(0,15)", ifelse(bystatecount$Carriers>=25, "[25,35)","[15,25)"))
graph1 <- ggplot(bystatecount, aes(x=reorder(StateCode, PlanOffered), y=PlanOffered))+
  geom_bar(aes(fill = Carriers), stat="identity")+
  ggtitle("Carriers vs. Plans Available By State")+
  labs(x="State", y="Plans Available")
graph1

#Graph2 - Map the median rate available by state on to the map.
#get state names
bystatecount$region <- tolower(state.name[match(bystatecount$StateCode,state.abb)])
#load US map
us_state_map = map_data('state')
statename<-group_by(us_state_map, region) %>% summarise(long = mean(long), lat = mean(lat))
mapdata <- left_join(bystatecount, us_state_map, by="region")

p <- ggplot()+geom_polygon(data=mapdata, aes(x=long, y=lat, group = group, fill=mapdata$MedianIndRate),colour="white")+
  scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
p1 <- p+theme_bw()+
  labs(fill = "Median Premium $/mon",title = "Median Monthly Premium Distribution", x="", y="")
p2<-p1 + scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())
graph2<-p2+geom_text(data=statename, aes(x=long, y=lat, label=region),  na.rm = T, size=2)+
  coord_map()
graph2

##Graph3 - BoxPlot of the provider groups
targetstate <- inner_join(IndividualOption, bystatecount, by="StateCode")
graph3 <- ggplot(targetstate, aes(x=Carriers, y=IndividualRate, fill=Carriers))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,1000))+
  stat_summary(fun.y = "mean",na.rm =T, geom="point",colour="darkred", size=1.5)+
  ggtitle("up to $1000 Ind Rate Distribution of grouped carriers")
graph3

###Now from the box plot we can conclude that the number of carriers available doesn't decide the affordability##
###so we will now look at the affordability by the two dimension: Age and Coverages

##Section2##########################################################################################
## benefit data scurbbing. Fun!!!! dim is 77353x176.

#first step is to pull 2015 Gold Silver Bronze ACA plans. 
benefit <- subset(rawbenefit, select = names(rawbenefit)[c(115,104,162,166,170)])
benefit$PlanId <- substr(benefit$PlanId, 1, 14)
IndividualOption$PlanId <- as.character(IndividualOption$PlanId)

#MOOP is not numeric. Need to scrub that. 
benefit$TEHBInnTier1IndividualMOOP <- gsub(",", "", benefit$TEHBInnTier1IndividualMOOP)
benefit$TEHBInnTier1IndividualMOOP <- gsub("\\$", "", benefit$TEHBInnTier1IndividualMOOP)
benefit$TEHBInnTier1IndividualMOOP <- as.numeric(benefit$TEHBInnTier1IndividualMOOP)
benefit$TEHBInnTier2IndividualMOOP <- gsub(",", "", benefit$TEHBInnTier2IndividualMOOP)
benefit$TEHBInnTier2IndividualMOOP <- gsub("\\$", "", benefit$TEHBInnTier2IndividualMOOP)
benefit$TEHBInnTier2IndividualMOOP <- as.numeric(benefit$TEHBInnTier2IndividualMOOP)
benefit$TEHBOutOfNetIndividualMOOP <- gsub(",", "", benefit$TEHBOutOfNetIndividualMOOP)
benefit$TEHBOutOfNetIndividualMOOP <- gsub("\\$", "", benefit$TEHBOutOfNetIndividualMOOP)
benefit$TEHBOutOfNetIndividualMOOP <- as.numeric(benefit$TEHBOutOfNetIndividualMOOP)

#Finalize the benefit table to use in the matching process
benefittouse <- group_by(benefit, PlanId, MetalLevel) %>%
  summarise(innettier1moop=mean(TEHBInnTier1IndividualMOOP),
            innettier2moop=mean(TEHBInnTier2IndividualMOOP),
            outnetmoop=mean(TEHBOutOfNetIndividualMOOP))

#join the benefit to the rates. This is the matched Rate and Benefit Level Table
planrates <- inner_join(IndividualOption, benefittouse, by="PlanId")

bystatecoverage <- group_by(planrates, StateCode, MetalLevel) %>%
  summarize(PlanOffered = length(unique(PlanId)),
            MedianIndRate= median(IndividualRate),
            MinIndRate = min(IndividualRate)) %>%
  arrange(desc(PlanOffered))

#graph4 <- ggplot(data=bystatecoverage, aes(x=reorder(StateCode, PlanOffered), y=PlanOffered))+
#  geom_bar(aes(fill=MetalLevel), stat="identity")+coord_flip()+
#  ggtitle("Number of Plans Offered By Coverage Levels")+
#  labs(x="Coverage Metal Level", y="Number of Plans Offered")
#graph4

#now exclude the dental coverage
medicalonly <- subset(bystatecoverage, bystatecoverage$MetalLevel != "High" & bystatecoverage$MetalLevel != "Low")

#State plans by coverage levels.  mostly offered are silver in a state. 
newdata <- group_by(medicalonly, StateCode) %>% summarise(avemin = mean(MinIndRate))
additionalgraph2 <- ggplot(data=newdata, aes(x=reorder(StateCode, avemin), y=avemin))+
  geom_bar(stat="identity")+
  ggtitle("Average Minimum Premium By State")+
  labs(x="State Order by Min Prem", y="Minimum Premium Per Month")
additionalgraph2 





graph5 <- ggplot(data=medicalonly, aes(x=reorder(StateCode, MinIndRate), y=MinIndRate))+
  geom_bar(stat="identity", aes(fill = MetalLevel))+
  facet_grid(. ~ MetalLevel)+
  ggtitle("Minimum Premium By State of Different Medical Plans")+
  labs(x="State Order by Min Prem", y="Minimum Premium Per Month")
graph5


new <- group_by(medicalonly, StateCode) %>% summarise(sum(PlanOffered))
new2 <- left_join(select(medicalonly, StateCode:PlanOffered), new, by = "StateCode")
new3 <- mutate(new2, percentage = PlanOffered / sum(PlanOffered))
additionalgraph <- ggplot(data=new3, aes(x=reorder(StateCode, PlanOffered), y=percentage))+
  geom_bar(stat="identity", aes(fill = MetalLevel))+
  ggtitle("Plan Type By State of Different Medical Plans")+
  labs(x="State Order by number of plan offered", y="% of Plan Types")
additionalgraph







#premium distribution by coverage levels in 2015. graph 6 box plot
graph6 <- ggplot(data=planrates, aes(x=MetalLevel, y=IndividualRate, fill=MetalLevel))+
  geom_boxplot()+
  stat_summary(fun.y="mean",na.rm =T, geom="point",colour="darkred", size=1.5)+
  ggtitle("Ind Rate Distribution of Coverage")
graph6

#now looking at this by age dimension
#by age group
byagecoverage <- select(planrates, Age, MetalLevel, IndividualRate) %>%
  group_by(Age, MetalLevel) %>%
  summarize(MeanIndRate= mean(IndividualRate),
            MedianIndRate = median(IndividualRate)) %>%
  arrange(desc(MeanIndRate))

graph7 <- ggplot(data=byagecoverage, aes(x=Age, y=MeanIndRate))+
  geom_point(stat='identity', size = 1.5, aes(color = factor(MetalLevel)))+
  ggtitle("Average Monthly Premium by Age")+
  labs(x="Age", y="Average Premium by Coverage")
graph7

#this graph shows that the spread in the higher age between higher ages are more significant.
################################################################################
medicalplanratesless1000 <- subset(planrates, MetalLevel != "High" & MetalLevel != "Low" & IndividualRate <= 1000)
medicalplanratesless1000$premiumgroup <- cut(medicalplanratesless1000$IndividualRate, 10)

medicalplanratesless1000$MetalLevel <- factor(x=medicalplanratesless1000$MetalLevel, levels = c("Platinum", "Gold", "Silver", "Bronze", "Catastrophic"))
graph8 <- ggplot(medicalplanratesless1000, aes(premiumgroup))+
  geom_bar(aes(fill=MetalLevel), position="dodge")+
  ggtitle("coverage offered in premiumg groups")+
  labs(x="Monthly Premium In Price Groups", y = "Number of Plan offered")
graph8

#now the goal is to look at the Bronze and Gold rate now and try to see their distribution
gold <- subset(planrates, MetalLevel == "Gold")
truehist(gold$IndividualRate)
lograte <- log(gold$IndividualRate)
truehist(lograte)

##add the fit gamma on the logloss data and graph it#
set.seed(123)

fit <- fitdistr(lograte, "gamma", start=list(shape = 1, rate = 0.5),lower=0.001)$estimate
sshape <- fit[1] #Alpha: shape
rrate <- fit[2] #Beta: rate
lines(density(rgamma(1000000,sshape,rrate)),col="red",lwd=3) #run about 1M iteration of the the fitted distribution
lines(density(lograte, kernel = "triangular"),  col="navy", lwd =3) #density function itself has kernel smoothing embedded
txt <- c("Gamma MLE Fit vs. Kernel Density Estimate for Gold Plan Rates")
title(txt, col.main ='navy')
legend("topleft", legend =c("Red: MLE","Blue: Kernal"), text.font = 20)

##bronze rate fit
bronze <- subset(planrates, MetalLevel == "Bronze")
truehist(bronze$IndividualRate)
logbronzerate <- log(bronze$IndividualRate)
truehist(logbronzerate)

fit <- fitdistr(logbronzerate, "gamma", start=list(shape = 1, rate = 0.5),lower=0.001)$estimate
sshape <- fit[1] #Alpha: shape
rrate <- fit[2] #Beta: rate
lines(density(rgamma(1000000,sshape,rrate)),col="red",lwd=3) #run about 1M iteration of the the fitted distribution
lines(density(logbronzerate, kernel = "triangular"),  col="navy", lwd =3) #density function itself has kernel smoothing embedded
txt <- c("Gamma MLE Fit vs. Kernel Density Estimate for Bronze Plan Rates")
title(txt, col.main ='navy')