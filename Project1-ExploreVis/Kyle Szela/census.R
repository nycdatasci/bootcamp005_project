#Load libraries.
library(ggplot2)
library(dplyr)
library(RColorBrewer)
setwd("~/Downloads/census")

#Get census data.
census <- read.csv("census-income.csv", stringsAsFactors = T, strip.white = T)

#######

#Subset by race and sex.
ratio_race_of_worker = group_by(census, race, sex) %>% 
  summarise(., 
            count_geq50 = sum(instance.weight[X == "50000+." & age >= 18]), 
            count_l50 = sum(instance.weight[X == "-50000" & age >= 18]),
            pct_geq50 = (count_geq50 / (count_l50 + count_geq50)) * 100)

#Plot graph.
racePlot_sex = ggplot(ratio_race_of_worker, aes(x = race, y = pct_geq50)) + 
  geom_bar(stat = "identity", aes(fill = race)) + 
  scale_fill_brewer(name = "Race", palette = "Set1") +
  ggtitle("Income by Race 94' - 95'") + 
  xlab("") + 
  ylab("Percent of Race with Income >50k") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + 
  facet_wrap( ~ sex)
racePlot_sex

#######

#Subset by only those with full time jobs, and by race and sex.
ratio_race_of_worker = census[(census$full.or.part.time.employment.stat == "Full-time schedules"),]

ratio_race_of_worker = group_by(ratio_race_of_worker, race, sex) %>% 
  summarise(., 
            count_geq50 = sum(instance.weight[X == "50000+." & age >= 18]), 
            count_l50 = sum(instance.weight[X == "-50000" & age >= 18]),
            pct_geq50 = (count_geq50 / (count_l50 + count_geq50)) * 100)

#Plot graph.
racePlot_sex = ggplot(ratio_race_of_worker, aes(x = race, y = pct_geq50)) + 
  geom_bar(stat = "identity", aes(fill = race)) + 
  scale_fill_brewer(name = "Race", palette = "Set1") +
  ggtitle("Income by Race 94' - 95'") + 
  xlab("") + 
  ylab("Percent of Race with Full-time Income >50k") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + 
  facet_wrap( ~ sex)
racePlot_sex

######

#Subset the census data by full-time, sector, and sex.
ratio_class_of_worker = census[!(census$class.of.worker == "Not in universe" | 
                                   census$class.of.worker == "Without pay"),]
ratio_class_of_worker = ratio_class_of_worker[(ratio_class_of_worker$full.or.part.time.employment.stat == "Full-time schedules"),]

ratio_class_of_worker = group_by(ratio_class_of_worker, class.of.worker, sex) %>% 
  summarise(., 
            count_geq50 = sum(instance.weight[X == "50000+." & age >= 18]), 
            count_l50 = sum(instance.weight[X == "-50000" & age >= 18]),
            pct_geq50 = (count_geq50 / (count_l50 + count_geq50)) * 100)

#Plot graph.
classPlot_sex = ggplot(ratio_class_of_worker, aes(x = class.of.worker, y = pct_geq50)) + 
  geom_bar(stat = "identity", aes(fill = class.of.worker)) + 
  scale_fill_brewer(name = "Class of Worker", palette = "Set1") + 
  #theme_bw() + 
  ggtitle("Income by Sector 94' - 95'") + 
  xlab("") + 
  ylab("Percent of Sector with Full-time Income >50k") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + 
  facet_wrap( ~ sex)
classPlot_sex

######

#Subset data by similarly, but for population percentage measurements.
ratio_class_of_worker_population = census[!(census$class.of.worker == "Not in universe" | 
                                              census$class.of.worker == "Without pay"),]

ratio_class_of_worker_population = ratio_class_of_worker_population[(ratio_class_of_worker_population$full.or.part.time.employment.stat == "Full-time schedules"),]

#Tally the total of males and females.
sector_totals = summarise(ratio_class_of_worker_population, 
                             Both = sum(instance.weight),
                             Males = sum(instance.weight[sex == "Male"]),
                             Females = sum(instance.weight[sex == "Female"]))

#Get percentages of population per sector.
sector_numbers_males = group_by(ratio_class_of_worker_population, class.of.worker, sex) %>%
  summarise(.,
            pct = (sum(instance.weight[sex == "Male"]) / sector_totals$Males) * 100)

sector_numbers_females = group_by(ratio_class_of_worker_population, class.of.worker, sex) %>%
  summarise(.,
            pct = (sum(instance.weight[sex == "Female"]) / sector_totals$Females) * 100)

sector_numbers_males$pct = sector_numbers_males$pct + sector_numbers_females$pct

#Male/female side by side plot.
sectorPlot_mf = ggplot(sector_numbers_males, aes(x = class.of.worker, y = pct)) + 
  geom_bar(stat = "identity", aes(fill = class.of.worker)) + 
  scale_fill_brewer(name = "Class of Worker", palette = "Set1") + 
  #theme_bw() + 
  ggtitle("Sector of Population 94' - 95'") + 
  xlab("") + 
  ylab("Percent of Population in Sector") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + 
  facet_wrap( ~ sex)
sectorPlot_mf

######

#Subset census by those with full-time jobs and are 18 or above.
ratio_age_census = census[(census$full.or.part.time.employment.stat == "Full-time schedules" & 
                             census$age >= 18 & 
                             census$age <= 70),]

#Get percentage of each age with selected income.

#Both
ratio_age = group_by(ratio_age_census, age) %>% 
  summarise(.,
            Both = (sum(instance.weight[X == "50000+." & age >= 18]) / 
                      (sum(instance.weight[X == "50000+." & age >= 18]) + 
                         sum(instance.weight[X == "-50000" & age >= 18]))) * 100)
#Just males
ratio_age_male = group_by(ratio_age_census, age) %>%
  summarise(.,
            Males = (sum(instance.weight[X == "50000+." & sex == "Male" & age >= 18]) / 
                       (sum(instance.weight[X == "50000+." & sex == "Male" & age >= 18]) + 
                          sum(instance.weight[X == "-50000" & sex == "Male" & age >= 18]))) * 100)
#Just females
ratio_age_female = group_by(ratio_age_census, age) %>%
  summarise(.,
            Females = (sum(instance.weight[X == "50000+." & sex == "Female" & age >= 18]) / 
                         (sum(instance.weight[X == "50000+." & sex == "Female" & age >= 18]) + 
                            sum(instance.weight[X == "-50000" & sex == "Female" & age >= 18]))) * 100)

#Inner join all three together.
ratio_age = inner_join(ratio_age, ratio_age_male, by = "age")
ratio_age = inner_join(ratio_age, ratio_age_female, by = "age")

#Stack them for ggplot2.
ratio_age_stacked = with(ratio_age,
                         data.frame(value = c(Both, Males, Females),
                                    variable = factor(rep(c("Both", "Males", "Females"),
                                                          each = NROW(ratio_age))),
                                    age = rep(age, 3)))

#Plot graph.
agePlot = ggplot(ratio_age_stacked, aes(age, value, colour = variable)) + 
  geom_line() +
  theme(legend.title=element_blank()) +
  ggtitle("Income by Age 94' - 95'") + 
  xlab("Age (years)") + 
  ylab("Percent at Age with Full-time Income >50k")
agePlot

######

#Looking at female to male education factor and earnings
#First graph number of females and males for each education level
#Subset census data by those making full-time incomes with age >=18.
education_numbers = census[(census$full.or.part.time.employment.stat == "Full-time schedules" & 
                              census$age >= 18),]

#New levels by increasing education.
new_levels = c("Children", "Less than 1st grade", "1st 2nd 3rd or 4th grade", 
               "5th or 6th grade", "7th and 8th grade", "9th grade", "10th grade", 
               "11th grade", "12th grade no diploma", "High school graduate", 
               "Associates degree-academic program", "Associates degree-occup /vocational", 
               "Some college but no degree", "Bachelors degree(BA AB BS)", 
               "Masters degree(MA MS MEng MEd MSW MBA)", "Doctorate degree(PhD EdD)", 
               "Prof school degree (MD DDS DVM LLB JD)")

#Refactoring
education_numbers$education = factor(education_numbers$education, levels = new_levels)

#Tally the totals for each level of education.
education_totals = summarise(education_numbers, 
                             Both = sum(instance.weight),
                             Males = sum(instance.weight[sex == "Male"]),
                             Females = sum(instance.weight[sex == "Female"]))

education_numbers_both = group_by(education_numbers, education) %>%
  summarise(.,
            Both = (sum(instance.weight) / education_totals$Both) * 100)

education_numbers_males = group_by(education_numbers, education, sex) %>%
  summarise(.,
            pct = (sum(instance.weight[sex == "Male"]) / education_totals$Males) * 100)

education_numbers_females = group_by(education_numbers, education, sex) %>%
  summarise(.,
            pct = (sum(instance.weight[sex == "Female"]) / education_totals$Females) * 100)

education_numbers_males$pct = education_numbers_males$pct + education_numbers_females$pct

#Getting education by income
education_income = group_by(education_numbers, education, sex) %>%
  summarise(., 
            count_geq50 = sum(instance.weight[X == "50000+." & age >= 18]), 
            count_l50 = sum(instance.weight[X == "-50000" & age >= 18]),
            pct = (count_geq50 / (count_l50 + count_geq50)) * 100) %>%
  select(., education, sex, pct)

#Stack data for ggplot2
population.ident = rep("Percent of Total Population", NROW(education_numbers_males))
income.ident = rep("Percent with Certain Education with Income >$50,000", NROW(education_income))

education_numbers_males$Identifier = population.ident
education_income$Identifier = income.ident

education_stacked = rbind(education_numbers_males, education_income)

#Male/female side by side education/income vertically
educationPlot_mf = ggplot(education_stacked, aes(x = education, y = pct)) + 
  geom_bar(stat = "identity", aes(fill = education)) + 
  theme_bw() + 
  scale_fill_discrete(name = "Education") + 
  ggtitle("Education Grid 94' - 95'") + 
  xlab("") + 
  ylab("") + 
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + 
  facet_grid(Identifier ~ sex)
 # facet_wrap( ~ sex) + 
 # facet_wrap( ~ Identifier)
educationPlot_mf


