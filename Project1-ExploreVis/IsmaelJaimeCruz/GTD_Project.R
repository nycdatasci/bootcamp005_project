# Global Terrorism Database

# Exploratory questions
# 1) Where do terrorist attacks most frequently occur?
# 2) How have terrorist attacks been increasing over the years in major cities?

# 4) How did incident information differ in terms of location?
# 5) How did incident information differ in terms of year?
# 6) Most common type of attack per year?
# 7) What is the relationship between kidnapping/hostage taking and casualty?
# 8) What location had the most "State Actor" events?
# 9) What locations had simulatneous attacks?
# 10) How did coordinated attacks change over time?
# 11) Most common weapon used per location?
# 12) What were the top days that had multiple terrorist attacks?
# 13) What criterion were the attacks most classified under?
# 14) How many attacks met all criteria?
# 15) Percent of doubt terrorism proper?
# 16) Weapon type for each year?
# 17) Common victim types per region?
# 18) Perpetrator group name with most attacks?
# 19) Common perpetrator group name per region?
# 20) In the last 3-5 years, how many attacks were done by perpetrators crossing
#     borders
# 21) Rank countries based on number of attacks then number of fatalities
# 22) Percentage type of attacks per year/5 years/10 years?
# Fatalities per attack?


# *What are the most relevant questions?
# * It will be interesting to see which terrorist attacks received the most media
#   coverage

# Flow
# Big picture (Number of terrorist attacks throughout the years)
# Types of terrorist attacks on a global scale
# Type of terrorist attacks on a per country level


library(openxlsx)
library(dplyr)
library(ggplot2)

gtd = read.xlsx('gtd_0615dist-2/globalterrorismdb_0615dist.xlsx')

# Attacks per year
attacks_per_year = gtd %>% group_by(iyear) %>% summarise(Attacks = n())
ggplot(data = attacks_per_year, aes(x = iyear, y = Attacks)) +
  geom_bar(stat = 'identity')
# What happened in the early 90s? Why did the trend of attacks go down?
# Why did attacks shoot up after 2011?

# Attacks per criteria
attacks_criteria = gtd %>% mutate(Attack_Criteria = paste(crit1, crit2, crit3)) %>%
  group_by(Attack_Criteria) %>% summarise(Attacks = n(),
      Attacks_Percent = round(Attacks / sum(attacks_criteria$Attacks)*100,2))
ggplot(attacks_criteria, aes(x = Attack_Criteria, y = Attacks_Percent)) +
  geom_bar(stat = 'identity')

# Attacks per criteria per year
attacks_criteria_per_year = gtd %>% mutate(Attack_Criteria = paste(crit1, crit2, crit3)) %>%
  group_by(iyear, Attack_Criteria) %>% summarise(Attacks = n())
# Stacked
ggplot(attacks_criteria_per_year, aes(x = iyear, y = Attacks, fill = Attack_Criteria)) +
  geom_bar(stat = 'identity', position = 'stack')
# Stacked with normalized height
ggplot(attacks_criteria_per_year, aes(x = iyear, y = Attacks, fill = Attack_Criteria)) +
  geom_bar(stat = 'identity', position = 'fill')
# Is it still worth looking at other kinds of attacks or just focus on those that 
# satisfy all criteria?

# Attacks by type
attack_type = gtd %>% group_by(attacktype1_txt) %>% summarise(Attacks = n(),
  Percent = round((Attacks/ sum(attack_type$Attacks)*100),2), Total = sum(attack_type$Attacks))
ggplot(data = attack_type, aes(x = '', y = Attacks, fill = attacktype1_txt)) +
  geom_bar(stat = 'identity', width = 1)

# Attacks by type per year
attack_type_year = gtd %>% group_by(iyear,attacktype1_txt) %>% summarise(Attacks = n())
ggplot(data = attack_type_year, aes(x = iyear, y = Attacks,
  fill = attacktype1_txt)) + geom_bar(stat = 'identity')

# Weapons used ranking
weapon_type = gtd %>% group_by(weaptype1_txt) %>% summarise(n())

# Targets
targets = gtd %>% group_by(targtype1_txt) %>% summarise(n())

# Casualties per year
casualties = gtd %>% group_by(iyear) %>% summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = casualties, aes(x = iyear, y = Casualties)) + geom_line()
  
# Casualties per year by attack type
casulaties_attack_type = gtd %>% group_by(iyear, attacktype1_txt) %>%
  summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = casulaties_attack_type, aes(x = iyear,
  y = Casualties, fill = attacktype1_txt)) + geom_bar(stat = 'identity',
                                                       position = 'dodge')

# Attacks per country
library(maps)
library(rworldmap)
attacks_per_country = gtd %>% group_by(country_txt) %>% summarise(n())

world = map_data(map = 'world')

ggplot() + 

  



