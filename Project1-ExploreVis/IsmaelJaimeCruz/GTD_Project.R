# Global Terrorism Database

# Exploratory questions

# How have terrorist attacks been increasing over the years in major cities?
# Most common weapon used per location?
# What criterion were the attacks most classified under?
# How many attacks met all criteria?
# Weapon type for each year?
# Common victim types per region?
# In the last 3-5 years, how many attacks were done by perpetrators crossing
#   borders
# Rank countries based on number of attacks then number of fatalities
# Percentage type of attacks per year/5 years/10 years?
# Fatalities per attack?
# Types of terrorist attacks on a global scale
# Type of terrorist attacks on a per country level


library(openxlsx)
library(dplyr)
library(ggplot2)

gtd = read.xlsx('globalterrorismdb_0615dist.xlsx')

# Deaths per year
sum(gtd$nkill, na.rm = T) / length(unique(gtd$iyear))

# Attacks per year
length(gtd$eventid) / length(unique(gtd$iyear))

# Attacks in the US
gtd_US = filter(gtd, country_txt == 'United States')
length(gtd_US$eventid) / length(unique(gtd_US$iyear))

# Attacks by year
attacks_per_year = gtd %>% group_by(iyear) %>% summarise(Attacks = n())
ggplot(data = attacks_per_year, aes(x = iyear, y = Attacks, fill = iyear)) +
  geom_bar(stat = 'identity') + ggtitle('Terrorist Attacks from 1970 to 2014') +
  xlab('Year') + scale_fill_gradient(low = 'red', high = 'red4') + guides(fill = F)
ggsave('attacks_by_year.png')
# What happened in the early 90s? Why did the trend of attacks go down?
# Why did attacks shoot up after 2011?

# Attacks by criteria
attacks_criteria = gtd %>% mutate(Attack_Criteria = paste(crit1, crit2, crit3)) %>%
  group_by(Attack_Criteria) %>% summarise(Attacks = n())
attacks_criteria$Attacks_Percent = round(attacks_criteria$Attacks /
                                           sum(attacks_criteria$Attacks)*100,2)
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
attack_type = gtd %>% group_by(attacktype1_txt) %>% summarise(Attacks = n())
attack_type$Percent = round((attack_type$Attacks/ sum(attack_type$Attacks)*100),2)
attack_type$Total = sum(attack_type$Attacks)
ggplot(data = attack_type, aes(x = 'Attack Types', y = Percent, fill = attacktype1_txt)) +
  geom_bar(stat = 'identity', width = 1) +
  ggtitle('Different Types of Terrorist Attacks') + xlab('') +
  scale_fill_brewer(palette = 'RdBu', name = '')
ggsave('attacks_by_type.pdf')

# Attacks by type per year
attack_type_year = gtd %>% group_by(iyear,attacktype1_txt) %>% summarise(Attacks = n())
ggplot(data = attack_type_year, aes(x = iyear, y = Attacks,
  fill = attacktype1_txt)) + geom_bar(stat = 'identity') +
  ggtitle('Attack Type by Year') + xlab('Year') +
  scale_fill_brewer(palette = 'RdBu', name = '')
ggsave('attacktype_by_year.pdf')

# Weapon type used
weapon_type = gtd %>% group_by(weaptype1_txt) %>% summarise(Count = n())
weapon_type$Percent = round((weapon_type$Count/ sum(weapon_type$Count)*100),2)
weapon_type$Total = sum(weapon_type$Count)

ggplot(data = weapon_type, aes(x = 'Weapon Types', y = Percent, fill = weaptype1_txt)) +
  geom_bar(stat = 'identity', width = 1) +
  ggtitle('Different Types of Weapons Used') + xlab('') +
  scale_fill_brewer(palette = 'RdBu', name = '')
ggsave('weapon_type.pdf')

# Targets
targets = gtd %>% group_by(targtype1_txt) %>% summarise(n())

# Casualties per year
casualties = gtd %>% group_by(iyear) %>% summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = casualties, aes(x = iyear, y = Casualties, fill = iyear)) + geom_bar(stat = 'identity') +
  ggtitle('Casualties from Terrorist Attacks from 1970 to 2014') +
  xlab('Year') + scale_fill_gradient(low = 'red', high = 'red4') + guides(fill = F)
ggsave('casualties_by_year.pdf')


# Casualties per year by attack type
casulaties_attack_type = gtd %>% group_by(iyear, attacktype1_txt) %>%
  summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = casulaties_attack_type, aes(x = iyear,
  y = Casualties, fill = attacktype1_txt)) + geom_bar(stat = 'identity',
  position = 'stack') +
  ggtitle('Casualties per Attack Type by Year') + xlab('Year') +
  scale_fill_brewer(palette = 'RdBu', name = '')
ggsave('casualties_attack_type.pdf')


# Attacks per country
attacks_per_country = gtd %>% group_by(country_txt) %>% summarise(Attacks = n())

library(googleVis)
plot(gvisGeoChart(attacks_per_country, "country_txt", "Attacks",
                  options=list(projection="kavrayskiy-vii")))
