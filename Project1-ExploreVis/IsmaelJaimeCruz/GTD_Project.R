# How have terrorist attacks been increasing over the years in major cities?
# Top cities with the most number of attacks?
# Fatalities of major cities?
# Fatalities of cities with most number of attacks?
# Common victim types per region?
# Rank countries based on number of attacks then number of fatalities
# Fatalities per attack?
# Types of terrorist attacks on a global scale
# Type of terrorist attacks on a per country level
# What happened in the early 90s? Why did the trend of attacks go down?
# Why did attacks shoot up after 2011?
# What were the countries that started experiencing more casualties from bombings in
# recent years?

library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(maps)

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
  xlab('Year') + scale_fill_gradient(low = 'red', high = 'red4') + guides(fill = F) +
  theme_solarized(light = F)
ggsave('attacks_by_year.png')

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
  geom_bar(stat = 'identity', width = 1, position = 'dodge') +
  ggtitle('Terrorist Attacks by Type') + xlab('') +
  scale_fill_brewer(palette = 'RdBu', name = '') + theme_solarized(light = F)
ggsave('attacks_by_type.png')

# Attacks by type per year
attack_type_year = gtd %>% group_by(iyear,attacktype1_txt) %>% summarise(Attacks = n())
ggplot(data = attack_type_year, aes(x = iyear, y = Attacks,
  fill = attacktype1_txt)) + geom_bar(stat = 'identity', position = 'stack') +
  ggtitle('Attack Type by Year') + xlab('Year') +
  scale_fill_brewer(palette = 'RdBu', name = '')
ggsave('attacktype_by_year.png')

# Attacks by type per year (Armed Assault, Bombing/Explosion, Assassination)
attack_type_year2 = gtd %>% 
  filter(attacktype1_txt == c('Armed Assault', 'Assassination', 'Bombing/Explosion')) %>%
  group_by(iyear,attacktype1_txt) %>%
  summarise(Attacks = n())
ggplot(data = attack_type_year2, aes(x = iyear, y = Attacks,
  fill = attacktype1_txt)) + geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle('Attack Type by Year') + xlab('Year') +
  scale_fill_brewer(palette = 'RdBu', name = '') + theme_solarized(light = F)
ggsave('attacktype_by_year.png')

# Weapon type used
weapon_type = gtd %>% group_by(weaptype1_txt) %>% summarise(Count = n())
weapon_type$Percent = round((weapon_type$Count/ sum(weapon_type$Count)*100),2)
weapon_type$Total = sum(weapon_type$Count)

ggplot(data = weapon_type, aes(x = 'Weapon Types', y = Percent, fill = weaptype1_txt)) +
  geom_bar(stat = 'identity', width = 1) +
  ggtitle('Different Types of Weapons Used') + xlab('') +
  scale_fill_brewer(palette = 'RdBu', name = '')

# Targets
targets = gtd %>% group_by(targtype1_txt) %>% summarise(n())

# Casualties per year
casualties = gtd %>% group_by(iyear) %>% summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = casualties, aes(x = iyear, y = Casualties, fill = iyear)) + geom_bar(stat = 'identity') +
  ggtitle('Casualties from Terrorist Attacks from 1970 to 2014') +
  xlab('Year') + scale_fill_gradient(low = 'red', high = 'red4') + guides(fill = F) +
  theme_solarized(light = F)
ggsave('casualties_by_year.png')


# Casualties per year by attack type
casulaties_attack_type = gtd %>% group_by(iyear, attacktype1_txt) %>%
  summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = casulaties_attack_type, aes(x = iyear,
  y = Casualties, fill = attacktype1_txt)) + geom_bar(stat = 'identity',
  position = 'stack') +
  ggtitle('Casualties per Attack Type by Year') + xlab('Year') +
  scale_fill_brewer(palette = 'RdBu', name = '')
ggsave('casualties_attack_type.png')

# Casualties per year by attack type (Armed Assault, Bombing/Explosion, Assassination)
casulaties_attack_type2 = gtd %>% 
  filter(attacktype1_txt == c('Armed Assault', 'Assassination', 'Bombing/Explosion')) %>%
  group_by(iyear, attacktype1_txt) %>%
  summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = casulaties_attack_type2, aes(x = iyear,
  y = Casualties, fill = attacktype1_txt)) + geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle('Casualties from Attack Type by Year') + xlab('Year') +
  scale_fill_brewer(palette = 'RdBu', name = '') + theme_solarized(light = F)
ggsave('casualties_attack_type.png')

# How have terrorist attacks been increasing over the years in major cities?
global_elite = c('New York City', 'Los Angeles', 'Chicago', 'Toronto', 'San Francisco',
                 'Boston', 'London', 'Paris', 'Brussels', 'Berlin', 'Amsterdam',
                 'Tokyo', 'Singapore', 'Seoul', 'Sydney', 'Melbourne')
global_elite_attacks = gtd %>% filter(city %in% global_elite) %>%
  group_by(iyear) %>% summarise(Attacks = n())
ggplot(data = global_elite_attacks, aes(x = iyear, y = Attacks, fill = iyear)) +
  geom_bar(stat = 'identity') + ggtitle('Terrorist Attacks in Global Elite Cities from 1970 to 2014') +
  xlab('Year') + scale_fill_gradient(low = 'red', high = 'red4') + guides(fill = F) +
  theme_solarized(light = F)
mean(global_elite_attacks$Attacks)
ggsave('gec_attacks.png')

# Top 16 cities by most number of attacks
top_cities = gtd %>% group_by(city) %>% summarise(Attacks = n()) %>%
    arrange(desc(Attacks))
top_16_cities =  top_n(top_cities, 17)[-1,]
top_16_cities_attacks = gtd %>% filter(city %in% top_16_cities$city) %>%
  group_by(iyear) %>% summarise(Attacks = n())
ggplot(data = top_16_cities_attacks, aes(x = iyear, y = Attacks, fill = iyear)) +
  geom_bar(stat = 'identity') + ggtitle('Terrorist Attacks in Top 16 Frequently Attacked Cities from 1970 to 2014') +
  xlab('Year') + scale_fill_gradient(low = 'red', high = 'red4') + guides(fill = F) +
  theme_solarized(light = F)
mean(top_16_cities_attacks$Attacks)
unique(gtd$region_txt[gtd$city %in% top_16_cities$city])
ggsave('t16_attacks.png')

# Casualties in global elite cities
global_elite_casualties = gtd %>% filter(city %in% global_elite) %>%
  group_by(iyear) %>% summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = global_elite_casualties, aes(x = iyear, y = Casualties, fill = iyear)) + geom_bar(stat = 'identity') +
  ggtitle('Terrorist Attack Casualties in Global Elite Cities from 1970 to 2014') +
  xlab('Year') + scale_fill_gradient(low = 'red', high = 'red4') + guides(fill = F) +
  theme_solarized(light = F)
sum(global_elite_casualties$Casualties)
mean(global_elite_casualties$Casualties)
ggsave('gec_casualties.png')

# Casualties in top 16 cities
top_16_cities_casualties = gtd %>% filter(city %in% top_16_cities$city) %>%
  group_by(iyear) %>% summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = top_16_cities_casualties, aes(x = iyear, y = Casualties, fill = iyear)) +
  geom_bar(stat = 'identity') + ggtitle('Casualties in Top 16 Frequently Attacked Cities from 1970 to 2014') +
  xlab('Year') + scale_fill_gradient(low = 'red', high = 'red4') + guides(fill = F) +
  theme_solarized(light = F)
sum(top_16_cities_casualties$Casualties)
mean(top_16_cities_casualties$Casualties)
ggsave('t16_casualties.png')

# Global map of attacks
world_map = map_data('world')
world_map = subset(world_map, region!="Antarctica")
ggplot(data = attacks_per_country) +
  geom_map(data = world_map, map = world_map, aes(map_id = region), fill = 'white',
  color = "#7f7f7f", size = 0.25) + geom_map(map = world_map,
  aes(map_id = region, fill = Attacks), size=0.25) + scale_fill_gradient(low="#fff7bc",
  high="#cc4c02", name="Terrorist Attacks") + expand_limits(x = world_map$long, y = world_map$lat) +
  labs(x="", y="", title="Terrorist Attacks by Country") + 
  theme(panel.grid=element_blank(), panel.border=element_blank()) +
  theme(axis.ticks=element_blank(), axis.text=element_blank()) +
  theme(legend.position="top")



# Global elite attacks map
global_elite_map = gtd %>% filter(city %in% global_elite) %>%
  group_by(subregion = city) %>% summarise(Casualties = sum(nkill, na.rm = T))
ggplot(data = global_elite_map) +
  geom_map(data = world_map, map = world_map, aes(map_id = subregion), fill = 'white', color = "#7f7f7f", size = 0.25) +
  geom_map(map = world_map, aes(map_id = subregion, fill = Casualties), size = 0.25) +
  scale_fill_gradient(low="#fff7bc", high="#cc4c02", name="Casualties") +
  expand_limits(x = world_map$long, y = world_map$lat) +
  labs(x="", y="", title="Terrorist Attacks by Country") + 
  theme(panel.grid=element_blank(), panel.border=element_blank()) +
  theme(axis.ticks=element_blank(), axis.text=element_blank()) +
  theme(legend.position="top")




# Attacks per country
attacks_per_country = gtd %>% group_by(region = country_txt) %>% summarise(Attacks = n())

library(googleVis)
plot(gvisGeoChart(attacks_per_country, "country_txt", "Attacks",
                  options=list(projection="kavrayskiy-vii")))
