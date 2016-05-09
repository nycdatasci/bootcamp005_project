setwd('~/Desktop/Data')

library(dplyr)

# -------- 2014 DATA MUNGING --------
p2014 <- read.csv('NP2014_D1.csv', header=TRUE)
b2014 <- read.csv('NP2014_D2.csv', header=TRUE)
d2014 <- read.csv('NP2014_D3.csv', header=TRUE)
m2014 <- read.csv('NP2014_D4.csv', header=TRUE)

bdm2014 <- b2014 %>% merge(d2014) %>% merge(m2014) # Merge births, deaths, migration
colnames(bdm2014)[1:3] <- c("race", "sex", "year") # Rename RACE_HISP to RACE
bdm2014 <- filter(bdm2014, race != 9) # Remove excess terms in race

names(bdm2014) <- tolower(names(bdm2014)) # Make all variables lower case
names(p2014) <- tolower(names(p2014)) # Make all variables lower case
pop <- filter(p2014, race %in% c(0:6)) # Remove excess terms in race

for (i in 2014:2060) { # Create new rows with Hispanic and non-Hispanic in the race column
  pop <- rbind(pop,filter(pop, (origin == 1) & (race == 0) & (sex == 0) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 7
  pop <- rbind(pop,filter(pop, (origin == 1) & (race == 0) & (sex == 1) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 7
  pop <- rbind(pop,filter(pop, (origin == 1) & (race == 0) & (sex == 2) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 7
  pop <- rbind(pop,filter(pop, (origin == 2) & (race == 0) & (sex == 0) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 8
  pop <- rbind(pop,filter(pop, (origin == 2) & (race == 0) & (sex == 1) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 8
  pop <- rbind(pop,filter(pop, (origin == 2) & (race == 0) & (sex == 2) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 8
}

pop <- filter(pop, origin == 0) # Filter ignoring origin
pop$origin <- NULL # Remove origin column 
final2014 <- merge(bdm2014, pop) # Merged to get complete table

saveRDS(final2014, "dat2014.rds") # Export to RDS file



# -------- 2012 DATA MUNGING --------
p2012 <- read.csv('NP2012_D1.csv', header=TRUE)
b2012 <- read.csv('NP2012_D2.csv', header=TRUE)
d2012 <- read.csv('NP2012_D3.csv', header=TRUE)
m2012 <- read.csv('NP2012_D4.csv', header=TRUE)

bdm2012 <- b2012 %>% merge(d2012) %>% merge(m2012) # Merge births, deaths, migration
colnames(bdm2012)[1:3] <- c("race", "sex", "year") # Rename RACE_HISP to RACE
bdm2012 <- filter(bdm2012, race != 9) # Remove excess terms in race

names(bdm2012) <- tolower(names(bdm2012)) # Make all variables lower case
names(p2012) <- tolower(names(p2012)) # Make all variables lower case
pop <- filter(p2012, race %in% c(0:6)) # Remove excess terms in race

for (i in 2012:2060) { # Create new rows with Hispanic and non-Hispanic in the race column
  pop <- rbind(pop,filter(pop, (origin == 1) & (race == 0) & (sex == 0) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 7
  pop <- rbind(pop,filter(pop, (origin == 1) & (race == 0) & (sex == 1) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 7
  pop <- rbind(pop,filter(pop, (origin == 1) & (race == 0) & (sex == 2) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 7
  pop <- rbind(pop,filter(pop, (origin == 2) & (race == 0) & (sex == 0) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 8
  pop <- rbind(pop,filter(pop, (origin == 2) & (race == 0) & (sex == 1) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 8
  pop <- rbind(pop,filter(pop, (origin == 2) & (race == 0) & (sex == 2) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 8
}

pop <- filter(pop, origin == 0) # Filter ignoring origin
pop$origin <- NULL # Remove origin column 
final2012 <- merge(bdm2012, pop) # Merged to get complete table

saveRDS(final2012, "dat2012.rds")



# -------- 2008 DATA MUNGING --------
p2008 <- read.csv('NP2008_D1.csv', header=TRUE)
b2008 <- read.csv('NP2008_D2.csv', header=TRUE)
d2008 <- read.csv('NP2008_D3.csv', header=TRUE)
m2008 <- read.csv('NP2008_D4.csv', header=TRUE)

bdm2008 <- b2008 %>% merge(d2008) %>% merge(m2008) # Merge births, deaths, migration
names(bdm2008) <- tolower(names(bdm2008)) # Make all variables lower case
names(p2008) <- tolower(names(p2008)) # Make all variables lower case
bdm2008 <- filter(bdm2008, race %in% c(0:6)) # Remove excess terms in race
pop <- filter(p2008, race %in% c(0:6)) # Remove excess terms in race
colnames(bdm2008)[1] <- "origin"
colnames(pop)[1] <- "origin"

for (i in 2000:2050) { # Create new rows with Hispanic and non-Hispanic in the race column
  pop <- rbind(pop,filter(pop, (origin == 1) & (race == 0) & (sex == 0) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 7
  pop <- rbind(pop,filter(pop, (origin == 1) & (race == 0) & (sex == 1) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 7
  pop <- rbind(pop,filter(pop, (origin == 1) & (race == 0) & (sex == 2) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 7
  pop <- rbind(pop,filter(pop, (origin == 2) & (race == 0) & (sex == 0) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 8
  pop <- rbind(pop,filter(pop, (origin == 2) & (race == 0) & (sex == 1) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 8
  pop <- rbind(pop,filter(pop, (origin == 2) & (race == 0) & (sex == 2) & (year == i)))
  pop$origin[length(pop$origin)] = 0
  pop$race[length(pop$race)] = 8
}

pop <- filter(pop, origin == 0) # Filter ignoring origin
final2008 <- merge(bdm2008, pop) # Merged to get complete table
final2008$origin <- NULL # Remove origin column

saveRDS(final2008, "dat2008.rds")
