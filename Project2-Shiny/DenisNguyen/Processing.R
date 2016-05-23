########## Load required libraries ##########
library(dplyr)


########## 2014 DATA PROCESSING ##########
### Read 2014 data files
p2014 <- read.csv('NP2014_D1.csv', header=TRUE)
b2014 <- read.csv('NP2014_D2.csv', header=TRUE)
d2014 <- read.csv('NP2014_D3.csv', header=TRUE)
m2014 <- read.csv('NP2014_D4.csv', header=TRUE)

### Merging datasets, removing extra rows, renaming columns for easier processing
bdm2014 <- b2014 %>% merge(d2014) %>% merge(m2014)
colnames(bdm2014)[1:3] <- c("race", "sex", "year")
bdm2014 <- filter(bdm2014, race != 9)
names(bdm2014) <- tolower(names(bdm2014))
names(p2014) <- tolower(names(p2014))
pop <- filter(p2014, race %in% c(0:6))

### Create new rows with Hispanic and non-Hispanic in the race column to match bdm2014
for (i in 2014:2060) {
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
pop <- filter(pop, origin == 0) # Filter for only Hispanic/Non-Hispanic terms
pop$origin <- NULL # Remove origin column (now deprecated)

### Merging to get the final table and renaming columns
final2014 <- merge(bdm2014, pop)
colnames(final2014)[c(5,92,179)] <- c("deaths","migration","total population")

saveRDS(final2014, "dat2014.rds")


########## 2012 DATA PROCESSING ##########
### Read 2012 data files
p2012 <- read.csv('NP2012_D1.csv', header=TRUE)
b2012 <- read.csv('NP2012_D2.csv', header=TRUE)
d2012 <- read.csv('NP2012_D3.csv', header=TRUE)
m2012 <- read.csv('NP2012_D4.csv', header=TRUE)

### Merging datasets, removing extra rows, renaming columns for easier processing
bdm2012 <- b2012 %>% merge(d2012) %>% merge(m2012)
colnames(bdm2012)[1:3] <- c("race", "sex", "year")
bdm2012 <- filter(bdm2012, race != 9)
names(bdm2012) <- tolower(names(bdm2012))
names(p2012) <- tolower(names(p2012))
pop <- filter(p2012, race %in% c(0:6))

### Create new rows with Hispanic and non-Hispanic in the race column to match bdm2014
for (i in 2012:2060) {
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
pop <- filter(pop, origin == 0) # Filter for only Hispanic/Non-Hispanic terms
pop$origin <- NULL # Remove origin column (now deprecated)

### Merging to get the final table and renaming columns
final2012 <- merge(bdm2012, pop)
colnames(final2012)[c(5,92,179)] <- c("deaths","migration","total population")

saveRDS(final2012, "dat2012.rds")


########## 2008 DATA PROCESSING ##########
### Read 2008 data files
p2008 <- read.csv('NP2008_D1.csv', header=TRUE)
b2008 <- read.csv('NP2008_D2.csv', header=TRUE)
d2008 <- read.csv('NP2008_D3.csv', header=TRUE)
m2008 <- read.csv('NP2008_D4.csv', header=TRUE)

### Merging datasets, removing extra rows, renaming columns for easier processing
bdm2008 <- b2008 %>% merge(d2008) %>% merge(m2008)
names(bdm2008) <- tolower(names(bdm2008))
names(p2008) <- tolower(names(p2008))
bdm2008 <- filter(bdm2008, race %in% c(0:6))
pop <- filter(p2008, race %in% c(0:6))
colnames(bdm2008)[1] <- "origin"
colnames(pop)[1] <- "origin"

### Create new rows with Hispanic and non-Hispanic in the race column to match bdm2014
for (i in 2000:2050) {
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
pop <- filter(pop, origin == 0) # Filter for only Hispanic/Non-Hispanic terms

### Merging to get the final table and renaming columns
final2008 <- merge(bdm2008, pop)
final2008$origin <- NULL # Remove origin column (now deprecated)
colnames(final2012)[c(5,6,7)] <- c("deaths","migration","total population")

saveRDS(final2008, "dat2008.rds")


########## STATE DATA PROCESSING ##########
### Read state data and rename column names
spop <- read.table('sppop.txt', header = TRUE, stringsAsFactors = FALSE)
colnames(spop) <- c("states","y1995","y2000","y2005","y2015","y2025")

### Removing . and , from data
for (i in 1:length(spop$states)) {
  spop$states[i] <- gsub("\\." , " ", spop$states[i])
  spop$y1995[i] <- gsub("," , "", spop$y1995[i])
  spop$y2000[i] <- gsub("," , "", spop$y2000[i])
  spop$y2005[i] <- gsub("," , "", spop$y2005[i])
  spop$y2015[i] <- gsub("," , "", spop$y2015[i])
  spop$y2025[i] <- gsub("," , "", spop$y2025[i])
}
### Changing data from thousands to absolute numbers
spop[, c(2:6)] <- sapply(spop[, c(2:6)], as.numeric)
spop[, c(2:6)] <- spop[, c(2:6)] * 1000

saveRDS(spop, "spop.rds")
