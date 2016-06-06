library(dplyr)
library(corrplot)
library(tm)
library(SnowballC)
library(wordcloud)
library(plotly)

recipespd <- read.delim("recipespd.txt")
ingredientspd <- read.delim("ingredientspd.txt")

calsperyear = group_by(recipespd, year) %>% summarise(., median_cal_count = median(cal_count, na.rm= TRUE))
plot(calsperyear)

calsperyear = group_by(recipespd, year) %>% summarise(., median(cal_count, na.rm= TRUE))
plot(calsperyear)

plot(cbind(recipespd['year'],recipespd['review_count']))

head(sort(table(recipespd$recipe_title), decreasing = TRUE), n = 10)
recipespd[recipespd$recipe_title == 'clone of a cinnabon', "year"]

plot(cbind(recipespd['prep_time'],recipespd['cook_time']))
recipespd$recipe_title = tolower(recipespd$recipe_title)
 group_by(recipespd, year) %>% filter(., 'gluten-free' %in% recipe_title) %>% summarise(., n)
 
 
cake = recipespd[grepl('cake', recipespd$recipe_title),] %>% group_by(year) %>% summarize(., n = n())
cookie = recipespd[grepl('cookie', recipespd$recipe_title),] %>% group_by(year) %>% summarize(., n = n())
pie = recipespd[grepl('pie', recipespd$recipe_title),] %>% group_by(year) %>% summarize(., n = n())
chocolate = recipespd[grepl('chocolate', recipespd$recipe_title),] %>% group_by(year) %>% summarize(., n = n())
salad = recipespd[grepl('salad', recipespd$recipe_title),] %>% group_by(year) %>% summarize(., n = n())
plot(chicken)
recipespd[grepl('pumpkin bread', recipespd$recipe_title),] %>% group_by(year) %>% summarize(., n = n())

listwords = strsplit(names, ' ')
sort(table(listwords))
chicken = recipespd[grepl('chicken', recipespd$recipe_title),] %>% group_by(year) %>% summarize(., n = n())
salt = ingredientspd[grepl('salt', ingredientspd$ingredient),] %>% group_by(year) %>% summarize(., n = n())
plot(ingredientspd[grepl('olive', ingredientspd$ingredient),] %>% group_by(year) %>% summarize(., n = n()))
butter = ingredientspd[grepl('butter', ingredientspd$ingredient),] %>% group_by(year) %>% summarize(., n = n())
shorten = ingredientspd[grepl('shorten', ingredientspd$ingredient),] %>% group_by(year) %>% summarize(., n = n())
oliveoil =ingredientspd[grepl('olive oil', ingredientspd$ingredient),] %>% group_by(year) %>% summarize(., n = n())
egg = ingredientspd[grepl('egg', ingredientspd$ingredient),] %>% group_by(year) %>% summarize(., n = n())
flour = ingredientspd[grepl('flour', ingredientspd$ingredient),] %>% group_by(year) %>% summarize(., n = n())
sugar = ingredientspd[grepl('sugar', ingredientspd$ingredient),] %>% group_by(year) %>% summarize(., n = n())
plot(ingredientspd[grepl('white', ingredientspd$ingredient),] %>% group_by(year) %>% summarize(., n = n()))
#put fluor, butter, and eggs and shortening on one graph
#compare salt, butter, oil, egg, flour, sugar over time
#word cloud of recipes and also another word cloud of ingredients
#focus on site has changed from cookie exchange site to broadening of tastes
years = as.data.frame(seq(1997, 2015))
colnames(years) = 'year'
colnames(salt) = c('year', 'salt')
colnames(butter) = c('year', 'butter')
colnames(shorten) = c('year', 'shorten')
colnames(egg) = c('year', 'egg')
colnames(flour) = c('year', 'flour')
colnames(sugar) = c('year', 'sugar')
colnames(oliveoil) = c('year', 'olive_oil')

trends = left_join(years, salt, by = c('year' = 'year')) %>% full_join(., butter, by = c('year' = 'year')) %>%
    left_join(., shorten, by = c('year' = 'year')) %>% left_join(., egg, by = c('year' = 'year')) %>%
     left_join(., flour, by = c('year' = 'year')) %>%
    left_join(., sugar, by = c('year' = 'year')) %>% left_join(., oliveoil, by = c('year' = 'year'))

trendsbaking = full_join(years, butter, by = c('year' = 'year')) %>%
     left_join(., egg, by = c('year' = 'year')) %>%
    left_join(., flour, by = c('year' = 'year')) %>% left_join(., sugar, by = c('year' = 'year'))

baking = cbind(years, reshape2::melt(trendsbaking[,-1]))
plot_ly(data = baking, x = year, y = value, mode = "lines",
        color = variable, colors = "Set1")
plot_ly(data = oliveoil, x = year, y = olive_oil, mode = "lines",
       colors = "Set1")

colnames(calsperyear) = c('year', 'median_cal_count')
plot_ly(data = calsperyear, x = year, y = median_cal_count, mode = "lines", #size = 1,
        colors = "Set1")
plot_ly(data = recipespd, x = year, y = review_count, mode = "markers",
        colors = "Set1")
plot_ly(data = cookie, x = year, y = n, mode = "lines",
        colors = "Set1")
plot(recipespd$year, recipespd$review_count)

scatter <- gvisScatterChart(trends)
plot(scatter)
cordata = cor(na.omit(data))
corrplot(cordata, method = 'circle')
old = filter(recipespd, year<2007)
new = filter(recipespd, year>=2007)
oldpaste = paste(old$recipe_title, collapse = ' ')
newpaste = paste(new$recipe_title, collapse = ' ')
jeopCorpusold <- Corpus(VectorSource(oldpaste))
jeopCorpusold <- tm_map(jeopCorpusold, PlainTextDocument)
jeopCorpusold <- tm_map(jeopCorpusold, removePunctuation)
jeopCorpusold <- tm_map(jeopCorpusold, removeWords, stopwords('english'))
jeopCorpusold <- tm_map(jeopCorpusold, stemDocument)
wordcloud(jeopCorpusold, max.words = 100, random.order = FALSE)

#cookie exchange popular in late '90s
jeopCorpusnew <- Corpus(VectorSource(newpaste))
jeopCorpusnew <- tm_map(jeopCorpusnew, PlainTextDocument)
jeopCorpusnew <- tm_map(jeopCorpusnew, removePunctuation)
jeopCorpusnew <- tm_map(jeopCorpusnew, removeWords, stopwords('english'))
jeopCorpusnew <- tm_map(jeopCorpusnew, stemDocument)
wordcloud(jeopCorpusnew, max.words = 100, random.order = FALSE)


names = paste(recipespd$recipe_title, collapse = ' ')
ingrdiennames = paste(ingredientspd$ingredient, collapse = ' ')
jeopCorpus <- Corpus(VectorSource(names))
jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
jeopCorpus <- tm_map(jeopCorpus, removePunctuation)
jeopCorpus <- tm_map(jeopCorpus, removeWords, stopwords('english'))
jeopCorpus <- tm_map(jeopCorpus, stemDocument)
wordcloud(jeopCorpus, max.words = 100, random.order = FALSE)

jeopCorpus2 <- Corpus(VectorSource(ingrdiennames))
jeopCorpus2 <- tm_map(jeopCorpus2, PlainTextDocument)
jeopCorpus2 <- tm_map(jeopCorpus2, removePunctuation)
jeopCorpus2 <- tm_map(jeopCorpus2, removeWords, stopwords('english'))
jeopCorpus2 <- tm_map(jeopCorpus2, stemDocument)
wordcloud(jeopCorpus2, max.words = 100, random.order = FALSE)

plot(recipespd$cook_time, recipespd$cal_count)
plot(recipespd$star_rating, recipespd$review_count)
plot(recipespd$year, recipespd$review_count)
hist(recipespd$review_count)
