library(dplyr)
library(ggplot2)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(ggthemes)

#Read in data
#Data taken from https://www.kaggle.com/snap/amazon-fine-food-reviews
reviews = read.csv("Reviews.csv", stringsAsFactors = F)

######Distribution of Scores###########
######################################

numreviews = group_by(reviews, Score) %>% summarise(pct = n()/nrow(reviews))
ScoreDist = ggplot(numreviews, aes(x = Score, y = pct*100))
ScoreDist + geom_bar(aes(fill = factor(Score)), stat = "identity") + 
  theme_minimal() + xlab("Rating (Stars)") + ylab("Percent of reviews") +
  ggtitle("Distribution of Ratings") + guides(fill = F) + 
  scale_fill_brewer(palette = "Pastel1")

###########Helpfulness############
################################

#Add a factor on whether or not the review is helpful.
# Here we define helpful/not helpful by the percent of people finding it
#helpful being >75%/<25%.
#Results did not seem to vary signficantly for different choices of these constants.
HELPFULMAX = .75
HELPFULMIN = .25
helpfulness = function(HelpfulnessNumerator, HelpfulnessDenominator) {
  if (HelpfulnessDenominator == 0) return("NA")
  if (HelpfulnessNumerator / HelpfulnessDenominator > HELPFULMAX) {
    return("Helpful")
  } else if (HelpfulnessNumerator / HelpfulnessDenominator < HELPFULMIN) {
    return("Not helpful")
  } else
  {
    return("Neither")
  }
}

#Add the factor helpfulness
reviews_helpful = mutate(reviews,
                    helpful = factor(apply(reviews, 1, 
                                 function(x) helpfulness(as.numeric(x["HelpfulnessNumerator"]), 
                                                          as.numeric(x["HelpfulnessDenominator"])))))
reviews_helpful$helpful = relevel(reviews_helpful$helpful, "NA")

#Distribution of helpfulness
numhelpful = group_by(reviews_helpful, helpful) %>% 
            summarise(pct = n()/nrow(reviews_helpful))

ggplot(numhelpful, aes(x = helpful, y = pct*100)) +
  geom_bar(aes(fill = helpful), stat = "identity") + 
  theme_minimal() + xlab("Helpfulness") + ylab("Percent of reviews") +
  ggtitle("Distribution of Helpfulness") + guides(fill = F) + 
  scale_fill_brewer(palette = "Pastel1") + 
  scale_x_discrete(labels=c("NA" = "No indication", "Helpful" = "> 75%", "Neither" = "25 - 75%",
                           "Not helpful" = "< 25%"))

#Distribution of scores among reviews that were voted upon.
helpfulornot = filter(reviews_helpful, helpful != "NA")

helpful_scoresand = group_by(helpfulornot, Score, helpful)
helpful_scores = helpful_scoresand %>% summarise(num = n())
pcthelpful_score = left_join(helpful_scores, 
                             summarise(helpful_scores, numrating = sum(num)), by = "Score")

pcthelpful_score = mutate(pcthelpful_score, pct = num/numrating)

ggplot(filter(pcthelpful_score, helpful != "Neither"), aes(x = Score, y = pct*100, fill = helpful)) +
  geom_bar(aes(fill = helpful), stat = "identity", position = 'dodge') + 
  theme_minimal() + xlab("Rating (Stars)") + ylab("Percent") +
  ggtitle("Percent of Reviews Found Helpful/Not Helpful\nAmong Voted on Reviews by Rating ") + 
  scale_fill_brewer(palette = "Pastel1", name = "Helpfulness", labels = c("Helpful (> 75%)", "Not helpful (< 25%)")) 

####################Word count#####################
##################################################

#Add variables for word count and character count of reviews.
reviewsLengths = mutate(reviews, numChar = nchar(reviews$Text))
reviewsLengths$numWord = sapply(strsplit(reviews$Text, " "), length)

#Box plot of word count by ratings
#Analogous box plot for character count gave similar results.
ggplot(reviewsLengths, aes(x = factor(Score), y = numWord)) + 
  geom_boxplot(outlier.shape = NA, aes(fill = factor(Score))) + 
  scale_y_continuous(limits = c(0,225)) + theme_minimal() + 
    xlab("Rating (Stars)") + ylab("Number of words in review") +
    ggtitle("Word Count by Rating") + 
    scale_fill_brewer(palette = "Pastel1") +
  guides(fill = F)

####Word Count Helpfulness
#Adding the word length and character length variables to the dataframe the has the
#helpfulness.
reviewsHelpfulLengths = mutate(reviews_helpful, numChar = nchar(reviews_helpful$Text))
reviewsHelpfulLengths$numWord = sapply(strsplit(reviewsHelpfulLengths$Text, " "), length)

#Word count by helpfulness of review (again character count gave similiar results.
ggplot(filter(reviewsHelpfulLengths, helpful == "Helpful" | helpful == "Not helpful"), aes(x = helpful, y = numWord)) + 
  geom_violin(aes(fill = helpful), draw_quantiles = c(.5)) + 
  scale_y_continuous(limits = c(0,200)) + theme_minimal() + 
  xlab("Helpfulness") + ylab("Number of words in review") +
  ggtitle("Word Count by Helpfulness") + 
  scale_fill_brewer(palette = "Pastel1") +
  scale_x_discrete(labels=c("Helpful" = "Helpful\n(> 75% helpful)",
                            "Not helpful" = "Not helpful\n(< 25% helpful)")) +
  guides(fill = F)

##################Frequency of reviewer#####################
#############################################################

#Add a column for the number of reviews for a given user
freq = group_by(reviews, UserId) %>% summarise(userfreq = n())
reviewsHelpfulLengthsFreq = left_join(reviewsHelpfulLengths, freq, 
                                     by = "UserId")
#Add on a column for whether or not the reviewer is a freqent reviewer----defined
#by having more than 50 reviews.
reviewsHelpfulLengthsFreq = mutate(reviewsHelpfulLengthsFreq,
                              freq = (reviewsHelpfulLengthsFreq$userfreq > 50))
#5% of reviews come from reviews who have reviewed 50 or more products

######Calculate the score distribution for frequent and nonfrequent reviewers
#Number of reviews by frequency and score
new = group_by(reviewsHelpfulLengthsFreq, Score, freq) %>% summarise(count = n())

#freqscore$tot is the total number of reviews for frequent and non frequent reviewers.
#freqscore$pctbyfreq is the score distrubution
freqscore = left_join(new, group_by(new, freq) %>% summarise(tot = sum(count)), by = "freq")
freqscore$pctbyfreq = freqscore$count/freqscore$tot

#Plot of distribution of scores for frequent/non-frequent reviewers.
ggplot(freqscore, aes(x = Score, y = pctbyfreq*100, fill = freq)) + 
  geom_bar(aes(fill = freq), stat = "identity", position = 'dodge') + 
  theme_minimal() + xlab("Rating (Stars)") + ylab("Percent of reviews") +
  ggtitle("Distribution of Ratings") + 
  scale_fill_brewer(palette = "Pastel1", name = "Frequency of reviewer", 
                    labels = c("Not frequent (1-50 reviews)", "Frequent (> 50 reviews)"))


########Word count for frequent/non-frequent reviewers
ggplot(reviewsHelpfulLengthsFreq, aes(x = freq, y = numWord)) + 
  geom_violin(aes(fill = freq), draw_quantiles = c(.5)) + 
  scale_y_continuous(limits = c(0,350)) + theme_minimal() + 
  xlab("Frequency of reviewer") + ylab("Number of words in review") +
  ggtitle("Word Count by Reviewer Frequency") + 
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = F) +
  scale_x_discrete(labels = c("Not frequent reviewer\n(1-50 reviews)", "Frequent reviewer\n(> 50 reviews)"))


#######Helpfulness and user frequency

#Calculuate the total number of helpful/not helpful reviews for frequent/non-frequent useres
helpfulfreq = left_join(reviewsHelpfulLengthsFreq %>% 
              group_by(freq, helpful) %>% summarise(count = n()),
          reviewsHelpfulLengthsFreq %>% 
              group_by(freq) %>% summarise(tot = n()),
          by = 'freq')

#Calculates percent of helpful/non-helpful reviews for frequent/non-frequent reviewers
helpfulfreq$pct = helpfulfreq$count/helpfulfreq$tot

ggplot(helpfulfreq, aes(x = helpful, y = pct*100, fill = freq)) + 
  geom_bar(aes(fill = freq), stat = "identity", position = 'dodge') + 
  theme_minimal() + xlab("Helpfulness") + ylab("Percent of reviews") +
  ggtitle("Helpfulness by Reviewer Frequency") + 
  scale_fill_brewer(palette = "Pastel1", name = "Frequency\nof reviewer", labels = c("Not frequent reviewer\n(1-50 reviews)\n", "Frequent reviewer\n(> 50 reviews)\n")) +
  scale_x_discrete(labels=c("NA" = "No\nindication", "Helpful" = "> 75%", "Neither" = "25 - 75%",
                          "Not helpful" = "< 25%"))

#######################################Word clouds####################

#Function to make a word cloud. Can specify the dataframe, the filename of the resulting
# wordcloud, the colorschee, and additional words you don't want to appear (common
# english stopwords are already removed.) This code is adapted from:
#http://www.r-bloggers.com/word-cloud-in-r/
makewordcloud <- function(data, column, filename, colorscheme = "BuGn", extraRemove = NULL)
{
  data.corpus <- Corpus(DataframeSource(data.frame(data[[column]])))
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  data.corpus <- tm_map(data.corpus, content_transformer(function(x) removeWords(x, c(stopwords("english"), extraRemove))))
  tdm <- TermDocumentMatrix(data.corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  pal <- brewer.pal(9, colorscheme)
  pal <- pal[-(1:2)]
  png(filename, width=1280,height=800)
  wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
  dev.off()
}

#Positive/negative reviews
positive = filter(reviews, Score > 3)
negative = filter(reviews, Score < 3)

#Sample 35000 rows from positive and negative reviews. The wordcloud function
# cannot handle the entire postive and negative dataframes.
positivesample = positive[sample(nrow(positive), 35000),]
negativesample = negative[sample(nrow(negative), 35000),]

#Make wordclouds.
makewordcloud(data = positivesample, column = 'Text', 
              filename = 'positiveSampleText.png', colorscheme = 'Greens')
makewordcloud(data = negativesample, column = 'Text', 
              filename = 'negativeSampleText.png', colorscheme = 'OrRd')
