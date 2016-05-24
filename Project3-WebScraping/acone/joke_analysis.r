setwd('/Users/adamcone/Desktop/projects/Web Scraping')
library('dplyr')
library('ggplot2')

joke_df = read.csv('joke_data.txt',
                   header = TRUE,
                   sep = '\t')

for (i in 1:nrow(joke_df)) {
  if (joke_df$Worthwhile[i] == 0) {
    joke_df$Worthwhile[i] = 'no'
  } else {
    joke_df$Worthwhile[i] = 'yes'
  }
}
rm(i)

joke_df$Worthwhile = as.factor(joke_df$Worthwhile)

summary(joke_df)

# Time to start some EDA.

# ------------------------------------------------------

# What's the relationship between word count and worthwhileness?

# I'll plot a histogram of the data

ggplot(data = joke_df,
       mapping = aes(x = Word_Count)
       ) +
  theme_bw() +
  geom_histogram(binwidth = 15,
            color = 'white',
            boundary = 0
            ) +
  scale_x_continuous(breaks = seq(from = 0,
                                 to = 210,
                                 length.out = 15)
                    ) +
  labs(title = 'Joke Word Count Histogram',
       x = 'Joke Word Count',
       y = 'Joke Frequency')

# I'll look at the medians:

word_count_results = group_by(joke_df, Worthwhile) %>%
                     summarise(., median(Word_Count))

#Median for no: 16; for yes: 18. Not much a difference.

# Next, I'll check-out the box plot:

ggplot(data = joke_df,
       mapping = aes(x = Worthwhile,
                     y = Word_Count)
       ) +
  geom_boxplot() +
  theme_bw() +
  labs(title ="Joke Word Count vs. Worthwhileness Box Plot",
       x = "Worth Inviting in My Head?",
       y = "Word Count")

# Now, I'd like to run a 2-sample Welch's t-test. I'll check the assumptions:

# Independent, simple random sample. Almost certainly not. It seems to me that
# this selection was curated by one Jason Donner. The natural assumption to me
# is that he took the jokes he heard that he liked, and didn't post the ones he
# didn't. This is not random at all: it's purposeful. Now, I could interpret
# the population as the set of all jokes that Jason Donner would see fit to put on
# slightly-warped.com, and that the random sampling came from which ones he was
# exposed to on this crazy journey through life. In that case, the two-sample
# Welch's t-test would test whether the sub-populations of jokes I would find
# worthwhile and not worthwhile in the overall population of jokes that Jason Donner
# would, if he were exposed to them, post online. OK, so I think I found an
# interpretation that makes Independent, simple, random sampling OK.

# Now, in addition my sample size from each sub population is 87 for 'yes', 300
# 300 for 'no'. Since samlple sizes of at least N = 30 are accepted as
# guaranteeing normality of the mean sampling distribution, the Normality
# assumption is probably OK. However, to make doubly sure, I will check
# the qq plot of each of the sub-populations:

yes_wc_vec = joke_df$Word_Count[joke_df$Worthwhile == 'yes']
no_wc_vec = joke_df$Word_Count[joke_df$Worthwhile == 'no']

qqnorm(yes_wc_vec,
       main = 'Normal Q-Q Plot for Word Count of Worthwhile Sample',
       ylab = 'Word Count Sample Quantiles')
qqline(yes_wc_vec)

# I see a deviation for larger word counts, but considering only maybe 5
# points have large disparities and my sample size is 87, I think this is OK.

qqnorm(no_wc_vec,
       main = 'Normal Q-Q Plot for Word Count of Not Worthwhile Sample',
       ylab = 'Word Count Sample Quantiles')
qqline(no_wc_vec)

# Again, I see a deviation for larger word counts, but considering only maybe 15
# points have large disparities and my sample size is 300, I think this is OK.

# So, now that I have dealt with the Independence and Normality assumptions, I
# I can run the Welch's t-test.

# H_0: the sub-populations of jokes I would find worthwhile and not worthwhile
# from the total population of jokes that Jason Donner would be independently
# and randomly exposed to and deem fit for his website have equal mean joke word
# counts.

t.test(yes_wc_vec, no_wc_vec)

# Welch Two Sample t-test
# 
# data:  yes_wc_vec and no_wc_vec
# t = 1.7931, df = 95.471, p-value = 0.07612
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.6108539 12.0186700
# sample estimates:
#   mean of x mean of y 
# 23.85057  18.14667

# So, with a p-value of ~7.6%, I fail to reject H_0.

# In conclusion, I looked for evidence that joke length, as measured by word
# count, was a useful predictor of whether I found the joke worthwhile. I didn't
# find much.

# ------------------------------------------------------

# Now I'll repeat the same procedure for the number of curse words. I used a
# dictionary of 41 curse words.

# What's the relationship between curse words and worthwhileness?

# I'll plot a histogram of the data

ggplot(data = joke_df,
       mapping = aes(x = Curse_Count)
       ) +
  theme_bw() +
  geom_histogram(bins = 9,
                 color = 'white'
                 ) +
  scale_x_continuous(breaks = seq(from = 0,
                                  to = 8,
                                  length.out = 9),
                     labels = c('0',
                                '1',
                                '2',
                                '3',
                                '4',
                                '5',
                                '6',
                                '7',
                                '8')
                     ) +
  labs(title = 'Joke Curse Word Count Histogram',
       x = 'Joke Curse Word Count',
       y = 'Joke Frequency')

curse_count_overall = group_by(joke_df, Curse_Count) %>%
  summarise(., n())

# The vast majority of jokes had no curse words: 310/387 or 80%.

# I'll look at the medians:

curse_count_results = group_by(joke_df, Worthwhile) %>%
  summarise(., median(Curse_Count))

#Median for no: 0; for yes: 0. No apparent difference.

# Next, I'll check-out the violin plot:

ggplot(data = joke_df,
       mapping = aes(x = Worthwhile,
                     y = Curse_Count
                     )
       ) +
  geom_violin() +
  theme_bw() +
  labs(title ="Joke Curse Word Count vs. Worthwhileness Violin Plot",
       x = "Worth Inviting in My Head?",
       y = "Curse Word Count")

# This is interesting. It seems from the violin plot that the jokes
# I found worthwhile had more curse words. I seemed to like all three
# of the outliers with 3, 4, and 8 curse words.

# Now, I'll run a 2-sample Welch's t-test. Since the sub-populations of 'yes'
# and 'no' are identical, I don't need to check Independence, but I do need
# to check Normality.

# Now, in addition my sample size from each sub population is 87 for 'yes', 300
# 300 for 'no'. Since samlple sizes of at least N = 30 are accepted as
# guaranteeing normality of the mean sampling distribution, the Normality
# assumption is probably OK. However, to make doubly sure, I will check
# the qq plot of each of the sub-populations:

yes_cwc_vec = joke_df$Curse_Count[joke_df$Worthwhile == 'yes']
no_cwc_vec = joke_df$Curse_Count[joke_df$Worthwhile == 'no']

qqnorm(yes_cwc_vec,
       main = 'Normal Q-Q Plot for Curse Word Count of Worthwhile Sample',
       ylab = 'Curse Word Count Sample Quantiles')
qqline(yes_cwc_vec)

# Again, there is a deviation for larger counts, and there are discrepancies
# based on the discrete, small, numbers. However, I think this is OK, given
# that N = 87.

qqnorm(no_cwc_vec,
       main = 'Normal Q-Q Plot for Curse Word Count of Not Worthwhile Sample',
       ylab = 'Curse Word Count Sample Quantiles')
qqline(no_cwc_vec)

# Now the deviation looks significant for all Curse Counts above 0. However,
# 300 >> 30 for the purposes of sample mean normality, so I'm going to go
# ahead anyway.

# So, now that I have dealt with the Independence and Normality assumptions, I
# I can run the Welch's t-test.

# H_0: the sub-populations of jokes I would find worthwhile and not worthwhile
# from the total population of jokes that Jason Donner would be independently
# and randomly exposed to and deem fit for his website have equal mean joke curse
# word counts.

t.test(yes_cwc_vec, no_cwc_vec)

# Welch Two Sample t-test
# 
# data:  yes_cwc_vec and no_cwc_vec
# t = 2.0757, df = 93.49, p-value = 0.04066
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.01070923 0.48285399
# sample estimates:
#   mean of x mean of y 
# 0.4367816 0.1900000 

# So, with a p-value of ~4.1%, I reject H_0, barely.

# I'm interested in trying a chi^2 test. For this, I'll categorize the
# observations differently. I'll have jokes that have no curse words (310)
# and everything else (77). To run the chi^2 test, I'll generate the
# appropriate matrix.

cs_curse_matrix = matrix(rep(0,4), 2, 2)
rownames(cs_curse_matrix) = c('no curse', 'curse')
colnames(cs_curse_matrix) = c('no', 'yes')

cs_curse_matrix['no curse', 'no'] = sum(no_cwc_vec == 0)
cs_curse_matrix['no curse', 'yes'] = sum(yes_cwc_vec == 0)
cs_curse_matrix['curse', 'no'] = sum(no_cwc_vec > 0)
cs_curse_matrix['curse', 'yes'] = sum(yes_cwc_vec > 0)

# Now I can run the test. H_0: the variables are independent. 

chisq.test(cs_curse_matrix)
mosaicplot(cs_curse_matrix,
           shade = TRUE,
           main = "Mosaic Plot of Curse and Worthwhile Variables"
           )

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  cs_curse_matrix
# X-squared = 2.5059, df = 1, p-value = 0.1134

# I looked for evidence of a relationship betweeen curse words and whether I
# found the joke worthwhile. I found weak evidence that the populations of
# worthwhile and not worthwhile jokes have different means. I failed to
# reject the null-hypothesis of independence of these variables, when I
# used a binary categorization of the jokes based on curses.

# Overall, I didn't find compelling evidence of a relationship between curse
# words and my experience of the jokes as worthwhile or not.

# ------------------------------------------------------

# Now I'll repeat the same procedure for the number of exclamation points.

# What's the relationship between exclamation points and worthwhileness?

# I'll plot a histogram of the data

ggplot(data = joke_df,
       mapping = aes(x = Exclamation_Count)
       ) +
  theme_bw() +
  geom_histogram(bins = 6,
                 color = 'white'
                 ) +
  scale_x_continuous(breaks = seq(from = 0,
                                  to = 5,
                                  length.out = 6)
                     ) +
  labs(title = 'Joke Exclamation Point Count Histogram',
       x = 'Joke Exclamation Point Count',
       y = 'Joke Frequency')

exclamation_count_overall = group_by(joke_df, Exclamation_Count) %>%
                            summarise(., n())

# The vast majority of jokes had no curse words: 349/387 or ~90%.

# I'll look at the medians:

curse_count_results = group_by(joke_df, Worthwhile) %>%
                      summarise(., median(Exclamation_Count))

#Median for no: 0; for yes: 0. No apparent difference.

# Next, I'll check-out the violin plot (too much overplotting for
# straight box plot):

ggplot(data = joke_df,
       mapping = aes(x = Worthwhile,
                     y = Exclamation_Count
                     )
       ) +
  geom_violin() +
  theme_bw() +
  labs(title ="Joke Exclamation Point Count vs. Worthwhileness Violin Plot",
       x = "Worth Inviting in My Head?",
       y = "Exclamation Point Count")

# I see virtually no difference here at all. Maybe something will show
# up in future tests, but it looks like nothin' here.

# Now, I'll run a 2-sample Welch's t-test. Since the sub-populations of 'yes'
# and 'no' are identical, I don't need to check Independence, but I do need
# to check Normality.

# Now, in addition my sample size from each sub population is 87 for 'yes', 300
# 300 for 'no'. Since samlple sizes of at least N = 30 are accepted as
# guaranteeing normality of the mean sampling distribution, the Normality
# assumption is probably OK. However, to make doubly sure, I will check
# the qq plot of each of the sub-populations:

yes_ep_vec = joke_df$Exclamation_Count[joke_df$Worthwhile == 'yes']
no_ep_vec = joke_df$Exclamation_Count[joke_df$Worthwhile == 'no']

qqnorm(yes_ep_vec,
       main = 'Normal Q-Q Plot for Exclamation Point Count of Worthwhile Sample',
       ylab = 'Exclamation Point Count Sample Quantiles')
qqline(yes_ep_vec)

# Again, there is a deviation for larger counts, and there are discrepancies
# based on the discrete, small, numbers. However, I think this is OK, given
# that N = 87.

qqnorm(no_ep_vec,
       main = 'Normal Q-Q Plot for Exclamation Point Count of Not Worthwhile Sample',
       ylab = 'Exclamation Point Count Sample Quantiles')
qqline(no_ep_vec)

# 300 >> 30 for the purposes of sample mean normality, so I'm going to go
# ahead.

# So, now that I have dealt with the Independence and Normality assumptions, I
# I can run the Welch's t-test.

# H_0: the sub-populations of jokes I would find worthwhile and not worthwhile
# from the total population of jokes that Jason Donner would be independently
# and randomly exposed to and deem fit for his website have equal mean joke
# exclamation point counts.

t.test(yes_ep_vec, no_ep_vec)

# Welch Two Sample t-test
# 
# data:  yes_ep_vec and no_ep_vec
# t = 2.3099, df = 96.424, p-value = 0.02303
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.02662164 0.35176916
# sample estimates:
#   mean of x  mean of y 
# 0.27586207 0.08666667  

# So, with a p-value of ~2.3%, I reject H_0. Fascinating. This seems to me
# like a significant relationship between the number of exclamation points
# in a joke and whether I found the joke worthwhile. I did not anticipate this.

# I'm interested in trying a chi^2 test as well. For this, I'll categorize the
# observations differently. I'll have jokes that have no exclamation points
# (349) and everything else (38). To run the chi^2 test, I'll generate the
# appropriate matrix.

cs_ep_matrix = matrix(rep(0,4), 2, 2)
rownames(cs_ep_matrix) = c('no exclamation', 'exclamation')
colnames(cs_ep_matrix) = c('no', 'yes')

cs_ep_matrix['no exclamation', 'no'] = sum(no_ep_vec == 0)
cs_ep_matrix['no exclamation', 'yes'] = sum(yes_ep_vec == 0)
cs_ep_matrix['exclamation', 'no'] = sum(no_ep_vec > 0)
cs_ep_matrix['exclamation', 'yes'] = sum(yes_ep_vec > 0)

# Now I can run the test. H_0: the variables are independent. 

chisq.test(cs_ep_matrix)
mosaicplot(cs_ep_matrix,
           shade = TRUE,
           main = "Mosaic Plot of Exclamation and Worthwhile Variables"
           )

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  cs_ep_matrix
# X-squared = 8.1054, df = 1, p-value = 0.004413

# Good grief: p-value of ~0.04%. This is strong evidence that these variables
# are not independent. In particular, looking at the mosaic plot, I see
# that I liked a disproportionately large number of jokes that had at least
# one exclamation point.

# I looked for evidence of a relationship betweeen exclamation points and
# whether I found the joke worthwhile. I found significant evidence that
# the populations of worthwhile and not worthwhile jokes did have different
# means, and strong evidence that the worthwhileness of a joke and whether it
# has at least one exclamation point are not independent variables.

# Overall, I found compelling evidence of a relationship between exclamation
# points and my experience of the jokes as worthwhile or not.