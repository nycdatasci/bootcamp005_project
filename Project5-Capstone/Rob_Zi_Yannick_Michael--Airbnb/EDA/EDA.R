library(ggplot2)
library(dplyr)
library(RColorBrewer)

df = read.csv('train_starting.csv')
df$age = factor(df$age, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+"))

browser = df %>% group_by(first_browser) %>% summarise(pct = n()/nrow(df))

ggplot(browser, aes(reorder(x = first_browser, -pct), y = pct)) + 
  geom_bar(stat = 'identity', aes(fill = colorRampPalette(brewer.pal(9, "Set1"))(52))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank()) + guides(fill = F) +  
  ggtitle("Distribution of First Browser") + xlab("Browser") + ylab("Percent of users") 

ggplot(df, aes(x = gender)) + geom_bar(aes(y = (..count..)/sum(..count..), fill = gender)) + 
  scale_colour_brewer("Set1") + theme_minimal() +
  xlab("Gender") + ylab("Percent of users") + ggtitle("Distribution of genders")

country_dist = df %>% group_by(country_destination) %>% summarise(pct = n()/nrow(df))
ggplot(country_dist, aes(reorder(x = country_destination, -pct), y = pct)) + 
  geom_bar(stat = 'identity', aes(fill = country_destination)) + theme_minimal() + guides(fill = F) +  
  ggtitle("Distribution of country of first destination") + xlab("Country") + ylab("Percent of users") 
  
ggplot(df, aes(x = country_destination)) + geom_bar(aes(fill = gender), position = "fill") +
  xlab("Country") + ylab("Percent") + ggtitle("Share of gender by country of first destination") +
  theme_minimal()

ggplot(df, aes(x = age)) + geom_bar(aes(y = (..count..)/sum(..count..), fill = factor(1))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank())+ 
  guides(fill = F) +
  xlab("Age") + ylab("Percent of users") + ggtitle("Age distributation of users")

ggplot(df, aes(x = age)) + geom_bar(aes(fill = gender), position = "fill") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.ticks = element_blank()) +
  xlab("Age") + ylab("Percent") + ggtitle("Share of gender by age")

ggplot(df, aes(x = country_destination)) + 
  geom_bar(aes(fill = age), position = "fill") + 
  scale_fill_manual(values = rev(colorRampPalette(brewer.pal(9, "RdBu"))(21)), na.value = "grey") +
  theme_minimal() + xlab("Country") + ylab("Percent") + ggtitle("Share of age by country\nof first destination")


ggplot(df, aes(x = bookings)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = bookings)) + scale_color_brewer(palette = "Set1") +
  xlab("When the user booked") + ylab("Percent of users") + guides(fill = F) +
  ggtitle("Distribution of time of booking") + theme_minimal()

ggplot(df, aes(x = lag_account_created)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = lag_account_created)) + scale_color_brewer("Set1") +
  xlab("Difference between when account was\ncreated and date of first booking") + ylab("Percent of users") + guides(fill = F) +
  ggtitle("How much time does it take a user to book?") + theme_minimal()

ggplot(df, aes(x = lag_first_active)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = lag_first_active)) + scale_color_brewer("Set1") +
  xlab("Difference between user's first activity\nand date of first booking") + ylab("Percent of users") + guides(fill = F) +
  ggtitle("How much time does it take a user to book?") + theme_minimal()

ggplot(df, aes(x = lag_account_created_first_active)) + 
  geom_bar(aes(fill = bookings), position = "fill") + 
  scale_fill_brewer(palette = "Set1", name = "When user booked", labels = c("Early", "Did not book", "Waited")) +
  xlab("Difference between when account was\ncreated and date of first booking") +
  ylab("Percent") + ggtitle("How long do users wait to book?") + theme_minimal()

ggplot(df, aes(x = country_destination)) + 
  geom_bar(aes(fill = bookings), position = "fill") +
  scale_fill_brewer(palette = "Set1", name = "When user booked", labels = c("Early", "Did not book", "Waited")) +
  xlab("Country") + ylab("Percent") + ggtitle("How long users wait to book,\nby country of first destination") +
  theme_minimal()

# ggplot(df, aes(x = counts)) + geom_density(aes(fill = (country_destination == 'NDF'), alpha = .2)) + scale_x_log10()
# 
# ggplot(df, aes(x = sum_secs_elapsed)) + geom_density(aes(fill =(country_destination == 'NDF'), alpha = .2)) + 
#   scale_x_log10()
