load("~/Desktop/Visualization Project/analysis/opdata.RData")
library(dplyr)
library(chron)
library(ggplot2)

# How did the stores total revenue change over time?
# To answer this question, write a function that generates bin
# numbers for a vector of dates and number of bins:

bin_numbers = function(dates, bins) {
  stopifnot(class(dates) == c("chron", "dates", "times"),
            class(bins) == "numeric",
            round(bins) == bins
            )
  bin_dur = (max(dates, na.rm = TRUE) - min(dates, na.rm = TRUE))/bins
  bin_num = ceiling((dates - min(dates, na.rm = TRUE))/bin_dur)
  bin_num[1] = 1
  return(bin_num)
}

# now I'll try out this function on the unfiltered data
#------------------------------------------------------
sales_bar = mutate(sales_u, Bin_Number = bin_numbers(DateTime, 13)) %>%
            group_by(., Bin_Number) %>%
            summarise(., Bin_Sale_Total = sum(Sale_Total))

# now, I'll plot the result
ggplot(data = sales_bar, aes(x = Bin_Number)) +
  geom_bar(aes(weight = Bin_Sale_Total)) +
  theme_bw() +
  labs(title = "Revenue vs. Time (original data)",
       x = "Time (6-month bins)",
       y = "Revenue ($)"
       ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
    ) +
  scale_y_continuous(limits = c(0, 7e5))
#------------------------------------------------------

# now I'll try out this function on the filtered data
#------------------------------------------------------
sales_bar = mutate(sales_f, Bin_Number = bin_numbers(DateTime, 13)) %>%
            group_by(., Bin_Number) %>%
            summarise(., Bin_Sale_Total = sum(Sale_Total))

# now, I'll plot the result
ggplot(data = sales_bar, aes(x = Bin_Number)) +
  geom_bar(aes(weight = Bin_Sale_Total)) +
  theme_bw() +
  labs(title = "Revenue vs. Time (clean data)",
       x = "Time (6-month bins)",
       y = "Revenue ($)"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 7e5))