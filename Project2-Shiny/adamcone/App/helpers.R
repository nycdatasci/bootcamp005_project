library("shiny")
library("dplyr")
library("ggplot2")

# I will create a function that inputs the parameters the user is prompted for,
# and main data frame items_tbl, and outputs a data frame that will be used for
# plotting the histogram.

items_filter = function(items_tbl,
                        Price_Range_selected,
                        Payment_Types_selected,
                        Days_of_Week_selected,
                        Times_of_Day_selected) {
  stopifnot(class(Price_Range_selected) == "integer",
            class(Payment_Types_selected) == "character",
            class(Days_of_Week_selected) == "character",
            class(Times_of_Day_selected) == "character",
            length(Price_Range_selected) == 2,
            length(Payment_Types_selected) %in% 1:5,
            length(Days_of_Week_selected) %in% 1:7,
            length(Times_of_Day_selected) %in% 1:3
            )
  plot_tbl = filter(items_tbl, Unit_Cost >= Price_Range_selected[1] &
                               Unit_Cost <= Price_Range_selected[2] &
                               Payment_Type %in% Payment_Types_selected &
                               Day_of_Week %in% Days_of_Week_selected &
                               Time_Bin %in% Times_of_Day_selected
                    ) %>%
             select(., Item_Total, Date_Bin) %>%
             group_by(., Date_Bin) %>%
             summarise(., Revenue_Total = sum(Item_Total))
             print(plot_tbl)
  return(plot_tbl)
}