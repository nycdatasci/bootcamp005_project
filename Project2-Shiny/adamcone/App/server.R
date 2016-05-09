library(shiny)
library(dplyr)
library(ggplot2)
source("./helpers.R")
load("./shinydataframe.RData")

server <- function(input, output) {
  data <- eventReactive(input$go, {items_filter(items_tbl,
                                                input$Price_Range_selected,
                                                input$Payment_Types_selected,
                                                input$Days_of_Week_selected,
                                                input$Times_of_Day_selected
  )}
  )
  
  output$Revenue_Plot <-
    renderPlot({
      ggplot(data = data(),
             mapping = aes(Date_Bin,
                           Revenue_Total
             )
      ) +
        theme_bw() +
        ggtitle("Open Produce Revenue vs. Time") +
        geom_bar(stat = "identity",
                 width = 1,
                 color = "white") +
        scale_x_continuous(name = "\nDate",
                           breaks = seq(from = 0.5,
                                        to = 20.5,
                                        length.out = 8),
                           labels = c("Jul '09",
                                      "Jul '10",
                                      "Jun '11",
                                      "May '12",
                                      "Apr '13",
                                      "Mar '14",
                                      "Feb '15",
                                      "Jan '16")
        ) +
        scale_y_continuous(name = "Revenue ($)")
    })
}