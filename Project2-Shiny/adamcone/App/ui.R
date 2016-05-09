library(shiny)
library(dplyr)


fluidPage(
  titlePanel("What Kind of Revenue Interests You?"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "Price_Range_selected",
                  label = h3("Product Unit Price Range ($)"),
                  min = 0, 
                  max = 100,
                  value = c(0, 100)
                  ),
      
      checkboxGroupInput(inputId = "Payment_Types_selected",
                         label = h3("Payment Type"), 
                         choices = list("Cash" = "Cash",
                                        "Check" = "Check",
                                        "Credit/Debit" = "Credit/Debit",
                                        "Food Stamps" = "Food Stamps",
                                        "Tab"),
                         selected = c("Cash",
                                      "Check",
                                      "Credit/Debit",
                                      "Food Stamps",
                                      "Tab")
                         ),
      
      checkboxGroupInput(inputId = "Days_of_Week_selected",
                         label = h3("Days of Week"), 
                         choices = list("Sunday" = "Sunday",
                                        "Monday" = "Monday",
                                        "Tuesday" = "Tuesday",
                                        "Wednesday" = "Wednesday",
                                        "Thursday" = "Thursday",
                                        "Friday" = "Friday",
                                        "Saturday" = "Saturday"),
                         selected = c("Sunday",
                                      "Monday",
                                      "Tuesday",
                                      "Wednesday",
                                      "Thursday",
                                      "Friday",
                                      "Saturday")
                         ),
      
      checkboxGroupInput(inputId = "Times_of_Day_selected",
                         label = h3("Time of Day"), 
                         choices = list("8:00am-2:00pm" = 1,
                                        "2:00pm-8:00pm" = 2,
                                        "8:00pm-2:00am" = 3
                                        ),
                         selected = c(1,
                                      2,
                                      3)
                         ),
      actionButton("go", "Plot Revenue")
      ),
    mainPanel(plotOutput("Revenue_Plot")
              )
  )
)