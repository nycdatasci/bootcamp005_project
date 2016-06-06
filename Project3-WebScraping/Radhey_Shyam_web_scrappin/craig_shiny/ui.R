# ui.R
library(shiny)
library(ggplot2) # for the diamonds dataset 

shinyUI(fluidPage(
titlePanel('New York Craiglist Car Listing'),
sidebarLayout(
  sidebarPanel('Select Fields',width=1),
  # selectInput("state", # choose the states
  #             label = "Choose a state to display", 
  #             choices = loan$state.full,selected = "South Carolina",width = 200),
  #selectInput("year", # choose the states
   #           label = "Choose a year", 
   #           choices = year_list,selected =year_list[5]),
 # sliderInput("range", # choose the range 
   #           label = "Borrower's Annual Income:",
   #           min = 10000, max = 500000, value = c(40000, 80000),step=1000),
  # radioButtons("radio", label=h3("Bar charts"), choices=col_list[3], selected = col_list[3]),
  # checkboxGroupInput("show_vars", "Columns in home loan to show:", 
  #                 col_list, selected = col_list[3]) ),
  # 
 mainPanel(
    tabsetPanel( 
  tabPanel('Price Histogram', plotOutput("price")),
  tabPanel('Weekdays Bar', plotOutput("week")),
  tabPanel('Ad Posting time', plotOutput("time_posting")),
  tabPanel("Violin Plot",plotOutput("week_time_violin")),
  tabPanel('Word Cloud', plotOutput("word_cloud")),
  tabPanel('Year Cloud', plotOutput("year_cloud")),
  tabPanel('Data',dataTableOutput("data")) 

)
)
)
))
