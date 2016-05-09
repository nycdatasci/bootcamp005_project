# ui.R
library(shiny)
library(ggplot2) # for the diamonds dataset 

shinyUI(pageWithSidebar(
headerPanel('US Home Loan Data'), 
sidebarPanel(
  selectInput("state", # choose the states
              label = "Choose a state to display", 
              choices = loan$state_full),
  sliderInput("range", # choose the range 
              label = "Borrower's Annual Income:",
              min = 10000, max = 500000, value = c(40000, 80000),step=1000),
  radioButtons("radio", label=h3("Bar charts"), choices=col_list[c(31)], selected = col_list[c(31)]),
  checkboxGroupInput("show_vars", "Columns in home loan to show:", 
                     col_list[c(11,12:19,24,25,30,31,33,35,29)], selected = col_list[c(19,31)])),
  
 mainPanel(
    tabsetPanel( tabPanel('select_data',dataTableOutput("mytable1")), 
  tabPanel('Gender', plotOutput("mytable2")), 
  tabPanel('Race',plotOutput("race")),
  tabPanel('data',dataTableOutput("mytable3")) )
) 
))
