library(shiny)

shinyUI(fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine. 
               Information will be collected from yahoo finance."),
      
      #textInput("symb", "Symbol", "CVX"),
      #textInput("symb", "Symbol", "XOM"),
      #textInput("symb", "Symbol", "GS"),
      textInput("symb", "Symbol", "GS"),
      #textInput("symb", "Symbol", "JPM"),
      #textInput("symb", "Symbol", "INTC"),
      #textInput("symb", "Symbol", "AMD"),
    
      
      
      dateRangeInput("dates", 
                     "Date range",
                     start = "1999-06-01", 
                     end = "2016-01-01"),
      
      br(),
      br(),
      
      checkboxInput("log", "Plot y axis on log scale", 
                    value = FALSE),
      
      checkboxInput("adjust", 
                    "Adjust prices for inflation", value = FALSE)
      ),
    
    mainPanel(plotOutput("plot"))
  )
))