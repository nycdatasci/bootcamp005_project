library(shiny)
library(dplyr)
library(ggplot2)
load("/Users/adamcone/Desktop/projects/Shiny Project/App/shinydataframe.RData")

server <- function(input, output) {
  output$Price_Range_selected <- renderPrint({ length(input$Price_Range_selected) })
  output$Payment_Types_selected <- renderPrint({ length(input$Payment_Types_selected) })
  output$Days_of_Week_selected <- renderPrint({ length(input$Days_of_Week_selected) })
  output$Times_of_Day_selected <- renderPrint({ length(input$Times_of_Day_selected) })
  data <- reactive(items_filter(items_tbl,
                                input$Price_Range_selected,
                                input$Payment_Types_selected,
                                input$Days_of_Week_selected#,
                                #input$Times_of_Day_selected
                                )
  )
  output$Revenue_Plot <-
    renderPlot({
      ggplot(data = data(),
             mapping = aes(x = data$Date_Bin,
                           y = plot_tbl$Revenue_Total
             )
      ) +
        theme_bw() +
        geom_freqpoly(stat = "identity")
    })
}