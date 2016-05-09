# server.R

library(quantmod)
source("helpers.R")

shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    data <- getSymbols(input$symb, src = "yahoo", 
                       from = input$dates[1],
                       to = input$dates[2],
                       auto.assign = FALSE)
    A=as.data.frame(data)
    
    
    
   #write.csv(data, file = "INTC.csv")
   # write.csv(data, file = "BP.csv")
   #write.csv(data, file = "AMD.csv")
   
  #write.csv(data, file = "CVX.csv")
  #write.csv(data,file="XOM.csv")
  # write.csv(data,file="GS.csv")
  # write.csv(data,file="JPM.csv")
  # write.csv(data,file="INTC.csv")
   write.csv(A,file="REF.csv")
   
    chartSeries(data, theme = chartTheme("white"), 
                type = "line", log.scale = input$log, TA = NULL)
  })
  
})