shinyServer(function(input, output) {
  
  graphInput <- reactive({
    graphdat <- dat2014
    graphdat <- graphdat[graphdat$race == input$race,]
    graphdat <- graphdat[graphdat$sex == input$sex,]
    graphdat <- graphdat[graphdat$year >= input$year[1] & 
                           graphdat$year <= input$year[2],]
    #input$population = 
    graphdat[, c(3,179)]
  })
  
  output$plot <- renderGvis({
    #print(input$year)
    gvisLineChart(
      graphInput(), options=list(
        lineWidth=3, pointSize=5,
        title="Population Projection",
        vAxis="{title:'Population'}",
        hAxis="{title:'Year'}", 
        width=1050, height=780)) 
  })
  
  
  
  output$data2014 <- DT::renderDataTable(
    DT::datatable({
      data <- dat2014
      
      data <- data[data$race == input$racet,]
      data <- data[data$sex == input$sext,]
      data[, c(3,179,4,5,92)]
    }))
})