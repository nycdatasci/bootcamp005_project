shinyServer(function(input, output) {
  
  graphInput <- reactive({
    graphdat <- dat2014
    graphdat <- graphdat[graphdat$race == input$race,]
    graphdat <- graphdat[graphdat$sex == input$sex,]
    graphdat <- graphdat[graphdat$year >= input$year[1] & 
                           graphdat$year <= input$year[2],]
    i <- c()
    i <- c("total_pop","births","total_deaths","total_nim")
    print(input$population %in% i)
    if (input$population %in% "total_pop") {
      i[length(i)+1] <- 179
    }
    if (input$population %in% "births") {
      i[length(i)+1] <- 179
    }
    if (input$population %in% "total_deaths") {
      i[length(i)+1] <- 179
    }
    if (input$population %in% "total_nim") {
      i[length(i)+1] <- 179
    }
    

    graphdat[, c(3,179)]
  })
  
  output$plot <- renderGvis({
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