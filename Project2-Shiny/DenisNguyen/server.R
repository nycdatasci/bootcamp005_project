shinyServer(function(input, output) {
  
  graphInput <- reactive({
    graphdat <- dat2014
    graphdat <- graphdat[graphdat$race == input$race,]
    graphdat <- graphdat[graphdat$sex == input$sex,]
    graphdat <- graphdat[graphdat$year >= input$year[1] & 
                           graphdat$year <= input$year[2],]
    i <- c()
    if ("total population" %in% input$population) {
      i[length(i)+1] <- 179
    }
    if ("births" %in% input$population) {
      i[length(i)+1] <- 4
    }
    if ("deaths" %in% input$population) {
      i[length(i)+1] <- 5
    }
    if ("migrants" %in% input$population) {
      i[length(i)+1] <- 92
    }
    graphdat[, c(3,i)]
  })
  
  output$plot <- renderGvis({
    gvisLineChart(
      graphInput(), options=list(
        lineWidth=3, pointSize=5,
        vAxis="{title:'Population'}",
        hAxis="{title:'Year'}", 
        width=950, height=800)) 
  })

  graphInputG <- reactive({
    graphdatG <- dat2014
    graphdatG <- graphdatG[graphdatG$race == input$raceG,]
    h <- switch(input$populationG,
                "total population" = 179,
                "births" = 4,
                "deaths" = 5,
                "migrants" = 92)
    graphdatG <- graphdatG[,c(2,3,h)]
    graphdatGG <- data.frame(graphdatG[1:47,2])
    colnames(graphdatGG)[1] <- "year"

    if ("0" %in% input$sexG) {
      graphdatGG <- cbind(graphdatGG,filter(graphdatG, sex == 0)[3])
      colnames(graphdatGG)[dim(graphdatGG)[2]] <- "All"
    }
    if ("1" %in% input$sexG) {
      graphdatGG <- cbind(graphdatGG,filter(graphdatG, sex == 1)[3])
      colnames(graphdatGG)[dim(graphdatGG)[2]] <- "Male"
    }
    if ("2" %in% input$sexG) {
      graphdatGG <- cbind(graphdatGG,filter(graphdatG, sex == 2)[3])
      colnames(graphdatGG)[dim(graphdatGG)[2]] <- "Female"
    }
    graphdatGG <- graphdatGG[graphdatGG$year >= input$yearG[1] & 
                               graphdatGG$year <= input$yearG[2],]
    data.frame(graphdatGG)
  })
  
  output$plotG <- renderGvis({
    gvisLineChart(
      graphInputG(), options=list(
        lineWidth=3, pointSize=5,
        vAxis="{title:'Population'}",
        hAxis="{title:'Year'}", 
        width=950, height=800))
  })
########  
  graphInputR <- reactive({
    graphdatR <- dat2014
    graphdatR <- graphdatR[graphdatR$sex == input$sexR,]
    t <- switch(input$populationR,
                "total population" = 179,
                "births" = 4,
                "deaths" = 5,
                "migrants" = 92)
    graphdatR <- graphdatR[,c(1,3,t)]
    graphdatRR <- data.frame(graphdatR[1:47,2])
    colnames(graphdatRR)[1] <- "year"
    
    if ("0" %in% input$raceR) {
      graphdatRR <- cbind(graphdatRR,filter(graphdatR, race == 0)[3])
      colnames(graphdatRR)[dim(graphdatRR)[2]] <- "All"
    }
    if ("1" %in% input$raceR) {
      graphdatRR <- cbind(graphdatRR,filter(graphdatR, race == 1)[3])
      colnames(graphdatRR)[dim(graphdatRR)[2]] <- "White"
    }
      if ("2" %in% input$raceR) {
      graphdatRR <- cbind(graphdatRR,filter(graphdatR, race == 2)[3])
      colnames(graphdatRR)[dim(graphdatRR)[2]] <- "Black"
    }
    if ("3" %in% input$raceR) {
      graphdatRR <- cbind(graphdatRR,filter(graphdatR, race == 3)[3])
      colnames(graphdatRR)[dim(graphdatRR)[2]] <- "American Indian/Alaska Native"
    }
    if ("4" %in% input$raceR) {
      graphdatRR <- cbind(graphdatRR,filter(graphdatR, race == 4)[3])
      colnames(graphdatRR)[dim(graphdatRR)[2]] <- "Asian"
    }
    if ("5" %in% input$raceR) {
      graphdatRR <- cbind(graphdatRR,filter(graphdatR, race == 5)[3])
      colnames(graphdatRR)[dim(graphdatRR)[2]] <- "Native Hawaiian/Other Pacific Islander"
    }
    if ("6" %in% input$raceR) {
      graphdatRR <- cbind(graphdatRR,filter(graphdatR, race == 6)[3])
      colnames(graphdatRR)[dim(graphdatRR)[2]] <- "Two or more"
    }
    graphdatRR <- graphdatRR[graphdatRR$year >= input$yearR[1] & 
                             graphdatRR$year <= input$yearR[2],]
    data.frame(graphdatRR)
  })

  output$plotR <- renderGvis({
    gvisLineChart(
      graphInputR(), options=list(
        lineWidth=3, pointSize=5,
        vAxis="{title:'Population'}",
        hAxis="{title:'Year'}", 
        width=950, height=800))
  })
  
  output$data2014 <- DT::renderDataTable(
    DT::datatable({
      data <- dat2014
      data <- data[data$race == input$racet,]
      data <- data[data$sex == input$sext,]
      data[, c(3,179,4,5,92)]
    }))
})