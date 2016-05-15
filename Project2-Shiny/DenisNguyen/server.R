shinyServer(function(input, output) {

### Code for Insight: Population tab
  graphInput <- reactive({
# Choosing information based on widget inputs
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
    if ("migration" %in% input$population) {
      i[length(i)+1] <- 92
    }
    graphdat[, c(3,i)]
  })
# Graphing data of interest
  output$plot <- renderGvis({
    gvisLineChart(
      graphInput(), options=list(
        lineWidth=3, pointSize=5,
        vAxis="{title:'Population'}",
        hAxis="{title:'Year'}", 
        width=950, height=750)) 
  })


### Code for Insight: Gender tab
  graphInputG <- reactive({
# Choosing information based on widget inputs
    graphdatG <- dat2014
    graphdatG <- graphdatG[graphdatG$race == input$raceG,]
    h <- switch(input$populationG,
                "total population" = 179,
                "births" = 4,
                "deaths" = 5,
                "migration" = 92)
    graphdatG <- graphdatG[,c(2,3,h)]
    graphdatGG <- data.frame(graphdatG[1:47,2])
    colnames(graphdatGG)[1] <- "year"
    if ("0" %in% input$sexG) {
      graphdatGG <- cbind(graphdatGG,filter(graphdatG, sex == 0)[3])
      colnames(graphdatGG)[dim(graphdatGG)[2]] <- "Both"
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
# Graphing data of interest
  output$plotG <- renderGvis({
    gvisLineChart(
      graphInputG(), options=list(
        lineWidth=3, pointSize=5,
        vAxis="{title:'Population'}",
        hAxis="{title:'Year'}", 
        width=950, height=750))
  })

  
### Code for Insight: Race tab
  graphInputR <- reactive({
# Choosing information based on widget inputs
    graphdatR <- dat2014
    graphdatR <- graphdatR[graphdatR$sex == input$sexR,]
    t <- switch(input$populationR,
                "total population" = 179,
                "births" = 4,
                "deaths" = 5,
                "migration" = 92)
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
    if ("7" %in% input$raceR) {
      graphdatRR <- cbind(graphdatRR,filter(graphdatR, race == 7)[3])
      colnames(graphdatRR)[dim(graphdatRR)[2]] <- "Not Hispanic"
    }
    if ("8" %in% input$raceR) {
      graphdatRR <- cbind(graphdatRR,filter(graphdatR, race == 8)[3])
      colnames(graphdatRR)[dim(graphdatRR)[2]] <- "Hispanic"
    }
    graphdatRR <- graphdatRR[graphdatRR$year >= input$yearR[1] & 
                             graphdatRR$year <= input$yearR[2],]
    data.frame(graphdatRR)
  })
# Graphing data of interest
  output$plotR <- renderGvis({
    gvisLineChart(
      graphInputR(), options=list(
        lineWidth=3, pointSize=5,
        vAxis="{title:'Population'}",
        hAxis="{title:'Year'}", 
        width=950, height=750))
  })

  
### Code for Map tab
# Choosing information based on widget inputs
  choice <- reactive({
    capitalize(input$changeS)
  })
  lowyear <- reactive({
    input$rangeS[1]
  })
  highyear <- reactive({
    input$rangeS[2]
  })
# Displaying data of interest
  output$myrange <- renderText({
    paste(choice(), "difference from", lowyear(), "to", highyear())
  })
  output$plotS <- renderGvis({
    spops <- spop[,c(grep("states", colnames(spop)),
                     grep(input$rangeS[1], colnames(spop)),
                     grep(input$rangeS[2], colnames(spop)))]
    if (input$changeS == "percent") {
      spops[,4] <- 100*(spops[,3]-spops[,2])/spops[,2]
      colnames(spops)[4] <- "change"
    }
    if (input$changeS == "total") {
      spops[,4] <- spops[,3]-spops[,2]
      colnames(spops)[4] <- "change"
    }
    gvisGeoChart(spops, locationvar="states", colorvar="change",
                 options=list(region="US", displayMode="regions", 
                              resolution="provinces",
                              width=900, height=650,
                              colorAxis="{colors:['#deebf7', '#3182bd']}"))
  })
  
  
### Code for Projections tab
  graphInputP <- reactive({
# Choosing information based on widget inputs
    graphdat2008 <- dat2008[(dat2008$race == 0) & (dat2008$sex == 0),]
    colnames(graphdat2008)[7] <- '2008'
    graphdat2012 <- dat2012[(dat2012$race == 0) & (dat2012$sex == 0),]
    colnames(graphdat2012)[179] <- '2012'
    graphdat2014 <- dat2014[(dat2014$race == 0) & (dat2014$sex == 0),]
    colnames(graphdat2014)[179] <- '2014'
    for (i in 1:13) {
      graphdat2014 <- rbind(graphdat2014, c(rep(NA, dim(graphdat2014)[1])))
      graphdat2014$year[47+i] <- 2000 + i
    }
    for (i in 1:11) {
      graphdat2012 <- rbind(graphdat2012, c(rep(NA, dim(graphdat2012)[1])))
      graphdat2012$year[49+i] <- 2000 + i
    }
    for (i in 1:10) {
      graphdat2008 <- rbind(graphdat2008, c(rep(NA, dim(graphdat2008)[1])))
      graphdat2008$year[50+i] <- 2050 + i
    }
    graphdatP <- merge(merge(graphdat2014[,c(3,179)], graphdat2012[,c(3,179)], by="year"), 
                       graphdat2008[,c(3,7)], by="year")
    q <- c()
    if ("2008" %in% input$projectionP) {
      q[length(q)+1] <- 4
    }
    if ("2012" %in% input$projectionP) {
      q[length(q)+1] <- 3
    }
    if ("2014" %in% input$projectionP) {
      q[length(q)+1] <- 2
    }
    graphdatP <- graphdatP[,c(1,q)]
    graphdatP <- graphdatP[graphdatP$year >= input$yearP[1] & 
                               graphdatP$year <= input$yearP[2],]
    graphdatP
  })
# Graphing data of interest
  output$plotP <- renderGvis({
    gvisLineChart(
      graphInputP(), options=list(
        lineWidth=3, pointSize=5,
        vAxis="{title:'Population'}",
        hAxis="{title:'Year', format:'####'}",
        width=950, height=750))
  })


### Code to display Data tab
  output$data2014 <- DT::renderDataTable(
    DT::datatable({
      data <- dat2014
      data <- data[data$race == input$racet,]
      data <- data[data$sex == input$sext,]
      data[, c(3,179,4,5,92)]
    }))
})