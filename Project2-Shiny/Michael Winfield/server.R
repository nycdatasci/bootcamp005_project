library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(reshape2)
library(googleVis)

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      clearShapes() %>%
      addTiles() %>%
      setView(lng = -25, lat = .07, zoom = 2)
  })
  
  
  filteredviz <- reactive({
    print (input$variable)
    
    return (filter_(viz[1:53330,], paste(input$variable, ">=", 1)))
    #return (filter_(viz[1:x,] where x is a smaller sample
  })
  

  
  observe({
    variable <- input$variable
    print(nrow(filteredviz()))
    
    colorgr <- as.factor(c("Storms", "Rain", "Fog"))
    
    factpal <- colorFactor(topo.colors(3), colorgr)
    
    leafletProxy("map", data = filteredviz()) %>% 
      clearShapes() %>%
      addCircles(~Long, ~Lat, popup=paste(filteredviz()$Temp1, "degrees on", filteredviz()$Month, "/",filteredviz()$Day,"/",filteredviz()$Year), radius=3,
                 stroke=TRUE, fillOpacity=0.4, color = ~factpal(input$variable))
})
  
  output$scatterlong <- renderPlot({
    ggplot(viz, aes(x=Long, y=Temp1)) + geom_point(aes(color=Temp1))+geom_density2d()+
      ggtitle("Temperature by Longitude")+
      xlab("Longitude")+
      ylab("Temperature")
  })
  
  output$scatterlat <- renderPlot({
    ggplot(viz, aes(x=Lat, y=Temp1)) + geom_point(aes(color=Temp1))+geom_density2d()+
      ggtitle("Temperature by Latitude")+
      xlab("Latitude")+
      ylab("Temperature")
  })
  
  output$avgtempyear <- renderPlot({
    viz2 <- group_by(viz, Year) %>% summarize(., Temp = mean(Temp1))
    viz2$Year <- as.factor(viz2$Year)
    ggplot(viz2, aes(x=Year, y=Temp), color = Year) + geom_bar(aes(fill=Year), stat='identity')+
      ggtitle("Average Temperature by Year")+
      xlab("Year")+
      ylab("Mean Temperature")
  })
  
  output$varintr <- renderPlot({
    viz$Year <- as.factor(viz$Year)
    ggplot(viz, aes(x=Year, y=Temp1)) + geom_freqpoly(aes(color=Year), stat='identity')+
      ggtitle("Variability in Temperature Measurements by Year")+
      xlab("Year")+
      ylab("Temperature")
  })
  
  
  output$varmonth <- renderPlot({
    viz$Month <- as.factor(viz$Month)
    ggplot(viz, aes(x=Month, y=Temp1)) + geom_freqpoly(aes(color=Month), stat='identity')+
      ggtitle("Variability in Temperature Measurements by Month")+
      xlab("Month")+
      ylab("Temperature")
  })
  
  output$MonthPlot <- renderPlot({
    viz$Month <- as.factor(viz$Month)
    viz4 <- group_by(viz, Month) %>% summarise(., Rain = sum(Rain), Storms = sum(Storms), Fog = sum(Fog), Temp = round(mean(Temp1)))
    vizb <- select(viz4, Rain, Storms, Fog, Temp)
    vizb <- as.matrix(vizb)
    viza <- t(vizb)
  
  barplot(viza[,input$Month], 
          main=paste("Weather and Temperature in", month.abb[input$Month],"(over all years)"),
          ylab="Value",
          xlab="Weather and Temperature", col=c("darkblue","red", "green", "yellow"),
          ylim=c(1,400))
  })
})
  


  