shinyServer(function(input, output){

  
  #---------------INTERACTIVE MAP------------------
  
  output$map <- renderLeaflet({
    leaflet(NYPD_for_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-74.0059700, 40.7142700, zoom = 10)

  }) #End of renderLeaflet
  

  data_map <- reactive({
    
      df <-  NYPD_for_map %>% 
          filter(Borough %in% input$borough, Offense %in% input$offense) %>%
          subset(Date > input$date[1] & Date < input$date[2])
    
  })


  observe ({
    result = data_map()
    print(data_map())
    if(nrow(result) > 1) {
      print(input$date)
      leafletProxy('map', data = result) %>%
        clearMarkers() %>%
        addCircleMarkers(radius = 5,
                         stroke = FALSE,
                         fillOpacity = 1,
                         popup = paste("Offense:",result$Offense, "<br>",
                                 "Borough:", result$Borough, "<br>",
                                 "Precint:", result$Precinct, "<br>",
                                 "Date:", result$Date, "<br>",
                                 "Sector:", result$Sector),
                                  color= ~pal(Offense)) 

    } else {  #End of if 
      leafletProxy('map') %>%
        clearMarkers()
    } #End of else

  }) #End of observe
  

  barchartdata <- reactive({
    
    validate(
      need(input$x != "None", "Please select x"),
      need(input$y != "None", "Please select y")
    )
      #ungroup(Offense_Borough)
      df <- Offense_Borough %>% 
        group_by_(input$x) %>%
        mutate(Total_Felonies= Count *1)
    
  }) #End of barchardata reactive
  
  output$barchart1 <- renderPlot({
    print(barchartdata())   
    p <- ggplot(barchartdata(), aes_string(x=input$x, y= input$y)) + 
      geom_bar(stat="identity") +
      theme_bw() +
      theme(legend.position = "right") +
      theme(legend.text=element_text(size=10)) +
      ggtitle(paste("Total Number of Felonies")) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
      theme(plot.title = element_text(lineheight=2, face="bold"))

     facets <- paste(input$facet_row, '~', input$facet_col)
     if (facets != '. ~ .')
       p <- p + facet_grid(facets)
  
    p
    
  }) #End of barchart1
  
  
  
  output$data_table <- DT::renderDataTable(DT::datatable({
    
    data <- NYPD_for_map
    
        if (input$off != "All") {
          data <- data[data$Offense == input$off,]
        }
        if (input$bor != "All") {
          data <- data[data$Borough == input$bor,]
        }
        if (input$c != "All") {
          data <- data[data$CompStat.Year == input$c,]
        }
    
    data
  })) #End of data table
  
  
  
  output$heatMap <- renderUI({
    
    ## here I'm creating the JSON through 'paste0()'.
    ## you can also use jsonlite::toJSON or RJSONIO::toJSON
    
    j <- paste0("[",NYPD_for_map[,"Latitude"], ",", NYPD_for_map[,"Longitude"], ",", "]", collapse=",")
    j <- paste0("[",j,"]")
    j
    
    tags$body(tags$script(HTML(sprintf("
                                       var addressPoints = %s
                                       var heat = L.heatLayer(addressPoints).addTo(map)"
                                       , j
    ))))
  })
  
  
}) #End of shinyServer











