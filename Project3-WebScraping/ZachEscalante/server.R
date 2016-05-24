# server.r
shinyServer(function(input, output) { 
#------------Data----------------
  
  first_filtered<-reactive({
    filtered_data <- filter(df, Neighborhood == input$neighborhood)
  })
#------------Stats--------------
  output$text1 <- renderText({
    paste('There are', nrow(first_filtered()), 'apts for rent in ', input$neighborhood)
  })
  
  output$text2 <- renderText({
    paste('The average price per sq foot is: $', 
          round(sum(first_filtered()$Price[is.na(first_filtered()$sq_feet)==FALSE])/
          sum(first_filtered()$sq_feet, na.rm = TRUE), digits=2))
    
  })
  
  output$text3 <- renderText({
    paste('The average price for a studio is: $', 
          round(sum(first_filtered()$Price[first_filtered()$bed=='studio'])/
          sum(first_filtered()$bed=='studio'), digits = 2))
  })
  
  output$text4 <- renderText({
          paste('There are ', 
                sum(first_filtered()$bed == 'studio'),
                ' studios for rent')
  })
  
#------------Maps-----------------  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(-73.9857, 40.7484, zoom = 12) %>%
      addTiles()

  })
  
    filteredData <- reactive({
      hood <- input$neighborhood
      return (cdshp_nyc[cdshp_nyc$ntaname == hood, ])
    })

    observe({
      leafletProxy('mymap') %>%
        clearShapes() %>%
        addPolygons(data=filteredData(), fillColor = "red")
    })

})

  







