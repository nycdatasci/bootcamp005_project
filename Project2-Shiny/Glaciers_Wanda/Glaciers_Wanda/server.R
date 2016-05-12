shinyServer(function(input, output){
 
  basemap_data <- reactive({
    return (glacP[glacP$geog_area == input$selected,])
  })
  
  ## world map
 output$plotworld <- renderLeaflet({
    leaflet(gen.data) %>%
       addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',attribution='Map tiles by
                 <a href="http://stamen.com">Stamen Design</a>,<a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash;
                 Map data &copy; <a href="http://www.openstreetmap.org
                 /copyright">OpenStreetMap</a>') %>% 
        addCircles(~LONGITUDE, ~LATITUDE, popup=paste("Name:", gen.data$NAME,"<br>",
                                                      "Lat:", gen.data$LATITUDE, "<br>",
                                                      "Long:", gen.data$LONGITUDE,"<br>"), weight = 3, 
                   radius=400,color="#00ecff", stroke = TRUE, fillOpacity = 0.8)
  
  })

  ## glacier map
  output$plot1 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      #setView(-73.944911, 40.732839, zoom = 11) %>%
      addPolygons(data = basemap_data(),
                  stroke = FALSE, weight = 2,
                  fillOpacity = 0.1, smoothFactor = 0.5,
                  color = cbPalette[input$slider[1]-2007])
    #,color = cbPalette[input$slider[1]-2007])
  })
  
linechart_data <- reactive({
    return (filter(NE.Gr.new, GEN_LOCATION %in% input$selectedts)%>% group_by(., YEAR) %>% summarise(., mean.annual
            = mean(ANNUAL_BALANCE))%>% mutate(., mean.cumulative = cumsum(mean.annual))
            )
})
  
# need to pretty it up
# maybe add a map here too or region? 

output$LineChart <- renderGvis({
 gvisLineChart(data=linechart_data(), xvar ="YEAR", yvar=c("mean.annual", "mean.cumulative"), 
 options=list(title="Glacier", 
 titleTextStyle="{color:'blue',fontName:'Courier',fontSize:16}",
 vAxes="[{title:'Mass Balance',
 titleTextStyle: {color: 'blue'},
 textStyle:{color: 'blue'},
 textPosition: 'out'}, 
 {title:'Millions',
 format:'#,###',
 titleTextStyle: {color: 'red'},  
 textStyle:{color: 'red'},
 textPosition: 'out'}]",
 hAxes="[{title:'Year', format: '####',
 textPosition: 'out'}]",
 legend="bottom",
 width=750, height=700
 ))
})

#Slider
sliderValues <- reactive({ #an expression whose result will change over time.
    print (input$slider)
    data.frame(
    Value = as.character(c(input$slider)) #, Balance = as.character(c(input$slider))
    )
   })
    
})
