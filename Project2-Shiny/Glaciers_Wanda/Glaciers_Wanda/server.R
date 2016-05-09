library(maptools)
library(ggplot2)
library(ggmap)
library(rgeos)
library(dplyr)
library(stringi)
library(sp)
library(ggvis)
library(googleVis)

shinyServer(function(input, output){
  #bluevis <- reactive({})
  
  #two different time periods 
  
  #redvis <- reactive({})
  # make it into an image?????
  #show bigger map
  #different shades for each glacier??
  #dropdown by region instead
  #let's look at the data again.
  #Norway, Iceland, Canada, Artic Circle 
  
  vis <- reactive({

    glacP <- readShapePoly(fn = "glims_download_28564/glims_polygons.shp")
    glacP <- gBuffer(glacP, byid=TRUE, width=0)
    glac.NE  <- glacP[glacP$geog_area == "Northeast Greenland",]
    
   #glac.choice <- as.data.frame(glac.NE$geog_area[1])
    glac.choice <- as.data.frame(c("Northeast Greenland", "Alaska"))
    
    maap <- ggplot2::fortify(glac.NE, region = "glac_name")
    maap$id = "NE GREENLAND"
    NE.Gr <- id.data %>% filter(., YEAR == 2008:2014) %>% select(., NAME.x, YEAR, ANNUAL_BALANCE, 
             GEN_LOCATION, SPEC_LOCATION, LATITUDE, LONGITUDE,GEO.REGION_CODE)
    MapDf <- merge(maap, NE.Gr, by.x="id", by.y="GEN_LOCATION") #done
    
    MapDf %>% ggvis(~long, ~lat) %>% group_by(group, id) %>% layer_paths(strokeOpacity:=0.5, 
             stroke:="#7f7f7f") %>% hide_legend("fill") %>% hide_axis("x") %>% hide_axis("y") %>%  set_options(width=400, 
                                                                                                               height=600, keep_aspect=TRUE)
    #MapDfnew <- group_by(MapDf, YEAR) %>% summarise_each(funs(mean), ANNUAL_BALANCE)%>% mutate(MapDf, 
                                                #  CUMULATIVE_BAL = cumsum(ANNUAL_BALANCE))
    
   # MapDf$colour <- ifelse(MapDf$CUMULATIVE_BAL <= 0, 'red', 'blue')
    MapDf$colour <- ifelse(MapDf$id == 'NE GREENLAND', 'blue')
    
    #group_by color
    #red<-MapDf %>% group_by(., colour) %>% filter(., colour == 'red')
    #problem: it could be red, blue, red, blue. not sequential
    #cumulative is sequential. but not feasible 
    #manually do it. if it's a certain year color it differently
    
                       # ifelse(MapDf$id == 'NE GREENLAND', 'blue') # need to scale it by year tho, input?
    #if statement is only considering location, not year. 
    
    #blue one
    #red one
    #if MapDf$colour is red, fill red
    #if MapDf$colur is blue, fill blue
    
    #RedGlacier
    #BlueGlacier
    
    #*** MapDf has polygon fields, necessary for coloring in?
    Glacier<-MapDf %>% group_by(group, id)%>% #Year?
      ggvis(~long, ~lat) %>%
      layer_paths(fill:= ~factor(colour))
    
    #i don't think this is dynamic. 
    
    #for one single year show the mass balance of multiple glaciers. manually?
    # force it by glacier name then. with the drop down to link it. include a legend for the color and mass balance representation.
    # i had initially wanted to show mass balance change for each year. but i guess the time series does that, for each glacier selected. 
  })
  
  vis %>% bind_shiny("plot1")
  ######
  #bluevis %>% bind_shiny("plot.blue")
  
  #redvis %>% bind_shiny("plot.red")
  
   output$LineChart <- renderGvis({
      gvisLineChart(data=FREYA, xvar ="YEAR", yvar=c("ANNUAL_BALANCE", "CUMULATIVE_BAL"),
                    options=list(title="Freya Glacier",
                                 titleTextStyle="{color:'blue',fontName:'Courier',fontSize:16}")) 
   })
   
   sliderValues <- reactive({ #an expression whose result will change over time.
    data.frame(
      Value = as.character(c(input$slider))
    )
   })
   
   output$values <- renderTable({
     sliderValues() #captures the ui.R input$slider field year selected
   })
   #renderPlot
   #renderGvis googleVis Chart
})
