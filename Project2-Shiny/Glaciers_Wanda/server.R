library(googleVis)

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
  vis <- reactive({
    # Lables for axes
    #xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    #yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    glacP <- readShapePoly(fn = "glims_download_28564/glims_polygons.shp")
    glacP <- gBuffer(glacP, byid=TRUE, width=0)
    glac.NE  <- glacP[glacP$geog_area == "Northeast Greenland",]
    glac.choice <- as.data.frame(glac.NE$geog_area[1])
    maap <- ggplot2::fortify(glac.NE, region = "glac_name")
    maap$id = "NE GREENLAND"
    MapDf <- merge(maap, NE.Gr, by.x="id", by.y="GEN_LOCATION") #done
    MapDf %>% ggvis(~long, ~lat) %>% group_by(group, id) %>% layer_paths(strokeOpacity:=0.5, 
             stroke:="#7f7f7f") %>% hide_legend("fill") %>% hide_axis("x") %>% hide_axis("y") %>%  set_options(width=400, height=600, keep_aspect=TRUE)
    MapDf$colour <- ifelse(MapDf$id == 'NE GREENLAND', 'blue') # need to scale it by year tho, input?
    
    Glacier<-MapDf %>% group_by(group, id)%>%
      ggvis(~long, ~lat) %>%
      layer_paths(fill:= ~colour)
  })
  
  vis %>% bind_shiny("plot1")
  
   output$LineChart <- renderGvis({
      gvisLineChart(FREYA)
  })
})
