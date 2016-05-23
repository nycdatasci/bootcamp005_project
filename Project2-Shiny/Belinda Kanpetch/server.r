# server.r
library(shiny)
library(leaflet)
library(CartoDB)
library(RColorBrewer)
library(dplyr)
library(markdown)
library(ggplot2)
library(DT)
library(shinythemes)


#### clean up data set
#### decode species code
#### add popup info
#### try eliminating if statements for map... ie graph if statements
#### make the scale for the graph reactive to the what it's being scaled by

server <- function(input, output, session) {
  
  output$out_sel_species = renderPrint(input$in_sel_species)
  
  #################################
  ###### Interactive Map Tab ######
  #################################
  
  # Creating the base map
  output$mymap = renderLeaflet({
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>%
      setView(-73.944911, 40.732839, zoom = 11) %>%
      addPolygons(data = cdshp,
                  stroke = TRUE, weight = 2,
                  fillOpacity = 0.1, smoothFactor = 0.5,
                  color = 'gray',
                  group = 'Community_Districts',
                  popup = ~as.character(boro_cd)) %>%
      addLayersControl(
        overlayGroup = 'Community_Districts')
  })
  
  # defining the map object
  data_map = reactive({
    # for debugging
    print(input$in_sel_species)
    print(input$in_sel_borough)
    
    if (!is.null(input$in_sel_species) & !is.null(input$in_sel_borough)) {
      return(filter(randsamp, (new_species %in% input$in_sel_species) &
                      (borough %in% input$in_sel_borough)))
    } else if (!is.null(input$in_sel_species)) {
      return(filter(randsamp, new_species %in% input$in_sel_species))
    } else if (!is.null(input$in_sel_borough)) {
      return(filter(randsamp, borough %in% input$in_sel_borough))
    } else {
      return(data.frame(matrix(ncol = 10, nrow = 0)))
    }
  })
  
  ### Creating a reactive map with user inputs of species or borough
  observe({
    # for debugging
    result = data_map()
    print(nrow(result))
    
    # plotting the points on the map
    if (nrow(result) > 1){
      leafletProxy('mymap', data = result) %>%
        clearMarkers() %>%
        addCircleMarkers(~long, ~lat, radius = 4, stroke = F,
                         fillOpacity = 0.5, popup =~as.character(new_species),
                         color = ~pal(new_species))
    } else {
      leafletProxy('mymap') %>%
        clearMarkers()
    }
  })
  
  
  
  ######################################
  ####### Creating reactive graph ######
  ######################################
  
  graphdata = reactive({
    if (input$scale == 'None') {
      df = trees %>%
        group_by_(input$input_xaxis) %>%
        summarise(total=sum(Freq)) %>%
        arrange(total)
    } else {
      df = trees %>%
        group_by_(input$input_xaxis, input$scale) %>%
        summarise(total = sum(Freq)) %>%
        arrange(total)
    }
  })
  
  
  #### ggplot graph
  output$graph = renderPlot({
    
    # for debugging
    print (graphdata())
    print (input$input_xaxis)
    
    if (input$sel_normalize == FALSE){
      if (input$scale == 'None'){
        ggplot(graphdata(), aes_string(x=input$input_xaxis, y ='total')) +
          geom_bar(stat = 'identity') +
          theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
          scale_fill_brewer(palette = 'Blues')
      } else {
        ggplot(graphdata(), aes_string(x = input$input_xaxis, y = "total")) +
          geom_bar(stat = 'identity', aes_string(fill = input$scale)) +
          theme(axis.text.x  = element_text(angle=90, vjust=0.5))  +
          scale_fill_brewer(palette = 'Blues')
      }
    } else {
      if (input$scale == 'None'){
        ggplot(graphdata(), aes_string(x=input$input_xaxis, y ='total')) +
          geom_bar(stat = 'identity', position = 'fill') +
          theme(axis.text.x  = element_text(angle=90, vjust=0.5))  +
          scale_fill_brewer(palette = 'Blues')
      } else {
        ggplot(graphdata(), aes_string(x = input$input_xaxis, y = "total")) +
          geom_bar(stat = 'identity', aes_string(fill = input$scale),
                   position = 'fill') +
          theme(axis.text.x  = element_text(angle=90, vjust=0.5))  +
          scale_fill_brewer(palette = 'Blues')
      }
    }
  })
  
  # output$graph = reactivePlot(mybarplot, width=, height = )
  
  ##### googleviz not working well
  # output$graph = renderGvis({
  #   print(input$input_xaxis)
  #   print(input$scale)
  #   if (input$scale == 'None'){
  #     gvisColumnChart(graphdata(), xvar = input$input_xaxis, yvar = 'total',
  #                     options = list(height = 600))
  #   } else {
  #     gvisColumnChart(graphdata(), xvar = input$input_xaxis, yvar = c('total', input$scale),
  #                     options = list(isStacked=TRUE, height = 600))
  #   }
  # })
  
  #############################
  ####### Data Table Tab ######
  output$table <- DT::renderDataTable(DT::datatable({
    print(input$data_borough)
    print(input$data_species)
    print(input$data_commdist)
    print(input$data_condition)
    
    data = all_trees
    if (input$data_borough != 'All') {
      data = data[data$borough == input$data_borough, ]
    }
    if (input$data_species != 'All') {
      data = data[data$new_species == input$data_species, ]
    }
    if (input$data_commdist != 'All'){
      data = data[data$NEW_CD == input$data_commdist, ]
    }
    if (input$data_condition != 'All'){
      data = data[data$treecondit == input$data_condition, ]
    }
    data
  }
  ))
}

