library(shiny)
library(shinydashboard)
library(leaflet)
library(RDSTK)
library(htmltools)

###############################################################
###############################################################
#This map contains three features:
# 1. A map where you can plot all NYC cultural institutions of chosen disciplines. You can also click on the institution to get info.
# 2. A map by zipcode where you can shade zipcodes by total instiutions or institutions per square mi for selected disciplines
# 3. A search box that will display institutions with info (any info) matching your search
################################################################
################################################################

shinyServer(function(input, output) {
  
  reactive({shade_choice = input$shade_choice})
  
  #shadeChoice returns a function that takes two arguments data, disciplines, and depends on the user input
  # of input$shade_choice that can be either total or persqmi. The function then calculates the total/per sqmi
  #institutions using the data.frame @data and the columns @disciplines
  shadeChoice <- reactive({
    function(data, disciplines) {
    total = apply(as.data.frame(data[,disciplines]), 1, sum)
    switch(input$shade_choice,
           "Total" = total,
           "Per square mile" = total/data['LANDSQMI']
           #"Per person" = total/data['POPULATION']
     )
    }
  })
  
  #pal2 returns a color palette that is based on what shadeChoice is chosen (see above) and the disciplines selected
  
  pal2 <- reactive({
    #Need to put this in so that if no disciplines are selected, the map is cleared (no palette is chosen)
    if(is.null(input$disciplines))
    {
      leafletProxy("zipmap") %>% clearShapes() %>% clearControls()
      NULL
    }
    #This returns a scale of colors.
    #Domain is the domain of possible inputs (need to filter out the NaNs and Infs from the numbers and replace them
    # with NAs (colors for NAs are handles)
    colorNumeric("Reds", domain = shadeChoice()(nyc@data, input$disciplines)[sapply(shadeChoice()(nyc@data, input$disciplines), is.finite)])
  })
  

  #Filters data based on disciplines selected
  #We use event reactive so that it reacts whenever input$disciplines changes (otherwise it only changes
  # when the groupcheckbox is nonempty. I also had some problem of the leafletProxy not triggering when a box
  # was deselected. EventReactive responds to all actions).
  disciplinesPoData <- eventReactive(input$disciplines, {
    filter(ci.no.po, Discipline %in% input$disciplines)
  })
  
  ###############################################
  ####### MAP THAT PLOTS INSTITUTIONS ############
  #############################################
  output$map <- renderLeaflet({
    leaflet() %>% 
      #setView(lat = 40.667, lng = -73.963, zoom = 11) %>%
      setView(lat = 40.697, lng = -73.993, zoom = 12) %>%
      addTiles(
        urlTemplate = 'http://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png', 
          attribution = 'Tiles courtesy of <a href="http://openstreetmap.se/" target="_blank">OpenStreetMap Sweden</a> &mdash; 
          Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
      )%>%
      addLegend("topleft", pal = pal, values = levels(ci.no.po$Discipline), labels =  levels(ci.no.po$Discipline),
                title = "Disciplines",
                opacity = .5)
  })

  #Updates makers and popups for each change in selected of disciplines
  observe({
    leafletProxy("map", data = disciplinesPoData()) %>%
      clearMarkers() %>%
      addCircleMarkers(radius=5,
                       #If there is an NA it gives a warning.
                       color = ~suppressWarnings(pal(Discipline)),
                       fillOpacity = .7,
                       stroke = F,
                       popup = ~paste(sep = '<br><br>',
                                      paste("<b>", Organization.Name, "</b>"),
                                      paste(Address, "<br>", City, ", ", State, " ", Zip.Code),
                                      paste("Discipline: ", Discipline),
                                      paste("Phone: <a href=\"tel:", Main.Phone.., "\">", Main.Phone..,"</a>")
                       )
      )
  })
  #I'm not sure why this needs to be a seperate statement, but if input$disciplines is NULL (i.e. no disciplines
  # selected, then the server does not enter the observeEvent above (or observe which I tried).
  # Thus, this observeEvent deals with when no disciplines are selected and clears the map.
  observeEvent(is.null(input$disciplines), ignoreNULL = F, {
    if(is.null(input$disciplines))
      leafletProxy("map") %>% clearMarkers()
  })
  
  ####################################################
  #####################################################
  ###### PLOTS FOR SHADING ZIPCODES
  #########################################
  ############################################
  
  
  #This isn't in an observe because this map changes with every user input change. However, if one were
  # to add something that didn't change this map, you could put this in an observe function.
  output$zipmap <- renderLeaflet({
    leaflet() %>%
      setView(lat = 40.667, lng = -73.963, zoom = 11) %>%
      addTiles(
        urlTemplate = 'http://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png',
        attribution = 'Tiles courtesy of <a href="http://openstreetmap.se/" target="_blank">OpenStreetMap Sweden</a> &mdash;
        Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
        # urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        # attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        # urlTemplate = 'http://otile{s}.mqcdn.com/tiles/1.0.0/{sat}/{z}/{x}/{y}.jpg',
        #   attribution = 'Tiles Courtesy of <a href="http://www.mapquest.com/">MapQuest</a> &mdash;
        # Portions Courtesy NASA/JPL-Caltech and U.S. Depart. of Agriculture, Farm Service Agency',
        # subdomains = '1234'
        #addProviderTiles("MapQuestOpen.Aerial") %>%
      ) %>%
      clearShapes() %>% clearControls() %>%
      addPolygons(data = nyc,
                  stroke = FALSE, fillOpacity = .7, smoothFactor = 0.5,
                  #Assigns a color to each value, If the value is NaN or Inf, it replaces it with an NA (which is dealt with by the pallete)
                  color = ~pal2()(sapply(shadeChoice()(nyc@data, input$disciplines), function(x) ifelse(is.finite(x), x, NA))),
                  popup = ~paste(sep = '<br><br>',
                                 paste("<b>", GEOID10, "</b>"),    #Zipcode
                                 paste("Number of institutions selected", 
                                       ifelse(input$shade_choice == 'Total', ': ', 'per squre mile: '),
                                       as.character(round(sapply(shadeChoice()(nyc@data, input$disciplines), function(x) ifelse(is.finite(x), x, NA)),
                                                          digits = 2)   #Rounds selected choice to 2 decimal places
                                       )
                                 )
                  )
                  
      ) %>%
      addLegend("topleft", pal = pal2(), 
                values = sapply(shadeChoice()(nyc@data, input$disciplines), function(x) ifelse(is.finite(x), x, NA)),
                title = switch(input$shade_choice,
                               'Total' = 'Institutions',
                               'Per square mile' = 'Institutions/sq mi'
                ),
                bins = 5,
                opacity = .5)
  })
    
  #Outputs a table with all rows where any value in that row matches input$searchbox (Text from box in ui)
  output$table <- renderGvis({
    
    gvisTable(filter(ci, apply(sapply(ci, function(x) grepl(input$searchbox, ignore.case = T, x)),1,any)),
        options = list(page = 'enable'))
  })
} 
)
