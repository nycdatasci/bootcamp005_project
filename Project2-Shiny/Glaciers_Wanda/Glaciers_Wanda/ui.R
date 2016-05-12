## ui.R ##

###*** it does show the outline, need to clarify shading slider
#glac.choice <- unique(as.character(glacP$geog_area))
glacier.choice<-c("East Central Greenland","Northeast Greenland","Iceland", "Northern Cordillera", "Alaska")
#Map: keep only the ones that work

locations <- unique(as.character(NE.Gr$GEN_LOCATION))

shinyUI(dashboardPage( skin = "purple",
  dashboardHeader(title = "Glacier Mass Balance", titleWidth=230),
  dashboardSidebar(
    sidebarUserPanel("Wanda Wang",
                     image = "http://2igww43ul7xe3nor5h2ah1yq.wpengine.netdna-cdn.com/wp-content/uploads/2016/05/IMG_2035-2-300x300.jpg"),
    sidebarMenu(
      menuItem("WorldMap", tabName = "world", icon = icon("globe")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("line-chart"))
     # menuItem("Data Source", tabName = "timeseries", icon = icon("database")),
     ) 
  
    # need a color legend for mass balance/representation of melting behavior/animation
   ),
  
  dashboardBody(
  tabItems(
    tabItem(tabName = "world", 
          fluidRow(
            box(" The World Glacier Monitoring Service (WGMS) reports data on 3681 glaciers around the world. 
             The available location data is plotted on a global scale below."
            ),
              box(
                title = "Glacier map", status = "success", 
                solidHeader = TRUE, collapsible = TRUE,
                leafletOutput("plotworld"), 
                width = 12)
            )
              ),
    tabItem(tabName = "map", 
        fluidRow(
          box(
            title = "Select Glacier", status = "info", 
            solidHeader = TRUE, collapsible = TRUE,
            
            selectizeInput("selected",
                         "Select Glacier to Display",
                         choices = glacier.choice,
                         selected = "Northeast Greenland"
                         )
            ),      
          
          box("Crucial to the survival of a glacier is its mass balance or surface mass balance (SMB), 
             the difference between accumulation and ablation (sublimation and melting). Climate change may cause variations in both 
             temperature and snowfall, causing changes in the surface mass balance"
          )
        ),
        fluidRow(
                 box(
                   title = "Glacier Viewer", status = "success", 
                   solidHeader = TRUE, collapsible = TRUE,
                   leafletOutput("plot1"), 
                 width = 12),
                 box(
                 sliderInput("slider", "Annual Melt", 
                             min = 2008, max = 2014, value = 0, step = 1, sep = "",
                             format="$#,##0", locale="us", animate=TRUE)
                 )
                 
        )
       ),
       
    tabItem(tabName = "timeseries", 
            fluidRow(
              box(
                title = "Select Glacier", status = "info", 
                solidHeader = TRUE, collapsible = TRUE,
                
                selectizeInput("selectedts",
                               "Select Glacier Location",
                               locations,
                               selected = "NE GREENLAND"
              )),
              
              box("Crucial to the survival of a glacier is its mass balance or surface mass balance (SMB), 
             the difference between accumulation and ablation (sublimation and melting). Climate change may cause variations in both 
                  temperature and snowfall, causing changes in the surface mass balance"
              )
            ),
            #image 
           fluidRow(
             box(
               title = "Time Series Chart", status = "primary",
               solidHeader = TRUE, collapsible = TRUE,
               htmlOutput("LineChart"),   #probelem with dates, years of data being different between glaciers.
               width = 15
               ) 
           )
    )
  ))
))
