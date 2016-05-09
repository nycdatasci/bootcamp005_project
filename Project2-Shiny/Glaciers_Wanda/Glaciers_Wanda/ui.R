## ui.R ##
library(shiny)
library(shinydashboard)
library(googleVis)
library(ggvis)

shinyUI(dashboardPage(
  dashboardHeader(title = "Glacier statistics"),
  dashboardSidebar(
    sidebarUserPanel("Wanda Wang",
                     image = "https://avatars1.githubusercontent.com/u/10689720?v=3&s=400"),
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("map"))
     )
   ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map", 
        fluidRow(
          box(
            title = "Select Glacier", status = "info", 
            solidHeader = TRUE, collapsible = TRUE,
            
            selectizeInput("selected",
                         "Select Glacier to Display",
                         glac.choice)), ################add more later!!!!!!
          
          box(
            title = "Glacier map", status = "success", 
            solidHeader = TRUE, collapsible = TRUE,
            
           # ifelse(sliderInput == 2000:2009, uiOutpul("plot1_ui"), )
            uiOutput("plot1_ui"),
            ggvisOutput("plot1") 
            # how to iniate/connect this with sliderInput???
            
            #####
          )
        ),
        
        fluidRow(
          box(
            "Select Year",
            sliderInput("slider", "Slider input:",
                        2000, 2014, value = c(2000, 2014), sep = "")
          )
          #server.R
        ),
        
        fluidRow(
        mainPanel(
          tableOutput("values") #displays a table with the selected slider years
          #instead need to output the value and feed it into the glacier graph
        )
        ),
        
        fluidRow(
          box(
            title = "Time Series Chart", status = "primary",
            solidHeader = TRUE,collapsible = TRUE,
            htmlOutput("LineChart"),
            width = 15) 
        )
        ###add in leaflet?
        ###add in bubble chart? 
        ##plotly
    )
  ))
))
