library(plyr)
library(dplyr)
library(reshape2)
library(leaflet)
library(RDSTK)
library(htmltools)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(googleVis)
library(rgdal) 
library(rgeos)
library(zipcode)

dashboardPage(
  skin = 'purple',
  dashboardHeader(title = "NYC Cultural Institutions", titleWidth = 240,
      dropdownMenu(notificationItem(HTML("Data provided by NYC Department of <br> Cultural Affairs "), icon = icon('question'), status = "info", href = 'http://www.nyc.gov/html/dcla'), badgeStatus = NULL, type = "notification",
                   icon = icon("question"))
  ),
  
  dashboardSidebar(
    sidebarUserPanel("By R. Castellano"),
    sidebarMenu(
      menuItem("Map Cultural Institutions", tabName = "mapmenu", icon = icon("bank")),
      menuItem("Examine by Zipcodes", tabName = "zipcodemenu", icon = icon("map")),
      menuItem("Search for Institutions", tabName = "search", icon = icon("search")),
      menuItem(#icon = icon("check"), 
                  checkboxGroupInput("disciplines", 
                                     label = "Disciplines", 
                                     choices = levels(ci.no.po$Discipline),
                                     selected = factor()
                  )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "mapmenu",
        leafletOutput("map", height = 760)
        
        
        
      ),
      tabItem(tabName = "zipcodemenu",
        leafletOutput("zipmap", height = 760),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                      draggable = TRUE, top = 50, left ="auto" , right =15 , bottom = "auto",
                      width = 140, height = "auto",
                      radioButtons('shade_choice', "Shade by institutions:",
                                   c("Total", "Per square mile" 
                                     #"Per person"
                                   ), 
                                   selected = NULL, inline = FALSE, width = NULL)
        )
      ),
      tabItem(tabName = "search",
        textInput("searchbox", label = "Search for an institution"),
        fluidRow(
          htmlOutput("table"), width = 50
        )
      )
      
    )
  )
)




