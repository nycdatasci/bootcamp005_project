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
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    ),
    
    sidebarSearchForm(textId = "searchText", 
                      buttonId = "searchButton",
                      label = "Search...")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map", #Charts
        fluidRow(
          box(
            title = "Select Glacier", status = "info", 
            solidHeader = TRUE, collapsible = TRUE,
            
            selectizeInput("selected",
                         "Select Glacier to Display",
                         glac.choice)), #comma #add more later!
          
          box(
            title = "Glacier map", status = "success", 
            solidHeader = TRUE, collapsible = TRUE,
           #htmlOutput("Glacier")
           uiOutput("plot1_ui"),
           ggvisOutput("plot1")
           #ggvisOutput(plot_id = rand_id("plot_id"))
            )
        ),
        
        fluidRow(
          box(
            title = "Time Series Chart", status = "primary",
            solidHeader = TRUE,collapsible = TRUE,
            htmlOutput("LineChart"),
            width = 12) 
        ),
        
        fluidRow(
        box(
          "Year", br(), "Select",
          sliderInput("slider", "Slider input:", 2000, 2014, 1)
        )
      )
      
      #
      #tabItem(tabName = "data",
        #fluidRow(
          #box(htmlOutput("table"))
        #)
      #)
    )
  ))
))
