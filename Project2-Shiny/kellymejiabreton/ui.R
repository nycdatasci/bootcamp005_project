        #ui.R

library(shiny)
library(leaflet)
#library(shinydashboard)
# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).


# Define the overall UI
shinyUI(
        
        # Use a fluid Bootstrap layout
        bootstrapPage(
                
                # Give the page a title
                titlePanel(
                        
                        title=h1("NYC Complaints by Borough, Jan. 2010 to Apr. 2016", align="center")),
        
                
                # Generate a row with a sidebar
                selectInput(inputId = "boro",
                            label = "Select a Borough:",
                            choices = c("BROOKLYN", "QUEENS", "MANHATTAN", "BRONX", "STATEN ISLAND"),
                            selected = "BROOKLYN"),
        
                   
                        # 
                        mainPanel(
                                tabsetPanel(
                                        tabPanel("Complaints", plotOutput(outputId = "wordCloud1", width="140%", height = "500px")),
                                                 tabPanel("Frequency", plotOutput(outputId = "barPlot", height = "300px"),
                                                 
                                                 selectInput(inputId = "Fre_q",
                                                             label = "Select the Frequency:",
                                                             choices = c("ANNUAL", "MONTH", "DAY"),
                                                             selected = "MONTH")),
                                        
                                        tabPanel("Top Ten Complaints", tableOutput("table")),
                                        tabPanel("Map", leafletOutput(outputId="map", width = "100%", height=400))
                                        
                                )
                                )
                        )
                        
                )


