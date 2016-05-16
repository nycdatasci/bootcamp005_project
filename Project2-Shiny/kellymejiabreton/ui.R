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
                        
                        title=h1("NYC Complaints by Borough, Jan. 2010 to Apr. 2016 (Sample Data)", align="center")),
        
                
                # Generate a row with a sidebar
                selectInput(inputId = "boro",
                            label = "Select a Borough:",
                            choices = c("BROOKLYN", "QUEENS", "MANHATTAN", "BRONX", "STATEN ISLAND"),
                            selected = "BROOKLYN"),
        
                   
                        # 
                        mainPanel(
                                tabsetPanel(
                                        
                                        tabPanel("Welcome",
                                                 
                                                 #column(6, offset = 3,
                                                        p(h2("



                                                         New York City!"),
                                                          
                                                          
                                                          h4("

                
                                                          'If I can make it there, I'll make it anywhere.' Frank Sinatra
                                                          "),
                                                          
                                                          
                                                          h3("
                                                          
                                                          When I think of New York City I think of noise, sirens, taxis, the constant battle of suitable heat with the landlord,
                                                             and the scent of a sanitation truck on a hot summer day.  It's home sweet home."
                                                             ),
                                                          
                                                          h3(
                                                        "Using the 311 dataset provided by NYC Open Data I crafted an interactive user application to assist 
                                                          potential/current NYC residents/visitors who are in search of an area with an appropriate sound and style to their 
                                                          liking.  NYC Complaints by Borough (N.C.B.) is a quick reference tool to aid users with their planning, grounded 
                                                          on the complaints reported in a specific borough or neighborhood. 

                                                          
                                                          ")
                                                          
                                                          
                                                          
                                                 )
                                                 
                                                 ),
                                        
                                        tabPanel("About the Dataset", 
                                                 
                                                 p(h2("
                                                      
                                                      
                                                      The Dataset"),

                                                      h3(
                                                        "The dataset is a daily collection of complaints made directly to 311 by the public.  311 is a NYC customer service center, 
                                                        launched by Mayor Michael Bloomberg in March 2003. Providing the public with easy access to NYC government services, while 
                                                        assisting agency improvement through consistent measurements.  311 is a huge success, with annual year-on-year usage growth 
                                                        since its launch.  Here are some facts on 311:
                                                        "),
                                                      
                                                   h3("
                                                      - Open 24 hours a day, 365 days’ year"),
                                                   
                                                   h3("
                                                      - Access to 180 languages"),
                                                   
                                                   h3("
                                                      - Receive 51,000 calls per day on average"),
                                                   
                                                   h3("
                                                      - Annual call volumes to 911 have decreased since the inception of 311"),
                                                   h3("

                                                      - 311 online was launched in March 2009")
                                                      
                                                      
                                                     
                                                         
                                                         
                                                         
                                                         
                                                 
                                                         
                                                         
                                                         
                                                 )
                                                 
                                                 
                                                 ),
                                        
                                        tabPanel("Complaints", plotOutput(outputId = "wordCloud1", height = "900", width="1500")),
                                                 tabPanel("Frequency", plotOutput(outputId = "barPlot", height = "700", width="1500"),
                                                 
                                                 selectInput(inputId = "Fre_q",
                                                             label = "Select the Frequency:",
                                                             choices = c("ANNUAL", "MONTH", "DAY"),
                                                             selected = "MONTH")),
                                        
                                        tabPanel("Top Ten Complaints", tableOutput("table"), height = "1500", width="1500"),
                                        tabPanel("Map", leafletOutput(outputId="map", height = "900", width="1500"))
                                        
                                )
                                )
                        )
                        
                )


