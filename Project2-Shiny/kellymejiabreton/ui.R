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
                        
                        title=h1("NYC Complaints by Borough, Jan. 2010 to Apr. 2016 (Sample Data)", align="center", style = "color: Darkblue")),
                
                tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
        
                
                # Generate a row with a sidebar
                selectInput(inputId = "boro",
                            label = "Select a Borough:",
                            choices = c("BROOKLYN", "QUEENS", "MANHATTAN", "BRONX", "STATEN ISLAND"),
                            selected = "BROOKLYN"),
        
                   
                        # 
                        mainPanel(
                                
                                tags$head(
                                        tags$style(type='text/css', 
                                                   ".nav-tabs {font-size: 20px} ")), 
                                
                                
                                tabsetPanel(
                                        
                                        tabPanel("Welcome",
                                                 
                                                 #column(6, offset = 3,
                                                        p(h2(
                                                        
                                                        br(),


                                                        "

                                                         New York City...", style = "font-family: 'times'; font-si16pt; color: Darkgreen"),
                                                          
                                                          br(),
                                                          
                                                          h3(em("

                
                                                          'If I can make it there, I'll make it anywhere.' ~ Frank Sinatra
                                                          "), style = "font-family: 'times'; font-si16pt"),
                                                          
                                                          br(),
                                                          
                                                          h3("
                                                          
                                                          When I think of New York City I think of noise, sirens, taxis, the constant battle of suitable heat with the landlord,
                                                             and the scent of a sanitation truck on a hot summer day.  There is no place like home.", style = "font-family: 'times'; font-si16pt"
                                                             ),
                                                          
                                                          h3(
                                                        "Using the 311 dataset provided by NYC Open Data I crafted an interactive user application to assist 
                                                          residents and visitors who are in search of an area with an appropriate sound and style to their 
                                                          liking.  NYC Complaints by Borough (N.C.B.) is a quick reference tool to aid users with their planning, grounded 
                                                          on the complaints reported in NYC. 

                                                          
                                                          ", style = "font-family: 'times'; font-si16pt")
                                                          
                                                          
                                                          
                                                 )
                                                 
                                                 
                                                 
                                                 
                                                 
                                                 ),
                                        
                                        tabPanel("About the Dataset", 
                                                 
                                                 p(h2(
                                                      br(),
                                                         
                                                        "
                                                      
                                                      The Dataset", style = "font-family: 'times'; font-si16pt; color: Darkgreen"),

                                                      h3(
                                                        "The dataset is a daily collection of complaints made directly to 311 by the public.  311 is a NYC customer service center, 
                                                        launched by Mayor Michael Bloomberg in March 2003. Providing the public with easy access to NYC government services, while 
                                                        assisting agency improvement through consistent measurements.  311 is a huge success, with annual year-on-year usage growth 
                                                        since its launch.  Here are some facts on 311:
                                                        ", style = "font-family: 'times'; font-si16pt"),
                                                      
                                                   h3("
                                                      - Open 24 hours a day, 365 days’ year", style = "font-family: 'times'; font-si16pt"),
                                                   
                                                   h3("
                                                      - Access to 180 languages", style = "font-family: 'times'; font-si16pt"),
                                                   
                                                   h3("
                                                      - Receive 51,000 calls per day on average", style = "font-family: 'times'; font-si16pt"),
                                                   
                                                   h3("
                                                      - Annual call volumes to 911 have decreased since the inception of 311", style = "font-family: 'times'; font-si16pt"),
                                                   h3("

                                                      - 311 online was launched in March 2009", style = "font-family: 'times'; font-si16pt")
        
                                                 )
                                                 
                                                 
                                                 ),
                                        
                                        tabPanel("Complaints", plotOutput(outputId = "wordCloud1", height = "700", width="1500")),
                                                 tabPanel("Frequency", plotOutput(outputId = "barPlot", height = "600", width="1300"),
                                                 
                                                 selectInput(inputId = "Fre_q",
                                                             label = "Select the Frequency:",
                                                             choices = c("ANNUAL", "MONTH", "DAY"),
                                                             selected = "MONTH")),
                                        
                                        tabPanel("Top Ten Complaints", dataTableOutput("table"), height = "1500", width="1500"),
                                        tabPanel("Map", leafletOutput(outputId="map", height = "900", width="1500"))
                                        
                                        #tabPanel("Next Steps",
                                                 
                                                 
                                                 #p(h2(
                                                         
                                                      #   br(),
                                                         
                                                         
                                                     #    "
                                                    #     Next steps", style = "font-family: 'times'; font-si16pt; color: Darkgreen"),
                                                         
                                                   #h3("

                                                  #    Allow the user to sort by:
                                                 #     Complaint type 
                                                 #     Specific neighborhoods 
                                                #      Time period
                                               #       Option to enter longitude and latitude coordinates
                                              #        Automate application to update live with the latest data available
                                               #       Upload the full data set to the application.", style = "font-family: 'times'; font-si16pt")
                                                   
                                                   
                                                   
                                                   
                                                #   )
                                                 
                                                 
                                                 
                                                 
                                                 
                                                  # )
                                        
                                        
                                )
                                )
                        )
                        
                )


