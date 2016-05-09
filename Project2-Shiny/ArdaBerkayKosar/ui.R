# Choices for drop-downs
vars_borough <- c(unique(NYPD_sample$Borough))
vars_offense <- c(unique(NYPD_sample$Offense))



shinyUI(navbarPage("7 Major Felonies in NYC", id="nyc", theme = shinytheme("journal"),
    
        tabPanel("Map",
            div(class = "outer",

                tags$head(
                  includeCSS("style.css"),
                  includeScript("gomap.js")
                ),
                
                leafletOutput("map", width="100%", height=800),
                
                absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 330, height = "auto",
                              
                              hr(),

                              h2("Felonies in NYC"),

                              checkboxGroupInput("borough", h4("Please select one borough"), vars_borough),
                              checkboxGroupInput("offense", h4("Please select offense type"), vars_offense)
                              
                ) #End of absolutepanel
            )#End of div
        ), # End of map tab panel
        
        tabPanel("Graphical Exploration",
            
            fluidRow(
              
              titlePanel("Felonies in NYC Graphical Exploration")
            ),
            
            fluidRow(
              plotOutput("barchart1", width = "100%", height = 600)
              
            ),
            
            hr(),
            
            fluidRow(
                 
                 column(3,
                        selectizeInput("x","Select X axis:", c("None" ,"Offense", "Borough", "Year"))
                        ),
                 
                 column(3,
                        selectInput('y', 'Select Y axis:',
                                    c("None","Count", "Normalized"))
                 ),

                 column(3,
                        selectInput('facet_row', 'Facet Row by:',
                                    c(None='.', c("Offense", "Borough", "Year")))
                        ),

                 column(3,
                        selectInput('facet_col', 'Facet Column by:',
                                    c(None='.', c("Offense", "Borough", "Year")))
                        )
                 
            ) #End of fluidRow
          
        ), #End of Data Exploration tabPanel
        
        
        
        
      tabPanel("Data Table",
               
               fluidRow(
                         column(4,
                                selectInput("off",
                                            "Offense",
                                            c("All",unique(as.character(NYPD_sample$Offense))))),

                         column(4,
                                selectInput("bor",
                                            "Borough",
                                            c("All", unique(as.character(NYPD_sample$Borough))))),
              
                        column(4,
                               selectInput("c","Year", c("All", c(2006:2015))))
                 
               ), #End of fluidRow
            
              fluidRow(
                DT::dataTableOutput("data_table")
              )
      ) #End of tabPanel Data Table
)) # End of ShinyUI
      
              
                   

                   
                   
                   
                   