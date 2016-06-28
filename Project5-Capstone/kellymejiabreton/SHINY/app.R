## app.R ##
library(shiny)
## app.R ##
library(shinydashboard)

ui <- dashboardPage(
        dashboardHeader(title = "Predict the Oscars"),
        dashboardSidebar(),
        dashboardBody(
                # Boxes need to be put in a row (or column)
                fluidRow(
                        column(3, wellPanel(
                                textInput("title", "Movie Title:", "title here"),
                                
                                numericInput("obs1", "Box Office Receipts ($2015 Million USD):", 300,
                                             min = 100, max = 4000),
                                
                                numericInput("obs2", "Movie Length (minutes):", 110,
                                             min = 27, max = 201),
                                
                                numericInput("obs3", "Rotten Tomatoes Audience Freshness:", 75,
                                             min = 24, max = 100),
                                
                                numericInput("obs4", "IMDb Rating:", 7.0,
                                             min = 4.0, max = 10.0),
                                
                                numericInput("obs5", "Rank in Year:", 6.0,
                                             min = 1.0, max = 10.0),
                                
                                selectizeInput("variable", "MPAA Rating:",
                                               c("PG.13", "PG", "R", "G")),
                                
                                selectizeInput("variable", "Genre:",
                                            c("Thriller", "Comedy", "Fantasy","Sci.Fi",
                                              "Romance", "Drama", "Family", "Crime",
                                              "Adventure","War", "Mystery", "Sport",
                                              "Horror", "Animation", "Music", "History",
                                              "Action", "Western",
                                              "Musical", "Biography"),
                                            options = list(maxItems = 3, placeholder = 'hi there')),

                                submitButton("Submit")
                        )),
                        
                               titlePanel(" AND THE OSCAR GOES TO ..."),
                        
                        mainPanel(
                                img(src="Oscars_Statue.png", height = 300, width = 200)
                        ),
                        mainPanel(
                                verbatimTextOutput("text")
                        ),
                        
                        conditionalPanel(
                                condition="output.forecast==0",
                                verbatimTextOutput("f0")
                        ),
                        
                        conditionalPanel(
                                condition="output.forecast==1",
                                verbatimTextOutput("f1")
                        )
                )
        )
)

server <- function(input, output) {
        
        output$text <- renderText({
                paste(input$title)
        })
        
        output$forecast <- renderText({
                paste(input$title)
        })
        
        output$f0 <- renderText({
                paste(input$title, " is not projected to win an Oscar. Try another film!")
        })
        
        output$f1 <- renderText({
                paste(input$title)
        })
        
        }


shinyApp(ui, server)