#####By Ruonan Ding#########
############################

library(shiny)
library(shinydashboard)
library(shinythemes)

shinyUI(navbarPage("Panama Papers Key Data Figures",   id = "nav",
                   theme = shinytheme("flatly"),
  tabPanel("Data Introduction",
           fluidRow(
             box(
               title = h3("Data Source"),
               textOutput("dataintro"),
               width = 12
             ),
             br(),
             br(),
             box(
               title= h3("ICIJ Disclaimer"),
               textOutput("disclaimer"),
               width = 12
             ),
             br(),
             box(title = "Term Definition:"),
             box(
               title= h4("Offshore Entity:"),
               textOutput("entites"),
               width = 12
             ),
             box(
               title= h4("Intermediary :"),
               textOutput("intermediary"),
               width = 12
             ),
             box(
               title= h4("Tax Status :"),
               textOutput("jurisdiction"),
               width = 12
             )
           )),
           
  tabPanel("Where does the US money go?",
        fluidRow(
          box(
            title = "The Intermediaries Countries that faciliated US Entities",
            htmlOutput("GeoLayer1"),
            width = 12
          ),
          box(            
            title = "The Final Jurisdition Countries of US Entities", 
            htmlOutput("GeoLayer2"),
            width = 12
          )
          )
        ),
  
  tabPanel("Entities Distribution By Location",
           fluidRow(
             box(     
               title = "Entities Distribution By Location",
               htmlOutput("GeoVis"),
               width = 12
             ),
             box(
               title = "Number of Entities Rank by Country",
               htmlOutput("PopTable"),
               width = 12
             ))),
  
  tabPanel("Entities Establishment Over Time",
        fluidRow(
          box(
            title = "Number of Entities Over Time",
            plotOutput("timeline_overview"),
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Selected Jurisdiction", status = "info", 
            selectInput("jurisdiction", "Jurisdiction :", jurisdiction.options , selected = "PMA"),
            width = 2
          ),
         box(
           title = "Number of Entities by Jurisdiction Over Time",
           plotOutput('timeline_adjustment'),
           width = 10
         )
      )),
    
       tabPanel("Entities Status",
            fluidRow(
              box(
                  title = "Entities Status As of 2015",
                  htmlOutput("statustable"))
            )),
        tabPanel("About Me",
                     fluidRow(
                       box(
                         title = h2("Ruonan Ding"),
                         br(),
                         htmlOutput("aboutme"),
                         width = 12)
                       )
                     )
          )
        )



