library(shiny)
library(dplyr)
library(ggplot2)
library(countrycode)
library(googleVis)
library(shinydashboard)
library(shinythemes)
library(plotly)


shinyUI(navbarPage("Airbnb",   id = "nav",
         theme = shinytheme("flatly"),
   
         tabPanel("Map",
           fluidRow(
             box( 
              plotlyOutput('plot'),
              width = 12
             )
           )
         ),
         tabPanel("About the Data",
                  fluidRow(
                    HTML('This data was taken from an <a href="https://www.kaggle.com/c/airbnb-recruiting-new-user-bookings">Airbnb Kaggle competition</a>.
                         The training set consisted of over 200,000 users over the time period of Jan 2010 - Jun 2014.')
                  )
         ),
         tabPanel("EDA",
                  fluidRow(
                    plotOutput('plot1'),
                    plotOutput('plot2'),
                    plotOutput('plot3'),
                    plotOutput('plot4'),
                    plotOutput('plot5'),
                    plotOutput('plot6'),
                    plotOutput('plot7'),
                    plotOutput('plot8'),
                    plotOutput('plot9'),
                    plotOutput('plot10'),
                    plotOutput('plot11'),
                    plotOutput('plot12'),
                    img(src='download.png', align = "center")
                  )
         ),
         tabPanel("About Us",
            fluidRow(
              box(
                title = h2("Rob Castellano, Zi Jin, Yannick Kimmel, Michael Winfield"),
                br(),
                htmlOutput("aboutus"),
                width = 12)
            )
         )
      )
)


