# COOPER HEWITT MUSEUM PROJECT
#ui.r
library(shiny)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

shinyUI(navbarPage(
  "Exploring the Collection at The Cooper Hewitt Design Museum",
  theme = shinytheme('cosmo'),
  tabPanel("Get Recommendations",
           # fluid row user input for filtering dataset
           fluidRow(
             column(4,
                    selectizeInput('input_decade', 'Select a decade',
                                   sort(unique(as.character(objects_info$decade)), decreasing=TRUE),
                                   multiple = FALSE,
                                   selected = NULL,
                                   options = list(
                                     placeholder = 'Please select an option below',
                                     onInitialize = I('function() { this.setValue(""); }')
                                   ))
             ),
             column(4,
                    selectizeInput('input_country', 'Select a country',
                                   sort(unique(as.character(objects_info$woe.country_name)), decreasing=FALSE),
                                   multiple = FALSE,
                                   selected = NULL,
                                   options = list(
                                     placeholder = 'Please select an option below',
                                     onInitialize = I('function() { this.setValue(""); }')
                                   ))
             ),
             column(4,
                    actionButton("recommend", label = "get recommendations"))
           ),
           # fluid row for codestart images
           fluidRow(
             column(2,offset = 1,
                    htmlOutput("picture1"),
                    radioButtons("radio1", label = "",
                                 choices = list("like" = 1, "eh" = 0), 
                                 selected = 0)
             ),
             column(2,
                    htmlOutput("picture2"),
                    radioButtons("radio2", label = "",
                                 choices = list("like" = 1, "eh" = 0), 
                                 selected = 0)),
             column(2,
                    htmlOutput("picture3"),
                    radioButtons("radio3", label = "",
                                 choices = list("like" = 1, "eh" = 0), 
                                 selected = 0)),
             column(2,
                    htmlOutput("picture4"),
                    radioButtons("radio4", label = "",
                                 choices = list("like" = 1, "eh" = 0), 
                                 selected = 0)),
             column(2,
                    htmlOutput("picture5"),
                    radioButtons("radio5", label = "",
                                 choices = list("like" = 1, "eh" = 0), 
                                 selected = 0))
           ),
           
           fluidRow(
             column(12,
                    h3('Recommendations:')
                    )
             ),
           
           # fluid fow for recommendation images
           fluidRow(
             column(2,offset = 1,
                    htmlOutput("rec_pic1")
             ),
             column(2,
                    htmlOutput('rec_pic2')
             ),
             column(2,
                    htmlOutput('rec_pic3')
             ),
             column(2,
                    htmlOutput('rec_pic4')
             ),
             column(2,
                    htmlOutput('rec_pic5')
             )
           )
  ),
  
  tabPanel("About",
          
           fluidRow(
             column(9, offset = 2,
                    h3('About the project'),
                    p('For my capstone project I wanted to create a recommendation system for
                      the Cooper Hewitt Design Museum that enhanced the already robust and
                      innovative system they have developed. More to come....'),
                    h3('About the data'),
                    p('The data was downloaded from the Cooper Hewitt Github and collected
                      through their API. More detils to come....')
                   
             ) #close mainpanel
           )
           
  ) #close tabpanel
  
) #close navebarpage
)#close shinyui
