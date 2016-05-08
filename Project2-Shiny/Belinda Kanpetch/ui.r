#ui.r
library(shiny)
library(leaflet)
library(CartoDB)
library(RColorBrewer)
library(dplyr)
library(markdown)
library(ggplot2)
library(DT)

shinyUI(navbarPage(
  "New York City Street Trees",
  theme = shinytheme('cosmo'),
  tabPanel("Map the trees",
           fluidRow(
             column(4, offset = 4,
                    selectInput('in_sel_species', 'Select Species',
                                unique(as.character( all_trees$new_species)),
                                multiple=TRUE, selectize=TRUE)
             ),
             column(4,
                    selectInput('in_sel_borough', 'Select Boroughs',
                                unique(as.character(all_trees$borough)),
                                multiple=TRUE, selectize=TRUE)
             )
           ),
           br(),
           fluidRow(
             leafletOutput("mymap", width = "100%", height = 700)
           )
  ),
  
  
  tabPanel("Visualize Data",
           fluidRow(
             column(4,
                    selectizeInput('input_xaxis', 'Distribution of:',
                                   c('Species', 'Borough',
                                     'Community_District',
                                     'Tree_Condition'))
             ),
             column(4,
                    selectizeInput('scale', 'by:',
                                   c('None','Species', 'Borough',
                                     'Community_District',
                                     'Tree_Condition'))
             ),
             column(4,
                    checkboxInput('sel_normalize', label = 'Normalize',
                                  value = F)
             )
           ),
           br(),
           fluidRow(
             plotOutput("graph", width = "100%", height = 700)
             
           )
           
  ),
  
  
  tabPanel("Data",
           fluidRow(
             column(3,
                    selectInput('data_species', 'Select Species',
                                unique(c('All', as.character( all_trees$new_species)))
                    )
             ),
             column(3,
                    selectInput('data_borough', 'Select Boroughs',
                                unique(c('All', as.character(all_trees$borough)))
                    )
             ),
             column(3, 
                    selectInput('data_commdist', 'Select Community District',
                                unique(c('All', as.character(all_trees$commdist)))
                    )
             ),
             column(3, 
                    selectInput('data_condition', 'Select Tree Condition',
                                unique(c('All', as.character(all_trees$treecondit)))
                    )
             )
           ),
           br(),
           fluidRow(
             DT::dataTableOutput('table')
           )
  ),
  
  tabPanel("About",
          
             fluidRow(
               column(9, offset = 2,
               img(src = 'pyrus_nyc.jpg', height = 400)
               )
             ),
             fluidRow(
               column(9, offset = 2,
               h3('Why Street Trees?'),
               p('The New York City street tree can sometimes be taken for granted or
                go unnoticed. Located along paths of travel they stand steady and patient;
                quietly going about their business of filtering out pollutants in our air,
                bringing us oxygen, providing shade during the warmer months, blocking winds
                during cold seasons, and relieving our sewer systems during heavy rainfall.
                All of this while beautifying our streets and neighborhoods. Some recent
                studies have found a link between presence of streets and lower stress
                levels in urban citizens.'),
               p('So what makes a street tree different from any other tree? Mainly its
                  location. A street tree is defined as any tree that lives within the
                  public right of way; not in a park or on private property. Although they
                  reside in the public right of way (or within the jurisdiction of The
                  Department of Transportation) they are the property of and cared for by
                  the NYC Department of Parks and Recreation.'),
               p('With the intent to understand the data and explore what the data was
                telling me I started with some very basic questions:'),
               p('- How many street trees are there in Manhattan?'), 
               p('- How many different species are there?'),
               p('- What is the general condition of the street trees?'),
               p('- What is the distribution of species by community district?'),
               p('- Is there a connection between median income of a
                community district to the number of street trees?'),
               br(),
               h3('The Data:'),
               p('The dataset used for this exploratory visualization was downloaded from the
             NYC Open Data Portal and was collected as part of TreeCount!2005, a street
             tree census maintained by the NYC Department of Park and Recreation. The first
             census count was 1995 and has been conducted every 10 years by trained volunteers. '),
               p('Some challenges with this dataset involved missing values in the form of
            unidentifiable species types. There were 2285 observations with unclassifiable
            species type, 487 observations that had unclassifiable community districts,
            geographic information (longitude and latitude) were character strings that had to be
            split into different variables, and species codes were given by 4 letter characters
            without any reference to genus, species, or cultivar and I had to find another dataset
            to decipher that code.')
             ) #close mainpanel
             )
         
  ) #close tabpanel
  
) #close navebarpage
) #close shinyui
