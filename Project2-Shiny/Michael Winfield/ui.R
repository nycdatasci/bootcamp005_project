library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  #"Temperature" = "Temp1",
  "Rain" = "Rain",
  "Fog" = "Fog",
  "Storms" = "Storms"
)

vm <- sort(as.numeric(paste(unique(as.character(viz$Month)))))
  
shinyUI(
  navbarPage(
    title = "Climate Data for the World's Oceans (1750-1850)",
    id = "nav",
                   
                   tabPanel("Storm, Rain, and Fog Measurements",
                            div(class="outer",
                                
                                tags$head(
                                  # Custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 330, height = "auto",
                                              
                                              h2("Would you like to visualize temperature, rain locations, storm locations, or fog sightings?"),
                                              
                                              selectInput("variable", "Weather Event:", vars)))),
                  tabPanel("Slider Barplot",
                            fluidRow(
                              column(3,
                                    absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                      draggable = TRUE, top = 700, left ="auto" , right =20 , bottom = "auto",
                                      width = 700, height = "auto",
                                      plotOutput("MonthPlot",height = 800),
                                      sliderInput("Month", "Month:", 
                                                  min=min(vm),
                                                  max=max(vm),
                                                  value=min(vm),
                                                  format="####",
                                                  animate=TRUE))))),
                  tabPanel("Scatters and Bars",
                            fluidRow(
                            column(2,
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                    draggable = TRUE, top = 500, left ="auto" , right =20 , bottom = "auto",
                                    width = 700, height = "auto",
                                    plotOutput("scatterlong",height = 400),
                                    plotOutput("scatterlat",height = 400)),
                              column(4,
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                    draggable = TRUE, top = 500, left ="auto" , right =20 , bottom = "auto",
                                    width = 700, height = "auto",
                                    plotOutput("avgtempyear",height = 700)),
                              column(9,
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                    draggable = TRUE, top = 500, left ="auto" , right =20 , bottom = "auto",
                                    width = 700, height = "auto",
                                    plotOutput("varintr", height = 700),
                                    plotOutput("varmonth", height = 700))))))),
                  tabPanel("Multiple Linear Regression",
                           fluidRow(
                           column(2,
                                  img(src = "Model-1.png", height =700,weight =300)),
                           column(3, 
                                  img(src = "Model-2.png", height =700,weight =300)),
                           column(3, 
                                  img(src = "Model-3.png", height =700,weight =300)),
                           column(4,
                                  img(src = "Model-4.png", height =700,weight =300),
                                  h2("model <- lm(Temp1 ~ Lat+Long+Year+Month+Rain+Storms+Fog, data = viz)")
                                  )))
    ))