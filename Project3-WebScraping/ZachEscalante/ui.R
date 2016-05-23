#ui.r
shinyUI(fluidPage(theme = "bootstrap.css", titlePanel("NYC Apartments - Best Value!"), sidebarLayout(
  sidebarPanel(
    h3("Choose Your Neighborhood"),
    selectInput("neighborhood", "I want to live in...", choices = cdshp_nyc$ntaname, selected = "this one"),
    br(),
    textOutput('text1'),
    br(),
    textOutput('text2'),
    br(),
    textOutput('text3'),
    br(),
    textOutput('text4')
  ), 
  mainPanel(
    leafletOutput('mymap', width = 800, height = 700)
  ) )
))
  


