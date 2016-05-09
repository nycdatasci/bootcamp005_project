shinyUI(
  navbarPage(
    title = "Population Projections",
    theme = shinytheme("flatly"),
    tabPanel("Insight: Big Picture",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
            helpText("Customize your graph with the options below."),
            br(),
            selectInput("race", h4("Race:"), choices = race),
            selectInput("sex", h4("Gender:"), choices = sex),
            checkboxGroupInput("population", h4("Population:"), 
                               choices = population, selected = "total population"),
            sliderInput("year", h4("Projection Range:"), sep = "",
                min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
        ),
        mainPanel(style = "margin-left:-100px; margin-top:-100px",
          htmlOutput("plot")
        )
      )
    ),
    tabPanel("Insight: Gender",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
          helpText("Customize your graph with the options below."),
          br(),
          selectInput("raceG", h4("Race:"), choices = race),
          selectInput("populationG", h4("Population:"), choices = population,
                      selected = "total population"),
          checkboxGroupInput("sexG", h4("Gender:"), 
                             choices = sex, selected = '0'),
          sliderInput("yearG", h4("Projection Range:"), sep = "",
                      min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
          ),
          mainPanel(style = "margin-left:-100px; margin-top:-100px",
            htmlOutput("plotG")
          )
        )
      ),
      tabPanel("Insight: Race",
        sidebarLayout(
          sidebarPanel(h3("Parameters"), style = "width:350px",
            helpText("Customize your graph with the options below."),
            br(),
            selectInput("sexR", h4("Gender:"), choices = sex),
            selectInput("populationR", h4("Population:"), choices = population),
            checkboxGroupInput("raceR", h4("Race:"), 
                               choices = race, selected = 0),
            sliderInput("yearR", h4("Projection Range:"), sep = "",
                        min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
          ),
          mainPanel(style = "margin-left:-100px; margin-top:-100px",
            htmlOutput("plotR")
          )
        )
      ),
#      tabPanel("Summary",
#        verbatimTextOutput("summary")
#      ),
      tabPanel("Table",
        fluidPage(
          column(3, selectInput("racet", h4("Select Race:"), choices = race)),
          column(3, selectInput("sext", h4("Select Gender:"), choices = sex))
      ),
      hr(),
      fluidPage(
        DT::dataTableOutput("data2014")
      )
    )
  )
)
