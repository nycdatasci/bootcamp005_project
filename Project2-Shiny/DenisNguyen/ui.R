shinyUI(
  navbarPage(
    title = "Population Projections",
    theme = shinytheme("flatly"),
    tabPanel("Insight: Population",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
            helpText("This graph displays projected population changes within a 
                     certain gender and race/ethnic group."),
            br(),
            selectInput("race", h4("Race:"), choices = race),
            selectInput("sex", h4("Gender:"), choices = sex),
            checkboxGroupInput("population", h4("Population:"), 
                               choices = population, selected = "total population"),
            sliderInput("year", h4("Projection Range:"), sep = "",
                min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
        ),
        mainPanel(style = "margin-left:-50px; margin-top:-100px",
          htmlOutput("plot")
        )
      )
    ),
    tabPanel("Insight: Gender",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
          helpText("This graph displays male and female projected population in
                   a race/ethnic group."),
          br(),
          selectInput("raceG", h4("Race:"), choices = race),
          selectInput("populationG", h4("Population:"), choices = population,
                      selected = "total population"),
          checkboxGroupInput("sexG", h4("Gender:"), 
                             choices = sex, selected = '0'),
          sliderInput("yearG", h4("Projection Range:"), sep = "",
                      min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
          ),
          mainPanel(style = "margin-left:-50px; margin-top:-100px",
            htmlOutput("plotG")
          )
        )
      ),
      tabPanel("Insight: Race",
        sidebarLayout(
          sidebarPanel(h3("Parameters"), style = "width:350px",
            helpText("This graph displays projected population for different
                     races and ethnic groups."),
            br(),
            selectInput("sexR", h4("Gender:"), choices = sex),
            selectInput("populationR", h4("Population:"), choices = population),
            checkboxGroupInput("raceR", h4("Race:"), 
                               choices = race, selected = 0),
            sliderInput("yearR", h4("Projection Range:"), sep = "",
                        min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
          ),
          mainPanel(style = "margin-left:-50px; margin-top:-100px",
            htmlOutput("plotR")
          )
        )
      ),
      tabPanel("Data Table",
        fluidPage(
          p("Data was obtained from ", a("here", href="http://www.census.gov/population/projections/data/"),
            style = "float:right"),
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
