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
                               choices = population, selected = "All"),
            sliderInput("year", h4("Projection Range:"), sep = "",
                min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
        ),
        mainPanel(
          htmlOutput("plot")
        )
      )
    ),
    tabPanel("Insight: Gender",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
          helpText("Customize your graph with the options below."),
          br(),
          selectInput("raceG", h4("Select Race:"), choices = race),
          selectInput("populationG", h4("Population:"), 
                               choices = population, selected = "All"),
          checkboxGroupInput("sexG", h4("Gender:"), 
                             choices = sex, selected = "All"),
          sliderInput("yearG", h4("Projection Range:"), sep = "",
                      min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
          ),
          mainPanel(
            htmlOutput("plotG", style="margin-left:-160px")
          )
        )
      ),
      tabPanel("Insight: Race",
        sidebarLayout(
          sidebarPanel(h3("Parameters"), style = "width:350px",
            helpText("Customize your graph with the options below."),
            br(),
            selectInput("sexR", h4("Select Gender:"), choices = sex),
            selectInput("populationR", h4("Population:"), 
                        choices = population, selected = "All"),
            checkboxGroupInput("raceR", h4("Race:"), 
                               choices = race, selected = "All"),
            sliderInput("yearR", h4("Projection Range:"), sep = "",
                        min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
          ),
          mainPanel(
            htmlOutput("plotR", style="margin-left:-160px")
          )
        )
      ),
      tabPanel("Summary",
        verbatimTextOutput("summary")
      ),
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
