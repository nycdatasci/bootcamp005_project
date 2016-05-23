shinyUI(
  navbarPage(
    title = "U.S. Population Projections",
    theme = shinytheme("flatly"),
    
### Welcome tab with important information
    tabPanel("Welcome",
      mainPanel(
        div(style = "margin-left: 15%;",
          br(),
          br(),
          h3(strong("Welcome to the Population Projections Shiny App"), style = "margin-top:0"),
          br(),
          p("The following app graphs projections of population and population change, 
            breaking down information by gender and race/ethnicity."),
          br(),
          h4(strong("What is a census?")),
          p("A census is the procedure of acquiring information from a population in order to 
            estimate its population, population change and makeup (age, race/ethnicity, gender). 
            The US conducts its census every 10 years, releasing the results approximately 5 years 
            after. The latest US Census was performed in 2010 and the next one is scheduled in 2020."),
          br(),
          h4(strong("Where was the data obtained?")),
          p("The data was obtained from the", a("United States Census Bureau.", 
                                                href="http://www.census.gov/population/projections/")),
          br(),
          h4(strong("How are projections made?")),
          p("Projections were produced using a cohort-component method and these are updated periodically 
            to incorporate revised assumptions about anticipated trends. More information can be found",
            a("here.", href="http://www.census.gov/population/projections/methodology/")),
          br(),
          h4(strong("This is boring.")),
          p("Great, click on another tab and use the Shiny App to inspect these projections. Try to 
            uncover interesting trends."),
          br(),
          p(strong("Enjoy your visit!"))
        )
      )
    ),
    
### Tab allowing insight into birth/death/migration changes in population
    tabPanel("Insight: Population",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
          helpText("This graph displays projected population changes within a 
                   certain gender and race/ethnic group. Select multiple options 
                   under population change to compare them."),
          br(),
          selectInput("race", h4("Race/Ethnicity:"), choices = race),
          selectInput("sex", h4("Gender:"), choices = sex),
          checkboxGroupInput("population", h4("Population Change:"), 
                             choices = population, selected = "total population"),
          sliderInput("year", h4("Years of Interest:"), sep = "",
                      min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
        ),
        mainPanel(style="position:relative",
          h2("Projected Population", style="margin:20px; margin-left:200px; z-index:100; position:absolute"),
          htmlOutput("plot", style="margin-left:-100px; margin-top:-80px")
        )
      )
    ),
    
### Tab allowing insight into gender changes in population
    tabPanel("Insight: Gender",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
          helpText("This graph displays male and female projected population in
                   a race/ethnic group. Select multiple options under gender to 
                   compare them."),
          br(),
          selectInput("raceG", h4("Race/Ethnicity:"), choices = race),
          selectInput("populationG", h4("Population Change:"), choices = population,
                      selected = "total population"),
          checkboxGroupInput("sexG", h4("Gender:"), 
                             choices = sex, selected = '0'),
          sliderInput("yearG", h4("Years of Interest:"), sep = "",
                      min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
        ),
        mainPanel(style="position:relative",
          h2("Projected Population", style="margin:20px; margin-left:200px; z-index:100; position:absolute"),
          htmlOutput("plotG", style="margin-left:-100px; margin-top:-80px")
        )
      )
    ),
    
### Tab allowing insight into race/ethnicity changes in population
    tabPanel("Insight: Race",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
          helpText("This graph displays projected population for different
                   races and ethnic groups. Select multiple options under 
                   race/ethnicity to compare them."),
          br(),
          selectInput("sexR", h4("Gender:"), choices = sex),
          selectInput("populationR", h4("Population Change:"), choices = population),
          checkboxGroupInput("raceR", h4("Race/Ethnicity:"), choices = race, selected = 0),
          sliderInput("yearR", h4("Years of Interest:"), sep = "",
                      min = 2014, max = 2060, value = c(2014, 2060), ticks = FALSE)
        ),
        mainPanel(style="position:relative",
          h2("Projected Population", style="margin:20px; margin-left:200px;
              z-index:100; position:absolute"),
          htmlOutput("plotR", style="margin-left:-100px; margin-top:-80px")
        )
      )
    ),
    
### Tab showing changes in population per state
    tabPanel("Map",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
          helpText("This graph displays population change between two dates. 
                   Hover over each state to get more detailed information."),
          br(),
          selectInput("changeS", h4("Calculation:"), choices = change),
          br(),
          sliderInput("rangeS", h4("Years of Interest:"), sep = "",
                      min = 1995, max = 2025, value = c(1995, 2025), step = 10)
        ),
        mainPanel(style="position:relative",
          h2(textOutput("myrange"), style="margin:20px; z-index:100; margin-left:80px; position:absolute"),
          htmlOutput("plotS", style="margin:-15px; margin-left:-50px; padding:0")
        )
      )
    ),
    
### Tab showing changes in population projections
    tabPanel("Projections",
      sidebarLayout(
        sidebarPanel(h3("Parameters"), style = "width:350px",
          helpText("This graph displays total population projections made by the US Census Bureau 
                   in different years. Select multiple options under projections to compare different 
                   projections."),
          br(),
          checkboxGroupInput("projectionP", h4("Projections:"), choices = 
                              list("2008" = "2008", "2012" = "2012", "2014" = "2014"), selected = '2014'),
          br(),
          sliderInput("yearP", h4("Years of Interest:"), sep = "",
                      min = 2010, max = 2060, value = c(2010, 2060), ticks = FALSE)
          ),
        mainPanel(style="position:relative",
                  h2("Population Projections", style="margin:20px; margin-left:200px;  z-index:100; position:absolute"),
                  htmlOutput("plotP", style="margin-left:-100px; margin-top:-80px")
        )
      )
    ),
    
### Tab showing data used for graphs
    tabPanel("Data",
      fluidPage(
        p("Data was obtained from ", a("here", href="http://www.census.gov/population/projections/data/national/"),
          style = "float:right"),
        column(3, selectInput("racet", h4("Select Race/Ethnicity:"), choices = race)),
        column(3, selectInput("sext", h4("Select Gender:"), choices = sex))
      ),
      hr(),
      fluidPage(
        DT::dataTableOutput("data2014")
      )
    )
  )
)
