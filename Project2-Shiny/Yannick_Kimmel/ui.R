library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
    dashboardHeader(title = "Health demographics in the USA", titleWidth = 340), 
    dashboardSidebar(
        sidebarUserPanel("Yannick Kimmel", image = "http://2igww43ul7xe3nor5h2ah1yq.wpengine.netdna-cdn.com/wp-content/uploads/2016/04/Yannick-300x300.jpg"),
        
        sidebarMenu(
            menuItem("Map", tabName = "mappanel", icon = icon("map")),
            menuItem("Trends", tabName = "trends", icon = icon("line-chart")),
            menuItem("Predictions", tabName = "predict", icon = icon("chevron-right")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        )
    ),
     dashboardBody(
         tabItems(
             tabItem(tabName = "mappanel",
                     fluidRow(
                         column(width = 12, 
                         box(
                             # helpText("Create demographic maps with
                             #          information from the USDA's Food Environment Atlas."),
                             selectInput("var", # choose the residents
                                         label = "Choose a variable to display",
                                         choices = c("Percent Adult Obese 2009", 
                                                     "Percent Adult Obese 2010",
                                                     "Percent Adult Diabetic 2009", 
                                                     "Percent Adult Diabetic 2010",
                                                     "Percent Poverty 2010"),
                                         selected = "Percent Obese 2009")
                             ),
                         box(sliderInput("range", # choose the range
                                         label = "Range of interest %:",
                                         min = 0, max = 50, value = c(0, 50)))
                         ), 
                                                plotOutput("map", width = "100%")
             )),
             tabItem(tabName = "trends",
                     fluidRow(
                         column(width = 12, 
                                box(
                                    # helpText("Create demographic maps with
                                    #          information from the USDA's Food Environment Atlas."),
                                    selectInput("var1", # choose the residents
                                                label = "Choose the X variable to display",
                                                choices = c("Percent Adult Obese 2010",
                                                            "Percent Adult Diabetic 2010",
                                                            "Percent Poverty 2010",
                                                            "Median household income, 2010",
                                                            "Population, low access to store (%), 2010",
                                                            "Grocery stores/1,000 pop, 2012",
                                                            "Convenience stores/1,000 pop, 2012",
                                                            "Fast-food restaurants/1,000 pop, 2012",
                                                            "Full-service restaurants/1,000 pop, 2012",
                                                            "Household food insecurity (%), 2010-12",
                                                            "Farms with direct sales, 2007",
                                                            "Farmers' markets/1,000 pop, 2013",
                                                            "Vegetable farms, 2007",
                                                            "High schoolers physically active (%), 2009",
                                                            "Recreation & fitness facilities/1,000 pop, 2012",
                                                            "% Population 65 years or older, 2010",
                                                            "% Population under age 18, 2010"),
                                                selected = "Median household income, 2010")
                                ),
                                box(selectInput("var2", # choose the residents
                                                label = "Choose the Y variable to display",
                                                choices = c("Percent Adult Obese 2010",
                                                            "Percent Adult Diabetic 2010",
                                                            "Percent Poverty 2010",
                                                            "Median household income, 2010",
                                                            "Population, low access to store (%), 2010",
                                                            "Grocery stores/1,000 pop, 2012",
                                                            "Convenience stores/1,000 pop, 2012",
                                                            "Fast-food restaurants/1,000 pop, 2012",
                                                            "Full-service restaurants/1,000 pop, 2012",
                                                            "Household food insecurity (%), 2010-12",
                                                            "Farms with direct sales, 2007",
                                                            "Farmers' markets/1,000 pop, 2013",
                                                            "Vegetable farms, 2007",
                                                            "High schoolers physically active (%), 2009",
                                                            "Recreation & fitness facilities/1,000 pop, 2012",
                                                            "% Population 65 years or older, 2010",
                                                            "% Population under age 18, 2010"),
                                                selected = "Percent Obese 2009")
                                )
                         ),
                         box(width =12, plotlyOutput("trendPlot", height = 550))
                     )
             ),
             
             tabItem(tabName = "predict",
                     fluidRow(
                         h1("Let's make a prediction:"),
                         h2(htmlOutput("predtable")),
                         column(width = 12, 
                             box(numericInput("GROC", label = h5("Grocery stores/1000 people"), 
                                              value = 0.24),
                                 numericInput("Conv", label = h5("Conveniences stores/1000 people"), 
                                              value = 0.60),
                                 numericInput("FF", label = h5("fast-food stores/1000 people"), 
                                              value = 0.58),
                                 numericInput("LACCESS", label = h5("Population, low access to store 
                                                                    (%), 2010"), 
                                              value = 23),
                                 numericInput("MEDHHIN", label = h5("Median household income, 2010"), 
                                              value = 4200),
                                 numericInput("PCT18", label = h5("% Population under age 18, 2010"), 
                                              value = 24)
                                 ),

                             box(
                                 numericInput("FOODINS", label = h5("Household food insecurity (%, 
                                                                    three-year average), 2010-12*"), 
                                              value = 15),
                                 numericInput("FARMRT", label = h5("Farmers' markets/1,000 pop, 2013"), 
                                              value = 0.58),
                                 numericInput("VEGFARM", label = h5("Vegetable farms, 2007"), 
                                              value = 21),
                                 numericInput("DIABETE", label = h5("Adult diabetes rate, 2010"), 
                                              value = 11),
                                 numericInput("HSACT", label = h5("High schoolers physically active (%), 2009*"), 
                                              value = 24),
                                 numericInput("POVRT", label = h5("Poverty rate, 2010"), 
                                              value = 17),
                                 numericInput("PCT65", label = h5("% Population 65 years or older, 2010"), 
                                              value = 16),
                                 numericInput("RECFAC", label = h5("Recreation & fitness facilities/1,000 pop, 2012"), 
                                              value = 0.067)
                             )
                         )
                     )
             ),
             
             tabItem(tabName = "data",
                     htmlOutput("tabdb"))
         )
     )
))