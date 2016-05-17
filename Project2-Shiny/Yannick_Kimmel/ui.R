library(shiny)
library(shinydashboard)
library(plotly)
source("helpers.R")
library(DT)

shinyUI(dashboardPage(
    dashboardHeader(title = "Food and health demographics in the USA", titleWidth = 400), 
    dashboardSidebar(
        sidebarUserPanel("Yannick Kimmel", image = "Yannick.jpg"),
        
        sidebarMenu(
            menuItem("Map", tabName = "mappanel", icon = icon("map")),
            menuItem("Trends", tabName = "trends", icon = icon("line-chart")),
            menuItem("Predictions", tabName = "predict", icon = icon("chevron-right")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        ),
        helpText("  Information from the USDA's Food Environment Atlas on counties in the USA.")
    ),
     dashboardBody(
         tabItems(
             tabItem(tabName = "mappanel",
                     fluidRow(
                         column(width = 12, 
                         box(
                             
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
                         
                         h2("Let's predict using multiple linear regression*:"),
                         h3(htmlOutput("predtable")),
                         column(width = 12, 
                             box(
                                 sliderInput("GROC", label = p("Grocery stores/1000 people, 2012"),
                                             min = rlow(data$GROCPTH12), 
                                             max = rhi(data$GROCPTH12), value = 
                                                 valme(data$GROCPTH12)), 
                                 sliderInput("Conv", label = p("Conveniences stores/1000 people, 2012"),
                                             min = rlow(data$CONVSPTH12), 
                                             max = rhi(data$CONVSPTH12), value = 
                                                 valme(data$CONVSPTH12)), 
                                 sliderInput("FF", label = p("Fast-food stores/1000 people, 2012"),
                                             min = rlow(data$FFRPTH12), 
                                             max = rhi(data$FFRPTH12), value = 
                                                 valme(data$FFRPTH12)), 
                                 sliderInput("Full", label = p("Full-service restaurants/1,000 pop, 2012"),
                                             min = rlow(data$FSRPTH12), 
                                             max = rhi(data$FSRPTH12), value = 
                                                 valme(data$FSRPTH12)), 
                                 sliderInput("LACCESS", label = p("Population, low access to store 
                                                                    (%), 2010"),
                                             min = rlow(data$PCT_LACCESS_POP10), 
                                             max = rhi(data$PCT_LACCESS_POP10), value = 
                                                 valme(data$PCT_LACCESS_POP10)), 
                                 sliderInput("MEDHHIN", label = p("Median household income, 2010"),
                                             min = rlow(data$MEDHHINC10), 
                                             max = rhi(data$MEDHHINC10), value = 
                                                 valme(data$MEDHHINC10)), 
                                 sliderInput("PCT18", label = p("% Population under age 18, 2010"),
                                             min = rlow(data$PCT_18YOUNGER10), 
                                             max = rhi(data$PCT_18YOUNGER10), value = 
                                                 valme(data$PCT_18YOUNGER10)), 
                                 sliderInput("RECFAC", label = p("Recreation & fitness facilities/
                                                                  1,000 pop, 2012"),
                                             min = rlow(data$RECFACPTH12), 
                                             max = rhi(data$RECFACPTH12), value = 
                                                 valme(data$RECFACPTH12))
                                 ),

                             box(
                                 sliderInput("FOODINS", label = p("Household food insecurity (%, 
                                                                   three-year average), 2010-12*"), 
                                             min = rlow(data$FOODINSEC_10_12), 
                                             max = rhi(data$FOODINSEC_10_12), value = 
                                                 valme(data$FOODINSEC_10_12)), 
                                 sliderInput("FARMRT",label = p("Farmers' markets/1,000 pop, 2013"),
                                             min = rlow(data$FMRKTPTH13), 
                                             max = rhi(data$FMRKTPTH13), value = 
                                                 valme(data$FMRKTPTH13)),
                                 sliderInput("VEGFARM", label = p("Vegetable farms, 2007"),
                                             min = rlow(data$VEG_FARMS07), 
                                             max = rhi(data$VEG_FARMS07), value = 
                                                 valme(data$VEG_FARMS07)),
                                 sliderInput("DIABETE", label = p("Adult diabetes rate, 2010"), 
                                             min = rlow(data$PCT_DIABETES_ADULTS10), 
                                             max = rhi(data$PCT_DIABETES_ADULTS10), value = 
                                                 valme(data$PCT_DIABETES_ADULTS10)),
                                 sliderInput("HSACT", label = p("High schoolers physically active (%), 2009"),
                                             min = rlow(data$PCT_HSPA09), 
                                             max = rhi(data$PCT_HSPA09), value = 
                                                 valme(data$PCT_HSPA09),
                                             step =  0.1),
                                 sliderInput("POVRT", label = p("Poverty rate, 2010"),
                                             min = rlow(data$POVRATE10), 
                                             max = rhi(data$POVRATE10), value = 
                                                 valme(data$POVRATE10)),
                                 sliderInput("PCT65", label = p("% Population 65 years or older, 2010"),
                                             min = rlow(data$PCT_65OLDER10), 
                                             max = rhi(data$PCT_65OLDER10), value = 
                                                 valme(data$PCT_65OLDER10))
                             )
                         ),
                         h2("Model fitting:"),
                         p("Summary: As a preliminary prediction analysis, multiple linear regression was used
                                  on 17 variables of interest to predict obesity rates. Stepwise regression 
                                  showed that at least 10 variables are significant. Basic diagnostics indicate
                                  model assumptions were not violated. 76% of the data was complete cases, while 
                                  the rest had at least one NA. Only the complete cases were used in prediction."),
                         h3("Coefficient table"),
                         DT::dataTableOutput("coefs"),
                         p(sigcodes),
                         p(sumreg),
                         h2("Appendix: Diagnostics"),
                         dataTableOutput("rendvifs"),
                         img(src = "Resvsfitted.jpeg"),
                         img(src = "qqpot.jpeg"),
                         img(src = "scaleloc.jpeg"),
                         img(src = "leverage.jpeg"),
                         img(src = "avplot1.jpeg"),
                         img(src = "avplot2.jpeg")
                         
                         
                     )
             ),
             
             tabItem(tabName = "data",
                     DT::dataTableOutput("tabdb"))
         )
     )
))