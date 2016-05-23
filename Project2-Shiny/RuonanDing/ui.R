library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  
  dashboardHeader(title = "Open Food Database Nutrition"),
  
  dashboardSidebar(
    sidebarUserPanel("By R.Ding"),
    sidebarMenu(
      menuItem("Data Intro", tabName = "data_intro"),
      menuItem("Average Nutrition", tabName = "country"),
      menuItem("Nutrition Relationship", tabName = "category"),
      menuItem("Brand Guide", tabName = "brand")
    )),

  dashboardBody(
    tabItems(
      tabItem(tabName = "data_intro",
              textOutput("text1"),
              img(src="openfoodfacts-logo-en.svg", height = 400, width = 400),
              textOutput("text2"),
              tags$head(tags$style("#text1{color: black;
                                 font-size: 20px;
                                   }"
                         )),
              tags$head(tags$style("#text2{color: black;
                                   font-size: 20px;
                                   }"
                         ))
              ),
      tabItem(tabName = "country",
              fluidRow(
                box(              
                  title = "Select Item", status = "info", 
                  selectizeInput("nutrition", "Nutrition:", nutrition),
                  width = 4
                  )),
              fluidRow(
                
                box(
                  title = "Country Nutrition", status = "success",
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("TestPlot1"),
                  width = 12
                ),
                box(
                  title = "Product Categroy", status = "success",
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("TestPlot2"),
                  width = 12
                )
              )),
      tabItem(tabName = "category",
              fluidRow(
                box(                
                title = "Select X:", status = "info", 
                selectizeInput("nutrition2", "Nutrition X:", nutrition2, selected = "energy_100g"),
                width = 6
                ),
                box(                
                  title = "Select Y:", status = "info", 
                  selectizeInput("nutrition3", "Nutrition Y:", nutrition2, selected = "nutrition_score_uk_100g"),
                  width = 6
                ),
                box(
                title = "Food Score", status = "success",
                solidHeader = TRUE, collapsible = TRUE,
                plotOutput("TestPlot3"),
                width = 12
                )
              )),
      tabItem(tabName = "brand",
              fluidRow(
                box(
                  "Parameter Selection", br(),
                  selectizeInput("country", "Country :", countrylist, selected = "France"),
                  checkboxGroupInput("category", "Category:", choices = category, selected = "Dairies"),
                  width = 4
                ),
                box(
                  tableOutput('table'),
                  width = 8
                )
                )
              )
            )
        )
      )
    )



