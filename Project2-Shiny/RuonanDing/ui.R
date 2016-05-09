library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  
  dashboardHeader(title = "Food Facts"),
  
  dashboardSidebar(
    sidebarUserPanel("Food Nutrition"),
    sidebarMenu(
      menuItem("Average Nutrition", tabName = "country"),
      menuItem("Food Score", tabName = "category")
    )),

  dashboardBody(
    tabItems(
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
                  width = 6
                ),
                box(
                  title = "Product Categroy", status = "success",
                  solidHeader = TRUE, collapsible = TRUE,
                  plotOutput("TestPlot2"),
                  width = 6
                )
              )),
      tabItem(tabName = "category",
              fluidRow(
                box(                
                title = "Select Item", status = "info", 
                selectizeInput("nutrition2", "Nutrition:", nutrition2),
                width = 4
                ),
                box(
                title = "Food Score", status = "success",
                solidHeader = TRUE, collapsible = TRUE,
                plotOutput("TestPlot3"),
                width = 12
                )
              ))
      )
    )
  )
)


