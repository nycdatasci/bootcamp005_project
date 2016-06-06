library(shiny)
library(ggplot2)
library(shinydashboard)
library(googleVis)

shinyUI(dashboardPage( 
  dashboardHeader(title = "Options vs Sentiment"),
  dashboardSidebar(
    sidebarUserPanel("Kyle Szela",
                     image = "Kyle.jpg"
    ),
    sidebarMenu(
      menuItem("Yearly", tabName = "yearly", icon = icon("database")),
      menuItem("Weekly", tabName = "weekly", icon = icon("filter")),
      menuItem("Hourly", tabName = "hourly", icon = icon("check")),
      menuItem("Bull/Bear Cloud", tabName = "bbcloud", icon = icon("cloud")),
      menuItem("Pos/Neg Cloud", tabName = "pncloud", icon = icon("cloud"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "yearly",
        fluidRow(
          box(
            title = "Yearly Chart",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 12,
            htmlOutput("yearlyChart")
          )
        )
      ),
      tabItem(
        tabName = "weekly",
        fluidRow(
          box(
            title = "Weekly Chart",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 12,
            htmlOutput("weeklyChart")
          )
        )
      ),
      tabItem(
        tabName = "hourly",
        fluidRow(
          box(
            title = "Hourly Line Chart by Day",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 12,
            height = "100%",
            htmlOutput("hourlyChart")
          )
        )
      ),
      tabItem(
        tabName = "bbcloud",
        fluidRow(
          box(
            title = "Twits Tagged with Bullish",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 6,
            plotOutput("bullCloud")
          ),
          box(
            title = "Twits Tagged with Bearish",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 6,
            plotOutput("bearCloud")
          )
        )
      ),
      tabItem(
        tabName = "pncloud",
        fluidRow(
          box(
            title = "Positive Twits",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 6,
            plotOutput("posCloud")
          ),
          box(
            title = "Negative Twits",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 6,
            plotOutput("negCloud")
          )
        )
      )
    )
  )
))
