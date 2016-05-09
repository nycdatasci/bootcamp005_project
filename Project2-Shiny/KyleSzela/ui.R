#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(signal)
library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
library(shinydashboard)
library(googleVis)
shinyUI(dashboardPage(
  dashboardHeader(title = "Anomaly Detection"),
  dashboardSidebar(
    
    sidebarUserPanel("Kyle Szela",
                     image = "Kyle.jpg"
    ),
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Filters", tabName = "filter", icon = icon("filter")),
      menuItem("Anomalies", tabName = "anomaly", icon = icon("check"))
      #menuItem("Save Data", tabName = "save", icon = icon("save"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Data Chart",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 12,
            htmlOutput("dataChart")
          )
        ),
        fluidRow(
          box(
            fileInput('file1', 'Choose file to upload',
                      accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.tsv'
                      )
            ),
            tags$hr(),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ','),
            radioButtons('quote', 'Quote',
                         c(None='',
                           'Double Quote'='"',
                           'Single Quote'="'"),
                         '"'),
            tags$hr(),
            title = "Select File",
            status = "info",
            solidHeader = T,
            collapsible = T
          ),
          box(
            title = "Data",
            status = "info",
            solidHeader = T,
            collapsible = T,
            htmlOutput('contents')
          )
        )
      ),
      tabItem(
        tabName = "filter",
        fluidRow(
          box(
            title = "Data Chart",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 12,
            htmlOutput('dataChartFiltered')
          )
        ),
        fluidRow(
          box(
            title = "Filters",
            status = "info",
            solidHeader = T,
            collapsible = T,
            width = 3,
            radioButtons("filt", NULL,
                         c("None" = "none",
                           "Butterworth" = "butt",
                           "Type-II Chebyshev" = "cheby2")),
            submitButton("Filter")
          ),
          box(
            title = "Butterworth",
            status = "info",
            solidHeader = T,
            collapsible = T,
            width = 3,
            textInput("buttern", label = "Filter Order", value = "3"),
            textInput("butterf", label = "Critical Frequencies", value = "0.1"),
            radioButtons("buttert", "Type",
                         c("Low-Pass" = "low",
                           "High-Pass" = "high"))
          ),
          box(
            title = "Chebyshev",
            status = "info",
            solidHeader = T,
            collapsible = T,
            width = 3,
            textInput("chebyn", label = "Filter Order", value = "5"),
            textInput("chebyd", label = "dB of Pass Band", value = "20"),
            textInput("chebyf", label = "Critical Frequencies", value = "0.2"),
            radioButtons("chebyt", "Type",
                         c("Low-Pass" = "low",
                           "High-Pass" = "high"))
          )
        )
      ),
      tabItem(
        tabName = "anomaly",
        fluidRow(
          box(
            title = "Data Chart",
            status = "primary",
            solidHeader = T,
            collapsible = T,
            width = 12,
            plotOutput('dataChartAnoms')
          )
        ),
        fluidRow(
          box(
            title = "Settings",
            status = "info",
            solidHeader = T,
            collapsible = T,
            textInput("anomalym", label = "Max % of Anomalies", value = "0.2"),
            textInput("anomalya", label = "Alpha", value = "0.05"),
            radioButtons("anomalyd", "Direction",
                         c("None" = "none",
                           "Positive" = "pos",
                           "Negative" = "neg",
                           "Both" = "both")),
            submitButton("Filter")
          )
        )
        
      )
    )
  )
))
  
