
library(shiny)
library(shinydashboard)
library(googleVis)

shinyUI(dashboardPage(
    dashboardHeader(title='US Temperatures'),
    dashboardSidebar(
        a('by Steven Ginzberg',href='http://blog.nycdatascience.com/author/stevenginzberg-net/'),
        br(),
        sidebarMenu(
            menuItem('Chart',tabName='chart',icon=icon('line-chart')),
            menuItem('Map',tabName='map',icon=icon('map'))),
        br(),
        sliderInput('yrSlider',label='Year',
                    min=1840,max=2000,value=1840,sep=''),
        sliderInput('moSlider',label='Month(s)',min=1,max=12,value=c(1,12)),
        br(),
        br(),
        a('Data Source',href='http://www.ncdc.noaa.gov/data-access/land-based-station-data/land-based-datasets/global-historical-climatology-network-ghcn')
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName='chart',
                    htmlOutput('wthrChart'),width=500,height=300),
            tabItem(tabName='map',
                    h1('Average Temperatures'),
                    htmlOutput('wthrMap'),width=400,height=300)
        )
        
)))
