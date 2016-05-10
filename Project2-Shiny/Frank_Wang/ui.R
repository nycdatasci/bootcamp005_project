library(shiny)
library(shinythemes)
library(googleVis)
library(plotly)

Hometype <- c("All Homes" = "All Homes", 
              "Single Family" = "Single Fam",
              "Condo" = "Condo", 
              "One Bed" = "One Bed",
              "Two Bed"="Two Bed",
              "Three Bed"="Three Bed")
CA_county<-c("CA"="CA","Alameda"="Alameda","Amador"="Amador","Butte"="Butte",                            
             "Calaveras"="Calaveras", "Contra-Costa"="Contra-Costa","Del_Norte"="Del_Norte",                            "El_Dorado"="El_Dorado","Fresno"="Fresno","Glenn"="Glenn","Humboldt"="Humboldt",                           "Kern"="Kern","Kings"="Kings","Lake"="Lake","Los Angeles"="Los_Angeles",                                   "Madera"="Madera","Marin"="Marin","Mariposa"="Mariposa","Mendocino"="Mendocino",                           "Merced"="Merced","Monterey"="Monterey","Napa"="Napa","Nevada"="Nevada",
             "Orange"="Orange","Placer"="Placer","Plumas"="Plumas","Riverside"="Riverside",
             "Sacramento"="Sacramento","San Benito"="San_Benito","San Bernardino"="San_Bernardino",
             "San Diego"="San_Diego","San Francisco"="San_Francisco","San Joaquin"="San_Joaquin",
             "San Luis Obispo"="San_Luis_Obispo","San Mateo"="San_Mateo","Santa Barbara"="Santa_Barbara",
             "Santa Clara"="Santa_Clara","Santa Cruz"="Santa_Cruz","Shasta"="Shasta",
             "Siskiyou"="Siskiyou","Solano"="Solano","Sonoma"="Sonoma","Stanislaus"="Stanislaus",
             "Sutter"="Sutter","Tehama"="Tehama","Tulare"="Tulare","Tuolumne","Tuolumne",
             "Ventura"="Ventura","Yolo"="Yolo", "Yuba"="Yuba",
             "Los Angeles Metropolitan Area"="Los_Angeles_Metropolitan_Area", 
             "S.F.Bay Area"="S.F._Bay_Area","Inland_Empire"="Inland_Empire")
shinyUI(
  fluidPage(
     titlePanel("Is it better to buy or rent?"),
#   theme = shinytheme("cosmo"),  
#--------------------------------------------------------  
 # titlePanel("Input your choices"),
# tabPanel("Summary",
  sidebarLayout(
    sidebarPanel(
#      helpText("What do you think about this app?"),
      numericInput("home_price", "Home price", 250000),
      selectInput("loan_term", 'Select mortgage term', 
                         choices = c("10 years","15 years","20 years","30 years"),
                         selected = "30 years"),
#      selectInput("var",
#                  label = "Choose one to display",
#                  choices = c("awesome", "dramastic",
#                              "admirable", "wonderful"),
#                  selected = "awesome"),
      sliderInput("downpayment",
                  label = "Downpayment (%)",
                  min = 0, max = 100, value = 20),
      sliderInput("interest_rate",
            label = "Interst rate (%)",
            min = 0, max = 10, value = 4,step=0.01),
       sliderInput("rent_rate_monthly",
            label = "Monthly rent($)",
            min = 0, max = 4000, value = 888),
       sliderInput("home_price_growth_rate",
            label = "Expected Home Appreciation Rate %",
            min = 0, max = 10, value = 2,step=0.01),
       sliderInput("rent_growth_rate",
            label = "Expected Rent growth Rate %",
            min = 0, max = 10, value = 2.5,step=0.01),
      sliderInput("invest_rate",
            label = "Expected investment Return Rate %",
            min = 0, max = 20, value = 6,step=0.01),
      sliderInput("Home_property_tax",
            label = "Property Tax Rate %",
            min = 0, max = 5, value = 1.35,step=0.01),
      sliderInput("Income_tax_rate",
            label = "Your tax rate %",
            min = 0, max = 40, value = 20),
      sliderInput("Home_Maintenance_year",
            label = "Annually Home Maintenance ($)",
            min = 0, max = 10000, value = 2500),
      sliderInput("Home_insurance",
            label = "Annually Home Insurance ($)",
            min = 0, max = 10000, value = 1150),
     sliderInput("cost_buy_rate",
            label = "Cost Buy  Home (% in Home price)",
            min = 0, max =10, value = 1,step=0.01),
      sliderInput("cost_sell_rate",
            label = "Cost Sell Home (% in Home price)",
            min = 0, max =10, value = 6,step=0.01),
     numericInput("years_stay", "years own home", 5)
    ),

mainPanel(
  tabsetPanel(
    tabPanel("Summary",
             h2("Summary of recommendation"),
        h3(textOutput("text1")),
        h3(textOutput("text5")),
        h3(textOutput("text2")),
        br(),
        h3(textOutput("text3")), 
        h3(textOutput("text4")),
        h3(textOutput("text6"))
    ),
    tabPanel("Explore", 
       mainPanel(plotlyOutput("plot"),height=400,width=300),
       mainPanel(plotlyOutput("rentplot"),height=400,width=300),
       mainPanel(plotlyOutput("rentplotmod"),height=400,width=300)
    ),
    tabPanel("Data",
             dataTableOutput("mytable1")  
    ),
    tabPanel("Housing Price",
             selectizeInput('Hometype', "Home Type", Hometype),
             mainPanel(plotlyOutput("housingpriceplot"),height=400,width=300),
             selectizeInput("CA_county", "California County", CA_county, selected="CA"),
             br(),
             mainPanel(plotlyOutput("CAhousingplot"),height=400,width=300),
             numericInput("zipcode", "Housing/Rent at zip code (Mar.2016)", 95014),
             h3(textOutput("zip_housing_rent1")),
             h3(textOutput("zip_housing_rent2"))
    ),
  tabPanel("About",
           fluidPage(theme = shinytheme("cosmo"), sidebarPanel(fluidRow(
             wellPanel(
               h4("Frank Lanfa Wang"),
               h5("Data Science Fellow"),
               h6("This project was completed for the NYC Data Science Academy Winter 2016 Bootcamp."),
               h6("More info at:"),
               tags$a(href = "http://nycdatascience.com/", "NYC Data Science Academy", style = "font-size: 18px;")
             ))
           )
           )
)))
)))