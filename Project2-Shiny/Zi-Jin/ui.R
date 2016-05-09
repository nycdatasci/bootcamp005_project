mybrand <- unique(top10$Brand)
size_interval <- unique(size$Size_Interval)
price_interval <- unique(price$Price_Interval)
mypackage <- unique(package$Package)

dashboardPage(
  dashboardHeader(title = "Soft Drink"),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Plots", tabName = "plots", icon = icon("th"))
    )
  ),
  dashboardBody(
      tabItem(tabName = "plots",
              #style = "height: 100%; width: 100%",
              tabsetPanel(type = "tabs", 
                          tabPanel("Top 10 Brands", 
                                   #style = "height: 100%; width: 100%",
                                   hr(),
                                   selectInput("brandname", "Brand (multiple allowed):",
                                               multiple = TRUE,
                                               choices = mybrand,
                                               selected = "Pepsi"),
                                   plotlyOutput('top10plot')
                                   ),
                          tabPanel("Size", 
                                   #style = "height: 100%; width: 100%",
                                   hr(),
                                   selectInput("sizeinterval", "Size Interval (multiple allowed):",
                                               multiple = TRUE,
                                               choices = size_interval,
                                               selected = "[400,800ml)"),
                                   plotlyOutput('sizeplot')
                          ),
                          tabPanel("Price", 
                                   #style = "height: 100%; width: 100%",
                                   hr(),
                                   selectInput("priceinterval", "Price Interval (multiple allowed):",
                                               multiple = TRUE,
                                               choices = price_interval,
                                               selected = "[2.5,3.5)"),
                                   plotlyOutput('priceplot')
                          ),
                          tabPanel("Package", 
                                   #style = "height: 100%; width: 100%",
                                   hr(),
                                   selectInput("packagename", "Package (multiple allowed):",
                                               multiple = TRUE,
                                               choices = mypackage,
                                               selected = "Bottle"),
                                   plotlyOutput('packageplot')
                          )
                          )
                          
              )
      ) #end of tabItem plots

  ) #end of dashboardBody
