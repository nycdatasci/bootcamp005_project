shinyUI(fluidPage(
      titlePanel("Cost of Living Estimator Between Cities"),
      fluidRow(
        column(4,
        wellPanel(sliderInput("Salary", "Annual Salary in Dollars:",min = 10000, max = 500000, value= 10000)),
               selectInput("From.city", "From City:",Place,selected=1),
               selectInput("To.city", "To City:",Place,selected=1),
               br(),
               htmlOutput("plot"),
       conditionalPanel("Output.text1 !=NULL ", textOutput("text1")),
       conditionalPanel("Output.text2 !=NULL ", textOutput("text2")),
       #conditionalPanel("Output.text1 !=NULL | Output.text2 !=NULL",htmlOutput("plott"))),
       #textOutput("text1"),
       #textOutput("text2"),
       br(),
       htmlOutput("plott"))
       ,
        column(7,offset=1.0,htmlOutput("plot1"),br(),br(),br(),br(),htmlOutput("plot2"))
        )
        )
      )
    