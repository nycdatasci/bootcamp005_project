library(shiny)
library(plotly)

shinyUI(fluidPage(
  titlePanel('Bachelor\'s Degrees and Salaries in the US' ),
  
  # choropleth of US
  fluidRow(
    column(2,
      selectInput(inputId = 'field',
        label = 'Choose Bachelor\'s Degree Field',
        choices = c('All', 'Science and Engineering', 'Science and Engineering Related',
                    'Business', 'Education', 'Arts, Humanities, and Other')
        )
    ),
    column(10,  
      plotlyOutput('choropleth')
    )
    
  ),
  
  # State and bachelor's degrees breakdown
  fluidRow(
    column(2,  
      selectInput(inputId = 'state',
          label = 'Choose state',
          choices = c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA',
                      'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ',
                      'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT',
                      'VA', 'WA', 'WV', 'WI', 'WY')
        )
    ),
    column(10, div(style = "height:50px;"),
      plotlyOutput('field_breakdown')  
    )
    
  ),
  
  
  # Median earnings per occupation
  fluidRow(
    column(2,
      checkboxGroupInput(inputId = 'job',
        label = 'Choose Occupation',
        choices = c('Management' = 'Mgmt',
                     'Business and Financial' = 'Bus_Fin',
                     'Computer and Mathematical' = 'Comp_Math',
                     'Architecture and Engineering' = 'Arch_Eng',
                     'Life, Physical, and Social Sciences' = 'Life_Phy_Soc_Sci',
                     'Community and Social Service' = 'Comm_Soc_Ser',
                     'Legal' = 'Legal',
                     'Education and Training' = 'Edu',
                     'Arts, Design, and Media' = 'Arts_Med',
                     'Health Diagnosing and Treating' = 'Health_Diag',
                     'Health Technologists' = 'Health_Tech'),
        selected = c('Management', 'Business and Financial', 'Computer and Mathematical',
                     'Architecture and Engineering', 'Life, Physical, and Social Sciences')
        )
    
    ),
    
    column(10,
      plotlyOutput('salaries') 
    )
  )
)















)