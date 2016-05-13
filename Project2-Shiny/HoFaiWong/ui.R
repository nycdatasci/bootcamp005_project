########################################
#### Project 2 - Shiny Visualization####
#### Ho Fai Wong - May 8, 2016      ####
########################################

## ui.r ##
shinyUI(
  navbarPage(
    title = "2015 World University Rankings",
    id = "nav",
    theme = shinytheme("flatly"),
    
    #####################
    ####Country Stats####
    #####################
    
    tabPanel("Country Stats",
             
             fluidRow(
               column(width = 3,
                      style = "background-color:#F8F8F8",
                      h4("Ranking organization"),
                      radioButtons("sourceCountry",
                                   "Select organization",
                                   choices = list("Shanghai Rankings" = 1,
                                                  "Times World University Rankings" = 2,
                                                  "Center for World University Rankings" = 3),
                                   selected = 1),
                      br(),
                      conditionalPanel(
                        condition = "input.sourceCountry == 1", #Shanghai
                        h4("Shanghai ranking metrics"),
                        
                        radioButtons("selectedStat_shanghai",
                                     "Select metric",
                                     choices = list("Top rank" = 1, 
                                                    "Median rank" = 2,
                                                    "Count of ranked universities" = 3,
                                                    "Mean alumni score" = 5,
                                                    "Mean award score" = 6,
                                                    "Mean HiCi score" = 7,
                                                    "Mean N&S score" = 8,
                                                    "Mean PUB score" = 9,
                                                    "Mean PCP score" = 10),
                                     selected = 1)
                      ),
                      
                      conditionalPanel(
                        condition = "input.sourceCountry == 2", #Times
                        h4("Times ranking metrics"),
                        
                        radioButtons("selectedStat_times",
                                     "Select metric",
                                     choices = list("Top rank" = 1, 
                                                    "Median rank" = 2,
                                                    "Count of ranked universities" = 3,
                                                    "Mean teaching score" = 5,
                                                    "Mean international score" = 6,
                                                    "Mean research score" = 7,
                                                    "Mean citations score" = 8,
                                                    "Mean income score" = 9),
                                     selected = 1)
                      ),
                      
                      conditionalPanel(
                        condition = "input.sourceCountry == 3", #CWUR
                        h4("CWUR ranking metrics"),
                        
                        radioButtons("selectedStat_cwur",
                                     "Select metric",
                                     choices = list("Top rank" = 1, 
                                                    "Median rank" = 2,
                                                    "Count of ranked universities" = 3,
                                                    "Mean education score*" = 5,
                                                    "Mean alumni employment score*" = 6,
                                                    "Mean faculty score*" = 7,
                                                    "Mean publications score*" = 8,
                                                    "Mean influence score*" = 9,
                                                    "Mean citations score*" = 10,
                                                    "Mean broad impact score*" = 11,
                                                    "Mean patents score*" = 12),
                                     selected = 1),
                        helpText("* CWUR sub-criteria rankings were converted to scores. See Reference tab for details")
                      ),
                      hr(),
                      em("Note:"),
                      br(),
                      helpText("Select an organization and their associated metrics. Please refer to the Reference tab for definitions")
               ),
               column(width = 9,
                      tabsetPanel(
                        tabPanel("Map",
                                 htmlOutput("country.map")),
                        tabPanel("Bar Chart",
                                 htmlOutput("country.bar")
                        )
                      )
               ))),
    
    
    ####################
    ####Scatter Plot####
    ####################
    
    tabPanel("Org Comparison",
             fluidRow(
               column(3,
                      style = "background-color:#F8F8F8",
                      h4("Ranking organization"),
                      radioButtons("sourceScatterY",
                                   "Select organization for y-axis",
                                   choices = list("Shanghai Rankings" = 1,
                                                  "Times World University Rankings" = 2,
                                                  "Center for World University Rankings" = 3),
                                   selected = 1),
                      radioButtons("sourceScatterX",
                                   "Select organization for x-axis",
                                   choices = list("Shanghai Rankings" = 1,
                                                  "Times World University Rankings" = 2,
                                                  "Center for World University Rankings" = 3),
                                   selected = 2),
                      hr(),
                      h4("Country Selection"),
                      selectInput('countryScatter',
                                  'Select one or more countries',
                                  c('All',countries),
                                  multiple = TRUE,
                                  selected='All',
                                  selectize = TRUE),
                      hr(),
                      em("Note:"),
                      br(),
                      helpText("Select ranking organizations for X and Y axis. You can also zoom in on the plot or select one or more countries")
                      
               ),
               column(9,
                      fluidRow(
                        column(12, 
                               plotlyOutput('country.scatter')
                        )
                      ),
                      fluidRow(
                        column(6,
                               #style = "background-color:#F8F8F8",
                               h4(paste0('Modify Y-axis criteria')),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterY == 1", #Shanghai
                                 h5('Filter by university rank (lower is better)'),
                                 sliderInput("y.sh.rank", "Shanghai rank:", min=0, max=500, value=c(1,500)),
                                 h5('Filter by criteria score (higher is better)'),
                                 sliderInput("y.sh.alumni", "Alumni score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.award", "Award score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.hici", "HiCi score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.ns", "N&S score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.pub", "PUB score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.pcp", "PCP score:", min=0, max=100, value=c(0,100))
                               ),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterY == 2", #Times
                                 h5('Filter by university rank (lower is better)'),
                                 sliderInput("y.t.rank", "Times rank:", min=0, max=400, value=c(1,400)),
                                 h5('Filter by criteria score (higher is better)'),
                                 sliderInput("y.t.teaching", "Teaching score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.t.international", "International score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.t.research", "Research score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.t.citations_times", "Citations score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.t.income", "Income score:", min=0, max=100, value=c(0,100))
                               ),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterY == 3", #CWUR
                                 h5('Filter by university rank (lower is better)'),
                                 sliderInput("y.c.rank", "CWUR rank:", min=0, max=1000, value=c(1,1000)),
                                 h5('Filter by criteria score (higher is better)'),
                                 sliderInput("y.c.education", "Education score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("y.c.alumni", "Alumni employment score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("y.c.faculty", "Faculty score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("y.c.pub", "Publications score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("y.c.influence", "Influence score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("y.c.citations", "Citations score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("y.c.impact", "Broad impact score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("y.c.patents", "Patents score*:", min=0, max=100, value=c(1,100)),
                                 helpText("* CWUR sub-criteria rankings were converted to scores. See Reference tab for details")
                               ) ),
                        
                        column(6,
                               #style = "background-color:#F8F8F8",
                               h4(paste0('Modify X-axis criteria')),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterX == 1", #Shanghai
                                 h5('Filter by university rank (lower is better)'),
                                 sliderInput("x.sh.rank", "Shanghai rank:", min=0, max=500, value=c(1,500)),
                                 h5('Filter by criteria score (higher is better)'),
                                 sliderInput("x.sh.alumni", "Alumni score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.award", "Award score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.hici", "HiCi score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.ns", "N&S score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.pub", "PUB score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.pcp", "PCP score:", min=0, max=100, value=c(0,100))
                               ),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterX == 2", #Times
                                 h5('Filter by university rank (lower is better)'),
                                 sliderInput("x.t.rank", "Times rank:", min=0, max=400, value=c(1,400)),
                                 h5('Filter by criteria score (higher is better)'),
                                 sliderInput("x.t.teaching", "Teaching score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.t.international", "International score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.t.research", "Research score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.t.citations_times", "Citations score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.t.income", "Income score:", min=0, max=100, value=c(0,100))
                               ),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterX == 3", #CWUR
                                 h5('Filter by university rank (lower is better)'),
                                 sliderInput("x.c.rank", "CWUR rank:", min=0, max=1000, value=c(1,1000)),
                                 h5('Filter by criteria score (higher is better)'),
                                 sliderInput("x.c.education", "Education score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("x.c.alumni", "Alumni employment score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("x.c.faculty", "Faculty score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("x.c.pub", "Publications score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("x.c.influence", "Influence score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("x.c.citations", "Citations score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("x.c.impact", "Broad impact score*:", min=0, max=100, value=c(1,100)),
                                 sliderInput("x.c.patents", "Patents score*:", min=0, max=100, value=c(1,100)),
                                 helpText("* CWUR sub-criteria rankings were converted to scores. See Reference tab for details")
                               ) )
                      )
               )
             )
    ),
    
    
    
    ###################
    ####Uni Profile####
    ###################
    
    tabPanel("University Profile",
             fluidRow(
               column(width = 3,
                      style = "background-color:#F8F8F8",
                      selectInput('selectUni',
                                  'Select university',
                                  universities,
                                  selected = 'New York University',
                                  selectize=TRUE)
               ),
               column(width = 9,
                      dataTableOutput("unirank.table"))
             ),
             hr(),
             fluidRow(width=12,
                      column(width = 4,
                             h4("Shanghai Scores"),
                             htmlOutput("bar.shanghai")
                      ),
                      column(width = 4,
                             h4("Times Scores"),
                             htmlOutput("bar.times")),
                      column(width = 4,
                             h4("CWUR Scores*"),
                             htmlOutput("bar.cwur"),
                             helpText("* CWUR sub-criteria rankings were converted to scores. See Reference tab for details"))
             )
    ),    
    
    
    ############
    ####Data####
    ############
    
    tabPanel("Data",
             tabsetPanel(
               tabPanel("Country Data",
                        fluidRow(
                          column(3,
                                 style = "background-color:#F8F8F8",
                                 selectizeInput('selectCountry',
                                                'Select one or more countries',
                                                countries,
                                                multiple = TRUE,
                                                selected=c('United States of America','United Kingdom','France','Hong Kong'))),
                          column(9,
                                 h4("Shanghai Rankings Data:"),
                                 div(dataTableOutput("selectcountry.shanghai.table"), style = "font-size:80%"),
                                 hr(),
                                 h4("Times Rankings Data:"),
                                 div(dataTableOutput("selectcountry.times.table"), style = "font-size:80%"),
                                 hr(),
                                 h4("CWUR Rankings Data:"),
                                 div(dataTableOutput("selectcountry.cwur.table"), style = "font-size:80%"))
                        )
               ),
               tabPanel("University Data",
                        fluidRow(
                          column(3,
                                 style = "background-color:#F8F8F8",
                                 selectizeInput('selectUniData',
                                                'Select one or more universities',
                                                universities,
                                                multiple = TRUE,
                                                selected=c('New York University','Harvard University','Carnegie Mellon University','University of Hong Kong'))),
                          column(9,
                                 h4("Shanghai Rankings Data:"),
                                 div(dataTableOutput("selectuni.shanghai.table"), style = "font-size:80%"),
                                 hr(),
                                 h4("Times Rankings Data:"),
                                 div(dataTableOutput("selectuni.times.table"), style = "font-size:80%"),
                                 hr(),
                                 h4("CWUR Rankings Data:"),
                                 div(dataTableOutput("selectuni.cwur.table"), style = "font-size:80%"))
                        )
               )
             )
    ),
    
    
    
    #################
    ####Reference####
    #################
    
    tabPanel("Reference",
             fluidRow(
               column(12,
                      tabsetPanel(
                        tabPanel("About the data",
                                 fluidRow(
                                   column(6,
                                          h4('Summary'),
                                          p("This Shiny application was developed by Ho Fai Wong as part of the NYC Data Science Academy bootcamp and last updated on 5/13/2016."),
                                          p(a("Kaggle", href="https://www.kaggle.com/mylesoneill/world-university-rankings")," provided world university rankings from 3 different ranking organizations. 
                                            This Shiny application focuses on the 2015 rankings, covering 1015 institutions across 61 countries."),
                                          tags$ul(
                                            tags$li(strong(a("Shanghai Rankings", href='http://www.shanghairanking.com/')),
                                                    ": 500 universities"),
                                            tags$li(strong(a("Times World University Rankings", href='https://www.timeshighereducation.com/world-university-rankings')),
                                                    ": 401 universities"),
                                            tags$li(strong(a("Center for World University Rankings", href='http://cwur.org/')),
                                                    ": 1000 universities")
                                          ),
                                          hr(),
                                          h4('Notes'),
                                          tags$ul(
                                            tags$li('University and country names were reconciled across organizations, and missing countries filled in when necessary.'),
                                            tags$li('When a university\'s world rank is a range, the middle of the range was used for visualization purposes.'),
                                            tags$li('CWUR sub-criteria rankings were converted to scores out of 100 for consistency and comparison with Times and Shanghai data: score = (1-rank/max)*100'),
                                            tags$li('Missing sub-criteria ranks/scores, assumed to be Missing At Random, were imputed using K-Nearest Neigbhors with k=sqrt(n).')
                                          )
                                   ),
                                   column(6,
                                          h4('Gaps in ranked universities across the 3 ranking organizations'),
                                          plotOutput('overlap'))
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(4,
                                          h4('Shanghai criteria correlations'),
                                          plotOutput('shanghai.correlation')),
                                   column(4,
                                          h4('Times criteria correlations'),
                                          plotOutput('times.correlation')),
                                   column(4,
                                          h4('CWUR criteria correlations'),
                                          plotOutput('cwur.correlation'))
                                 )
                        ),
                        
                        tabPanel("Criteria Definitions",
                                 column(4,
                                        strong('Shanghai Rankings'),
                                        tags$ul(
                                          tags$li(strong('world_rank'),' - world rank for university. Contains rank ranges and equal ranks (eg. 101-152). For this analysis, the middle of each range was used.'),
                                          tags$li(strong('alumni'),' - Alumni Score, based on the number of alumni of an institution winning nobel prizes and fields medals.'),
                                          tags$li(strong('award'),' - Award Score, based on the number of staff of an institution winning Nobel Prizes in Physics, Chemistry, Medicine, and Economics and Fields Medals in Mathematics.'),
                                          tags$li(strong('hici'),' - HiCi Score, based on the number of Highly Cited Researchers selected by Thomson Reuters.'),
                                          tags$li(strong('ns'),' - N&S Score, based on the number of papers published in Nature and Science.'),
                                          tags$li(strong('pub'),' - PUB Score, based on total number of papers indexed in the Science Citation Index-Expanded and Social Science Citation Index.'),
                                          tags$li(strong('pcp'),' - PCP Score, the weighted scores of the above five indicators divided by the number of full time academic staff.')
                                        )
                                 ),
                                 column(4,
                                        strong('Times World University Rankings'),
                                        tags$ul(
                                          tags$li(strong("world_rank")," - world rank for the university. Contains rank ranges and equal ranks (eg. =94 and 201-250). For this analysis, the middle of each range was used."),
                                          tags$li(strong("teaching")," - university score for teaching (the learning environment)."),
                                          tags$li(strong("international")," - university score international outlook (staff, students, research)."),
                                          tags$li(strong("research")," - university score for research (volume, income and reputation)."),
                                          tags$li(strong("citations")," - university score for citations (research influence)."),
                                          tags$li(strong("income")," - university score for industry income (knowledge transfer)."),
                                          tags$li(strong("num_students")," - number of students at the university."),
                                          tags$li(strong("student_staff_ratio")," - Number of students divided by number of staff."),
                                          tags$li(strong("international_students")," - Percentage of students who are international."),
                                          tags$li(strong("female_male_ratio")," - Female student to Male student ratio.")
                                        )
                                 ),
                                 column(4,
                                        strong('Center for World University Rankings'),
                                        tags$ul(
                                          tags$li(strong('world_rank'),' - world rank for university.'),
                                          tags$li(strong('national_rank'),' - rank of university within its country.'),
                                          tags$li(strong('quality_of_education')," - measured by the number of a university's alumni who have won major international awards, prizes, and medals relative to the university's size."),
                                          tags$li(strong('alumni_employment'),"measured by the number of a university's alumni who have held CEO positions at the world's top companies relative to the university's size"),
                                          tags$li(strong('quality_of_faculty'),"measured by the number of academics who have won major international awards, prizes, and medals"),
                                          tags$li(strong('publications'),"measured by the number of research papers appearing in reputable journals"),
                                          tags$li(strong('influence'),"measured by the number of research papers appearing in highly-influential journals"),
                                          tags$li(strong('citations'),"measured by the number of highly-cited research papers"),
                                          tags$li(strong('broad_impact'),"measured by the university's h-index"),
                                          tags$li(strong('patents'),"measured by the number of international patent filings")
                                        )
                                 )
                        )
                      )
               )
               
             )
    )
  )
)