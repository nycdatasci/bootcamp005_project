########################################
#### Project 2 - Shiny Visualization####
#### Ho Fai Wong - May 8, 2016      ####
########################################


## ui.R ##
shinyUI(
  navbarPage(
    title = "2015 World University Rankings",
    id = "nav",
    theme = shinytheme("united"),
    
    #####################
    ####Country Stats####
    #####################
    
    tabPanel("Country Stats",
             
             fluidRow(
               column(width = 3,
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
                                                    "Mean education rank" = 5,
                                                    "Mean alumni employment rank" = 6,
                                                    "Mean faculty rank" = 7,
                                                    "Mean publications rank" = 8,
                                                    "Mean influence rank" = 9,
                                                    "Mean citations rank" = 10,
                                                    "Mean broad impact rank" = 11,
                                                    "Mean patents rank" = 12),
                                     selected = 1)
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
                               h4(paste0('Modify Y-axis criteria')),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterY == 1", #Shanghai
                                 sliderInput("y.sh.rank", "Shanghai rank:", min=0, max=500, value=c(1,500)),
                                 sliderInput("y.sh.alumni", "Alumni score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.award", "Award score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.hici", "HiCi score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.ns", "N&S score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.pub", "PUB score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.sh.pcp", "PCP score:", min=0, max=100, value=c(0,100))
                               ),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterY == 2", #Times
                                 sliderInput("y.t.rank", "Times rank:", min=0, max=400, value=c(1,400)),
                                 sliderInput("y.t.teaching", "Teaching score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.t.international", "International score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.t.research", "Research score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.t.citations_times", "Citations score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("y.t.income", "Income score:", min=0, max=100, value=c(0,100))
                               ),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterY == 3", #CWUR
                                 sliderInput("y.c.rank", "CWUR rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("y.c.education", "Education rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("y.c.alumni", "Alumni employment rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("y.c.faculty", "Faculty rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("y.c.pub", "Publications rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("y.c.influence", "Influence rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("y.c.citations", "Citations rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("y.c.impact", "Broad impact rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("y.c.patents", "Patents rank:", min=0, max=1000, value=c(1,1000))
                               ) ),
                        
                        column(6,
                               h4(paste0('Modify X-axis criteria')),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterX == 1", #Shanghai
                                 sliderInput("x.sh.rank", "Shanghai rank:", min=0, max=500, value=c(1,500)),
                                 sliderInput("x.sh.alumni", "Alumni score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.award", "Award score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.hici", "HiCi score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.ns", "N&S score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.pub", "PUB score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.sh.pcp", "PCP score:", min=0, max=100, value=c(0,100))
                               ),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterX == 2", #Times
                                 sliderInput("x.t.rank", "Times rank:", min=0, max=400, value=c(1,400)),
                                 sliderInput("x.t.teaching", "Teaching score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.t.international", "International score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.t.research", "Research score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.t.citations_times", "Citations score:", min=0, max=100, value=c(0,100)),
                                 sliderInput("x.t.income", "Income score:", min=0, max=100, value=c(0,100))
                               ),
                               
                               conditionalPanel(
                                 condition = "input.sourceScatterX == 3", #CWUR
                                 sliderInput("x.c.rank", "CWUR rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("x.c.education", "Education rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("x.c.alumni", "Alumni employment rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("x.c.faculty", "Faculty rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("x.c.pub", "Publications rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("x.c.influence", "Influence rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("x.c.citations", "Citations rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("x.c.impact", "Broad impact rank:", min=0, max=1000, value=c(1,1000)),
                                 sliderInput("x.c.patents", "Patents rank:", min=0, max=1000, value=c(1,1000))
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
                      box(width = 4,
                          h4("Shanghai Scores (out of 100)"),
                          htmlOutput("bar.shanghai")
                      ),
                      box(width = 4,
                          h4("Times Scores (out of 100)"),
                          htmlOutput("bar.times")),
                      box(width = 4,
                          h4("CWUR Ranks (out of 1000 universities)"),
                          htmlOutput("bar.cwur"))
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
                        tabPanel("Data Sources & Definitions",
                                 column(12,
                                        
                                        strong('Shanghai Rankings'),
                                        tags$ul(
                                          tags$li(strong('world_rank'),' - world rank for university. Contains rank ranges and equal ranks (eg. 101-152). For this analysis, the middle of each range was used.'),
                                          tags$li(strong('alumni'),' - Alumni Score, based on the number of alumni of an institution winning nobel prizes and fields medals.'),
                                          tags$li(strong('award'),' - Award Score, based on the number of staff of an institution winning Nobel Prizes in Physics, Chemistry, Medicine, and Economics and Fields Medals in Mathematics.'),
                                          tags$li(strong('hici'),' - HiCi Score, based on the number of Highly Cited Researchers selected by Thomson Reuters.'),
                                          tags$li(strong('ns'),' - N&S Score, based on the number of papers published in Nature and Science.'),
                                          tags$li(strong('pub'),' - PUB Score, based on total number of papers indexed in the Science Citation Index-Expanded and Social Science Citation Index.'),
                                          tags$li(strong('pcp'),' - PCP Score, the weighted scores of the above five indicators divided by the number of full time academic staff.')
                                        ),
                                        
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
                                        ),
                                        
                                        strong('Center for World University Rankings'),
                                        tags$ul(
                                          tags$li(strong('world_rank'),' - world rank for university.'),
                                          tags$li(strong('national_rank'),' - rank of university within its country.'),
                                          tags$li(strong('quality_of_education - rank for quality of education.'),
                                                  tags$li(strong('alumni_employment'),' - rank for alumni employment.'),
                                                  tags$li(strong('quality_of_faculty'),' - rank for quality of faculty.'),
                                                  tags$li(strong('publications'),' - rank for publications.'),
                                                  tags$li(strong('influence'),' - rank for influence.'),
                                                  tags$li(strong('citations'),' - rank for citations.'),
                                                  tags$li(strong('broad_impact'),' - rank for broad impact (only available for 2014 and 2015)'),
                                                  tags$li(strong('patents'),' - rank for patents.')
                                          )
                                        )
                                 )
                        ),
                        tabPanel("About the dataset",
                                 fluidRow(
                                   column(4,
                                          h4('Summary'),
                                          p("2015 university rankings from 3 organizations covering 1015 institutions across 61 countries were reconciled and analyzed:"),
                                          tags$ul(
                                            tags$li(strong("Shanghai Rankings"),": 500 universities"),
                                            tags$li(strong("Times World University Rankings"),": 401 universities"),
                                            tags$li(strong("Center for World University Rankings"),": 1000 universities")
                                          ),
                                          br(),
                                          p('Missing data, assumed to be Missing At Random, was imputed using K-Nearest Neigbhors with k=sqrt(n).'),
                                          p('In cases where the world rank was a range, the middle of the range was used for visualization purposes.'),
                                          p('Times and Shanghai data contained scores for sub-criteria, but CWUR data contained rankings. For this visualization project, CWUR rankings were not converted to scores.')
                                   ),
                                   column(8,
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
                        )
                      )
               )
             )
    )
  )
)