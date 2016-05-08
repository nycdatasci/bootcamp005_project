
library(shiny)
library(shinythemes)



shinyUI(navbarPage(title = "Meta Kaggle",
                   id = "nav",
                   theme = shinytheme("united"),
                   
                   tabPanel("Forward",
                            column(3),
                            column(6,
                            h4("Is group size associated with success in Kaggle competitions? 
                               \nIs the number of competitions competed in associated with ones rank in Kaggle competions?"),
                            br("Kaggle is a community of data scientists that compete with each other to solve complex data science problems."),
                            br("The Meta Kaggle dataset contains data on all the Kaggle competions and who competed in them."),
                            br("The graphs in this app only show users and teams that 
                               actually competed in Kaggle competitions 
                               and is a subset of the overall Meta Kaggle data.")
                            )),
                   
                   tabPanel("Rank by Team Size Total",
                            column(3,
                                   h3("Kaggle Teams by Rank"),
                                   sliderInput("rankingtot",h4("Competition Rank"), min = 1, 
                                               max = 100, value = c(1, 10)),
                                   hr(),
                                   checkboxGroupInput("teamstot", label = h3("Team Group Size"), 
                                                      choices = list("1 Team Member" = 1,
                                                                     "2 Team Members" = 2, 
                                                                     "3 Team Members" = 3,
                                                                     "4 Team Members" = 4,
                                                                     "5 Team Members" = 5,
                                                                     "6 Team Members" = 6,
                                                                     "7 Team Members" = 7,
                                                                     "8 Team Members" = 8,
                                                                     "10 Team Members" = 10,
                                                                     "11 Team Members" = 11,
                                                                     "12 Team Members" = 12,
                                                                     "15 Team Members" = 15,
                                                                     "23 Team Members" = 23,
                                                                     "24 Team Members" = 24
                                                                     ),
                                                      selected = c(1,2,3,4,5,6,7,8,10,11,12,15,23,24))
                            ),
                            column(6,
                                   plotOutput("teamgraphtot", width=700,height=400),
                                   selectInput("selectgraph", label = h3("Graph Select"), 
                                               choices = c("Percent of Team Size" = 1,
                                                           "Proportion of Team Size" = 2)),
                                   br("This graph allows you to compare the different sized Kaggle teams and how they ranked in the competitions."),
                                   br("Using the check boxes you can compare specific groups against each other and see which team sizes did better by the precent of teams at those ranks. 
                                      You can also view this information by the proportion of the teams."),
                                   br("The team sizes have been weighted by the number of team sizes that participated in competions."),
                                   br("You can see these numbers in the table to the right for comparison.")
                                   ),
                            column(3,
                                   h3("Number of Teams by Group size"),
                                   tableOutput('teamtable')
                            ) 
                            
                            
                            ),
                   
                   tabPanel("Rank by Team Size by Competition",
                            column(3,
                                   h3("Kaggle Teams by Rank"),
                                   sliderInput("ranking",h4("Competition Rank"), min = 1, 
                                               max = 100, value = c(1, 10)),
                                   hr(),
                                   checkboxGroupInput("teams", label = h3("Team Group Size"), 
                                                      choices = list("1 Team Member" = 1,
                                                                     "2 Team Members" = 2, 
                                                                     "3 Team Members" = 3,
                                                                     "4 Team Members" = 4,
                                                                     "5 Team Members" = 5,
                                                                     "6 Team Members" = 6,
                                                                     "7 Team Members" = 7,
                                                                     "8 Team Members" = 8,
                                                                     "10 Team Members" = 10,
                                                                     "11 Team Members" = 11,
                                                                     "12 Team Members" = 12,
                                                                     "15 Team Members" = 15,
                                                                     "23 Team Members" = 23,
                                                                     "24 Team Members" = 24
                                                      ),
                                                      selected = c(1,2,3,4,5,6,7,8,10,11,12,15,23,24))
                            ),
                            column(6,
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   plotOutput("teamgraph", width=700,height=400),
                                   br("This graph allows you to compare the different sized Kaggle teams and how they ranked in specific competitions."),
                                   br("The team sizes have been weighted by the number of team sizes that participated in the selected competition.")),
                            column(3,
                                   selectInput("select", label = h3("Competition Select"), 
                                               choices = c(unique(tg.final$Title.x))),
                                   br(),
                                   h3("Competition Description"),
                                   textOutput("value"),
                                   br(),
                                   h3("Competition Date/Time"),
                                   textOutput("date")
                                   )
                                   
                            
                            ),
                   
                   tabPanel("Rank by Team Leader Experience Total",
                            column(3,
                                   h3("Kaggle Teams by Rank"),
                                   sliderInput("ranking2tot",h4("Competition Rank"), min = 1, 
                                               max = 100, value = c(1, 10)),
                                   hr(),
                                   checkboxGroupInput("compstot", label = h3("Number of Competitions
                                                                          Participated in"), 
                                                      choices = list("1 - 10" = "1-10",
                                                                     "11 - 20" = "11-20", 
                                                                     "21 - 30" = "21-30",
                                                                     "31 - 40" = "31-40",
                                                                     "41 - 50 " = "41-50",
                                                                     "51 - 60" = "51-60",
                                                                     "61 - 70" = "61-70",
                                                                     "71 - 80" = "71-80",
                                                                     "81 - 90" = "81-90",
                                                                     "91 - 100" = "91-100"
                                                      ),
                                                      selected = c("1-10","11-20","21-30","31-40",
                                                                   "41-50","51-60","61-70","71-80",
                                                                   "81-90","91-100"))
                            ),
                            column(6,
                                   plotOutput("compgraphtot", width=700,height=400),
                                   selectInput("selectgraph2", label = h3("Graph Select"), 
                                               choices = c("Percent of Competitions" = 1,
                                                           "Proportion of Competitions" = 2)),
                                   br("This graph allows you to compare the different number of competitions the leader of the team has participated in and how the team ranked in the competitions."),
                                   br("The number of competitions participated in have been weighted by the number of leaders that participated in competions."),
                                   br("You can see these numbers in the table to the right for comparison.")
                                   ),
                            column(3,
                                   h3("Number of Team Leaders \nby Competions Participated"),
                                   tableOutput('comptable')
                            ) 
                            ),
                   
                   tabPanel("Rank by Team Leader Experience by Competition",
                            column(3,
                                   h3("Kaggle Teams by Rank"),
                                   sliderInput("ranking2",h4("Competition Rank"), min = 1, 
                                               max = 100, value = c(1, 10)),
                                   hr(),
                                   checkboxGroupInput("comps", label = h3("Number of Competitions
                                                                          Participated in"), 
                                                      choices = list("1 - 10" = "1-10",
                                                                     "11 - 20" = "11-20", 
                                                                     "21 - 30" = "21-30",
                                                                     "31 - 40" = "31-40",
                                                                     "41 - 50 " = "41-50",
                                                                     "51 - 60" = "51-60",
                                                                     "61 - 70" = "61-70",
                                                                     "71 - 80" = "71-80",
                                                                     "81 - 90" = "81-90",
                                                                     "91 - 100" = "91-100"
                                                      ),
                                                      selected = c("1-10","11-20","21-30","31-40",
                                                                   "41-50","51-60","61-70","71-80",
                                                                   "81-90","91-100"))
                                   ),
                            column(6,
                                   br(),
                                   br(),
                                   br(),
                                   br(),
                                   plotOutput("compgraph"),
                                   br("This graph allows you to compare the different number of competitions competed in by the leader of the team and how the team ranked in specific competitions."),
                                   br("The competitions participated in have been weighted by the number of leaders that participated in the selected competition.")),
                            column(3,
                                   selectInput("select2", label = h3("Competition Select"), 
                                               choices = c(unique(tc.final$Title))),
                                   br(),
                                   h3("Competition Description"),
                                   textOutput("value2"),
                                   br(),
                                   h3("Competition Date/Time"),
                                   textOutput("date2")
                                   
                                   
                                   )
)
                   
                   ))
