shinyUI(
  navbarPage("Student Loan Data Analysis", 
             
             tabPanel("Static Data", 
                      fluidPage(titlePanel("FFELP Static Pool Data"), theme = shinytheme("united"),
                                fluidRow(
                                  column(2, 
                                         selectInput("bond", "Choose Bond",
                                                     unique(as.character(bond.data$Tranche)), multiple = FALSE),
                                         selectizeInput("date", "Month and Year", 
                                                        unique(as.character(bond.data$variable)), multiple = FALSE)
                                  ),
                                  column(7, plotOutput('p1'), 
                                         br(),
                                         plotOutput('p2')),
                                  column(3,  
                                         checkboxGroupInput("delinquency", 
                                                            label = h3("Delinquency Options"),
                                                            choices = unique(as.character(bond.data[14:27,3])), selected =1),
                                         checkboxGroupInput("repayment", 
                                                            label = h3("Repayment Status"),
                                                            choices = unique(as.character(bond.data[c(7,8,9,29,31,32,33),3])), selected =1)
                                  ))
                      ) # End of fluid row
             ),
             tabPanel("Time Series",
                      fluidPage(titlePanel("FFELP Prepayment Data"), theme = shinytheme("united"),
                                fluidRow(
                                  column(2,
                                         selectInput("bond1", "Choose Bond", unique(as.character(bond.data$Tranche)), multiple = FALSE)
                                  ),
                                  column(7, plotOutput('p3'), 
                                         helpText("CPR (1): School, Grace, Deferment and Forbearance loans are not scheduled to make payments. Repayment loans are scheduled to make payments."),
                                         helpText("CPR (2): School and Grace loans are not scheduled to make payments. Deferment, Forbearance and Repayment loans are scheduled to make payments."),
                                         helpText("Since Issued CPR: pre-payment speed estimate based on the current period's ending pool balance calculated against the original pool balance of a particular issue."),
                                         helpText("CPR = 1 - (SF_1 * SF_2 * SF_3)^4"),
                                         helpText("SF_n = (APB_n / PPB_n"),
                                         helpText("APB = Actual month-end Pool Balance"),
                                         helpText("PPB = Projected month-end Pool Balance")
                                  ),
                                  column(3,
                                         checkboxGroupInput("cpr", label = h3("CPR Data"),choices = unique(as.character(bond.data[34:36,3]))
                                         )
                                  )
                                )       
                      ))
  )
)
