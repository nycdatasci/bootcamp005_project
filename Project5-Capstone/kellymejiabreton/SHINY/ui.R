## ui.R ##
library(shinydashboard)

dashboardPage(
        dashboardHeader(),
        dashboardSidebar(),
        dashboardBody(
                column(6,
                       verbatimTextOutput("title")
                )
        )
)