shinyUI(fluidPage(theme = shinytheme("journal"),
  titlePanel("Chapter Ratings for A Song of Ice and Fire"),
  plotlyOutput('plot'),
  fluidRow(
    column(3,
           
           #This is the interactive portion.
           #Choose POV characters as well as all chapters and a trend curve for each chapter.
           selectInput('povs',
                      'Select POV Characters',
                       c('All', 'Trend estimation', levels(asoiaf$POV)),
                       multiple = TRUE,
                       selected=c('All'),
                       selectize = TRUE)
    ),
    column(3,
           HTML("<br> Created by <a href=\"https://github.com/rtcastellano\" target = \"_blank\">R. Castellano</a>
                <br>Data courtesy of <a href=\"http://towerofthehand.com/books/guide.html\" target = \"_blank\">Tower of the Hand</a>")
    )
  )
))