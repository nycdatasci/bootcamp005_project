library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(plotly)

#reading intitial csv file
nba_df = read.csv("/Users/tqrahman/Desktop/NBA Shiny App/data/NBA History.csv", header = TRUE)

#mutating a column for difference in points
nba_df = mutate(nba_df, diff = abs(pts-opp_pts))

#finding out the average points per game in a given year
nba_pts = summarize(group_by(nba_df, fran_id, year_id, is_playoffs), ppg = sum(pts)/length(fran_id))
t_df = nba_pts
bp_df = t_df[rev(order(t_df$ppg)),]

#win/loss proportion
nba_wl = summarise(group_by(nba_df, fran_id, year_id, is_playoffs, game_location, game_result), Wins = sum(game_result=='W'), Loss = sum(game_result=='L'))

#best_wins df
nba_best = summarise(group_by(nba_df, fran_id, year_id, is_playoffs), w_ratio = sum(game_result=='W')/(sum(game_result=='W')+sum(game_result=='L')))
temp_df = nba_best
df = temp_df[rev(order(temp_df$w_ratio)),]

#df = ratio of wins and total games
nba_ratio = summarize(group_by(nba_wl, fran_id, year_id, is_playoffs, game_location), win_ratio = sum(Wins)/(sum(Wins)+sum(Loss)))
head(nba_ratio)

#a new df for ppg in a season
season = filter(nba_df, nba_df$is_playoffs == 'Season') %>%
  select(year_id, date_game, team_id, fran_id, pts, opp_id, opp_pts, game_location, game_result, diff, finals)

#a new df for ppg in a playoff
playoffs = filter(nba_df, nba_df$is_playoffs == 'Playoffs') %>%
  select(year_id, date_game, team_id, fran_id, pts, opp_id, opp_pts, game_location, game_result, diff, finals)

#df for number of rings
rings_df = filter(playoffs, finals == 'Y') %>%
  select(fran_id, year_id)
nextrings = unique(rings_df)
nextrings = summarise(group_by(nextrings, fran_id), Total = n())


ui <- navbarPage(
  
  title = "NBA Stats 1947-2015",
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');

                    body {
                    background-color: white;
                    background-image: url('n_1.png');
                    background-size:cover;
                    background-attachment:fixed;
                    }
                    "))
    ),
  tabPanel("Stat Leaders",
           fluidRow(
             column(12,
                    plotlyOutput("rings"))
           ),
           br(),
           br(),
           fluidRow(
             column(6,
                    selectInput("game",
                         "Season or Playoff",
                         choices = c("Season", "Playoffs"),
                         multiple = F)),
             column(6,
                    sliderInput("range",
                                "Range Top Records",
                                min = 1,
                                max = 10,
                                value = c(1,5))
                    )
             ),
           br(),
           fluidRow(
             column(12,
                    plotlyOutput("home"))
           ),
           br(),
           br(),
           fluidRow(
             column(12,
                    plotlyOutput("t_points")
                    ))
           ),
           
  tabPanel("Team",
           fluidRow(
             column(4,
                    selectInput("team",
                                "Team Name",
                                choices = c(levels(season$fran_id)),
                                multiple = F)
                    ),
             column(4,
                    selectInput("season",
                                "Season or Playoff",
                                choices = c("Season", "Playoffs"),
                                multiple = F))
             #        ,
             # column(4,
             #        selectInput("year",
             #                    "Year",
             #                    choices = c(unique(season$year_id))))
             ),
           fluidRow(
             column(12,
                    plotlyOutput("teamplot"))
             ),
           br(),
           br(),
           fluidRow(
             column(12,
                    plotlyOutput("win_loss"))
             )
           ),
  tabPanel("Data",
           fluidRow(
             column(12,
             textInput("inputId",
                       label = NULL,
                       value = "Data: https://www.putdat.com/rsVnH9T"))
           )),
  windowTitle = "NBA Stats",
  theme = "bootstrap.css"
)



server <- function(input, output) {
  
  #filter dataframe depending on input for season and team
  seasoninput = reactive({
    filter(nba_pts, is_playoffs == input$season, fran_id == input$team)
  })
  
  bestinput = reactive({
      filter(df, is_playoffs == input$game)[(input$range[1]):(input$range[2]),]
  })
  
  bpinput = reactive({
    filter(bp_df, is_playoffs == input$game)[input$range[1]:input$range[2] ,]
  })
  
  ratioinput = reactive({
    filter(nba_ratio, is_playoffs == input$season, fran_id == input$team)
  })
  
  #create plot for each team for either playoffs or season
  output$teamplot <- renderPlotly({
    p <- ggplot(seasoninput(), aes(x=year_id, y=ppg)) +
      geom_bar(stat="identity", fill = "#FE2E2E", color= rgb(1,1,1,0.1), width = 0.9) +
      labs(title = paste0("Average PPG in ", input$season, collapse = NULL),
           x = " ",
           y = " ") +
     theme_bw() +
      guides(color="none")
    p <- ggplotly(p)
    x = list(title = input$season)
    y = list(title = "Average Points per Game")
    layout(p, xaxis = x, yaxis = y, paper_bgcolor = "rgba(0,0,0,0)", 
           plot_bgcolor= 'rgba(255,255,255,0.1)', 
           bgcolor= 'rgba(255,255,255,0.1)')
  })
  
  #create graph for userinput year for each team and season/playoff
  output$win_loss <- renderPlotly({
    g <- ggplot(ratioinput(), aes(x=year_id, y=win_ratio, color=game_location)) +
      geom_line() +
      geom_line(aes(y = win_ratio)) +
      labs(title = "Winning Percentage for Home and Away",
           x = " ",
           y = " ",
           color = "Location") +
      theme_bw()
    ggplotly(g)
    x = list(title = "Year")
    y = list(title = "Winning Ratio")
    layout(g, xaxis = x, yaxis = y, paper_bgcolor = "rgba(0,0,0,0)", 
           plot_bgcolor= 'rgba(255,255,255,0.1)', 
           bgcolor= 'rgba(255,255,255,0.1)')
  })
  
  #Best Record First, Second, Third
  output$home <- renderPlotly({
    q <-ggplot(bestinput(), aes(x=year_id, y=w_ratio, fill = fran_id)) +
      geom_bar(stat = "identity", color = "white") +
      labs(title = "Wins",
           x = " ",
           y = " ") +
      scale_x_continuous(breaks=seq(1947, 2015, by = 3)) +
      scale_y_continuous(breaks=seq(0, 1, by = .2)) +
      theme_bw() + 
      scale_fill_discrete(name = "Teams",
                          labels = c("Away", "Home"))
    ggplotly(q)
    x = list(title = "Year")
    y = list(title = "Wins/Total Games")
    layout(q, xaxis = x, yaxis = y, paper_bgcolor = "rgba(0,0,0,0)", 
           plot_bgcolor= 'rgba(255,255,255,0.1)', 
           bgcolor= 'rgba(255,255,255,0.1)')
  })
  
  #Graph for 
  output$t_points <- renderPlotly({
    t <- ggplot(bpinput(), aes(x=year_id, y=ppg, fill = fran_id)) +
      geom_bar(stat = "identity", color = "white") +
      labs(title = "Average PPG",
           x = " ",
           y = " ") +
      theme_bw() + 
      scale_fill_discrete(name = "Teams")
    ggplotly(t)
    x = list(title = "Year")
    y = list(title = "Average PPG")
    layout(t, xaxis = x, yaxis = y, paper_bgcolor = "rgba(0,0,0,0)", 
           plot_bgcolor= 'rgba(255,255,255,0.1)', 
           bgcolor= 'rgba(255,255,255,0.1)')
  })
  
  output$rings <- renderPlotly({
     z <- ggplot(nextrings, aes(x=fran_id, y = Total)) +
      geom_point(aes(color = "FA8320", size = Total)) +
      labs(title = "Total Championships",
           x=" ",
           y=" ") +
       theme_bw() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
       guides(color="none")
     ggplotly(z)
     x = list(title = "Team")
     y = list(title = "Number of Championships")
     layout(z, xaxis = x, 
            yaxis = y,
            showlegend = FALSE,
            paper_bgcolor = "rgba(0,0,0,0)", 
            plot_bgcolor= 'rgba(255,255,255,0.1)', 
            bgcolor= 'rgba(255,255,255,0.1)')
                        
  })
}

shinyApp(ui = ui, server = server)

