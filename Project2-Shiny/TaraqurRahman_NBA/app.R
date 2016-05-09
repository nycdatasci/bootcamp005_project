library(shiny)
library(shinydashboard)
library(googleVis)
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

# fluidPage(theme = 'style.css',
ui <- navbarPage(
  theme = shinytheme('spacelab'),
  title = "NBA Stats 1947-2015",
  id = "nav",
  tabPanel("Home"),
  tabPanel("Stat Leaders",
           fluidRow(
             column(4,
                    selectInput("game",
                         "Season or Playoff",
                         choices = c("Season", "Playoffs"),
                         multiple = F)),
             column(4,
                    sliderInput("range",
                                "Range Top Records",
                                min = 1,
                                max = 10,
                                value = c(1,5))
               
             )
             ),
           fluidRow(
             column(12,
                    plotlyOutput("home"))
           ),
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
                                multiple = F)
                    ),
             column(4,
                    selectInput("year",
                                "Year",
                                choices = c(unique(season$year_id))))
             ),
           fluidRow(
             column(12,
                    plotlyOutput("teamplot"))
             ),
           fluidRow(
             column(12,
                    plotlyOutput("win_loss"))
             )
           ),
  tabPanel("Code")
  )



server <- function(input, output) {
  
  #filter dataframe depending on input for season and team
  seasoninput = reactive({
    filter(nba_pts, is_playoffs == input$season, fran_id == input$team)
  })
  
  bestinput = reactive({
    filter(df, is_playoffs == input$game)[input$range[1]:input$range[2] ,]
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
      geom_bar(stat="identity", fill = "#0072B2", color= 'white') +
      labs(title = paste0("Average PPG in ", input$season, collapse = NULL),
           x = " ",
           y = " ") +
      theme_bw() +
      guides(color="none")
    ggplotly(p)
    x = list(title = input$season)
    y = list(title = "Average Points per Game")
    p %>% layout(xaxis = x, yaxis = y)
  })
  
  #create graph for userinput year for each team and season/playoff
  output$win_loss <- renderPlotly({
    g <- ggplot(ratioinput(), aes(x=year_id, y=win_ratio, color=game_location)) +
      geom_line() +
      labs(title = "Winning Percentage for Home and Away",
           x = " ",
           y = " ",
           color = "Location") +
      theme_bw()
    ggplotly(g)
    x = list(title = "Year")
    y = list(title = "Winning Ratio")
    g %>% layout(xaxis = x, yaxis = y)
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
      scale_fill_discrete(name = "Teams")
    ggplotly(q)
    x = list(title = "Year")
    y = list(title = "Number of Wins")
    q %>% layout(xaxis = x, yaxis = y)
  })
  
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
    t %>% layout(xaxis = x, yaxis = y)
  })
}

shinyApp(ui = ui, server = server)

