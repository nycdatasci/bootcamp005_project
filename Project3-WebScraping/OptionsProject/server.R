library(googleVis)
library(ggplot2)
library(rparse)
library(shiny)
library(wordcloud)
library(BBmisc)

shinyServer(function(input, output) {
  
  Sys.setenv(PARSE_APPLICATION_ID = "7PdlFRkYtj5kmDaJNIQfGKHOVRCajQKVEfRBWB1e")
  Sys.setenv(PARSE_API_KEY = "0r1owyGri3p7uyo7UX5kivlkhJshtH169KWHGLqk")
  Sys.setenv(PARSE_MASTER_KEY = "8dHrxPljlhjB2QJuV0YGrY5tUh7MptdDYnJ3sg6a")
  
  dat = as.data.frame(parse_query('Yearly', limit = 1))
  daily = data.frame(
    avgPolsDaily = dat['avgPolsDaily'][[1]][[1]],
    datesDaily = as.character(dat['datesDaily'][[1]][[1]]),
    bearsDaily = dat['bearsDaily'][[1]][[1]],
    bullsDaily = dat['bullsDaily'][[1]][[1]],
    negTwitsDaily = dat['negTwitsDaily'][[1]][[1]],
    posTwitsDaily = dat['posTwitsDaily'][[1]][[1]],
    totalTwitsDaily = dat['totalTwitsDaily'][[1]][[1]]
  )
  
  daily['datesDaily'] = as.character(daily['datesDaily'][[1]])
  daily['datesDaily'] = as.POSIXct(daily['datesDaily'][[1]])
  daily['percentBulls'] = as.numeric(daily['bullsDaily'][[1]]) / (as.numeric(daily['bullsDaily'][[1]]) + as.numeric(daily['bearsDaily'][[1]])) * 100
  daily['percentPos'] = as.numeric(daily['posTwitsDaily'][[1]]) / (as.numeric(daily['negTwitsDaily'][[1]]) + as.numeric(daily['posTwitsDaily'][[1]])) * 100
  daily['avgPolsDailyScaled'] = daily['avgPolsDaily'] * 100
  
  tally = data.frame(
    bearsTally = dat['bearsTally'][[1]][[1]],
    bullsTally = dat['bullsTally'][[1]][[1]],
    datesTally = dat['datesTally'][[1]][[1]],
    negTwitsTally = dat['negTwitsTally'][[1]][[1]],
    posTwitsTally = dat['posTwitsTally'][[1]][[1]],
    totalTwitsTally = dat['totalTwitsTally'][[1]][[1]]
  )
  
  tally['datesTally'] = as.character(tally['datesTally'][[1]])
  tally['datesTally'] = as.POSIXct(tally['datesTally'][[1]]) - 21600
  
  stock = data.frame(
    avgSents = dat['avgSents'][[1]][[1]],
    closes = dat['closes'][[1]][[1]],
    dates = dat['dates'][[1]][[1]],
    highs = dat['highs'][[1]][[1]],
    impactScores = dat['impactScores'][[1]][[1]],
    ivmean10 = dat['ivmean10'][[1]][[1]],
    ivmean20 = dat['ivmean20'][[1]][[1]],
    ivmean30 = dat['ivmean30'][[1]][[1]],
    ivmean60 = dat['ivmean60'][[1]][[1]],
    lows = dat['lows'][[1]][[1]],
    opens = dat['opens'][[1]][[1]],
    volumes = dat['volumes'][[1]][[1]]
  )
  
  stock['dates'] = as.character(stock['dates'][[1]])
  stock['dates'] = as.POSIXct(stock['dates'][[1]])
  stock['avgSentsScaled'] = normalize(stock['avgSents'],method = "range", c(min(stock['closes'][[1]]) / 1.2, max(stock['closes'][[1]]) / 1.2)) - 10
  stock['ivmean10Scaled'] = normalize(stock['ivmean10'],method = "range", c(min(stock['closes'][[1]]) / 1.2, max(stock['closes'][[1]]) / 1.2)) - 25
  stock['ivmean20Scaled'] = normalize(stock['ivmean20'],method = "range", c(min(stock['closes'][[1]]) / 1.2, max(stock['closes'][[1]]) / 1.2)) - 35
  stock['ivmean30Scaled'] = normalize(stock['ivmean30'],method = "range", c(min(stock['closes'][[1]]) / 1.2, max(stock['closes'][[1]]) / 1.2)) - 45
  stock['ivmean60Scaled'] = normalize(stock['ivmean60'],method = "range", c(min(stock['closes'][[1]]) / 1.2, max(stock['closes'][[1]]) / 1.2)) - 55
  
  dat2 = as.data.frame(parse_query('wordDicts', limit = 1))
  
  wordDicts = data.frame(
    bearsVals = dat2['bearsVals'][[1]][[1]],
    bearsWords = dat2['bearsWords'][[1]][[1]],
    bullsVals = dat2['bullsVals'][[1]][[1]],
    bullsWords = dat2['bullsWords'][[1]][[1]],
    negVals = dat2['negVals'][[1]][[1]],
    negWords = dat2['negWords'][[1]][[1]],
    posVals = dat2['posVals'][[1]][[1]],
    posWords = dat2['posWords'][[1]][[1]]
  )
  
  wordDicts['bearsWords'] = as.character(wordDicts['bearsWords'][[1]])
  wordDicts['bullsWords'] = as.character(wordDicts['bullsWords'][[1]])
  wordDicts['posWords'] = as.character(wordDicts['posWords'][[1]])
  wordDicts['negWords'] = as.character(wordDicts['negWords'][[1]])
  
  output$yearlyChart <- renderGvis({
    gvisLineChart(stock, xvar = colnames(stock)[3], yvar = colnames(stock)[c(-1, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12)],
                  options=list(scaleType='maximized', height = 450))
  })
  
  output$weeklyChart <- renderGvis({
    gvisLineChart(daily, xvar = colnames(daily)[2], yvar = colnames(daily)[c(-1, -2, -3, -4, -5, -6, -7)],
                  options=list(scaleType='maximized', height = 450))
  })
  
  output$hourlyChart <- renderGvis({
    gvisLineChart(tally, xvar = colnames(tally)[3], yvar = colnames(tally)[c(2, 6)])
  })
  
  output$posCloud <- renderPlot({
    wordcloud(wordDicts[,8], wordDicts[,7],
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$negCloud <- renderPlot({
    wordcloud(wordDicts[,6], wordDicts[,5],
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$bullCloud <- renderPlot({
    wordcloud(wordDicts[,4], wordDicts[,3],
              colors=brewer.pal(8, "Dark2"))
  })
  
  output$bearCloud <- renderPlot({
    wordcloud(wordDicts[,2], wordDicts[,1],
              colors=brewer.pal(8, "Dark2"))
  })
  
})
