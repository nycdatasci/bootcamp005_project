library(googleVis)
library(ggplot2)
library(AnomalyDetection)
library(signal)
shinyServer(function(input, output){
  
  dataframe<-reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    tab = read.csv(inFile$datapath, header = input$header,
                   sep = input$sep, quote = input$quote)
    

    tt <- tryCatch(as.POSIXct(tab[,1]),error=function(e) e, warning=function(w) w)
    if (is(tt,"warning") | is(tt,"error")) {
      tab$Old = tab[,1]
      tab[,1] = as.POSIXct(1:nrow(tab), origin = Sys.time())
    } else {
      tab[,1] = as.POSIXct(tab[,1])
    }
    
    
    tab
  })
  
  output$dataChart <- renderGvis({
    if (!is.null(dataframe())) 
      gvisLineChart(dataframe()[,c(1,2)], xvar = colnames(dataframe())[1], yvar = colnames(dataframe())[2],
                    options = list(
                      crosshair.trigger = 'selection',
                      enableInteractivity = FALSE,
                      hAxis.maxTextLines = 10,
                      tooltip.trigger = 'none'
                    ))
  })
  
  output$contents <- renderGvis({
    if (!is.null(dataframe()))
      gvisTable(dataframe(), 
                options = list(page='enable'))
  })
  
  output$dataChartFiltered <- renderGvis({
    if (input$filt == "none") {
      return(NULL)
    } else if (input$filt == "butt") {
      bf <- butter(as.numeric(input$buttern), as.numeric(input$butterf), type = input$buttert)
      filtered = data.frame(timestamp = dataframe()[,1], 
                            count = as.numeric(filter(bf, dataframe()[,2])))
      gvisLineChart(filtered, xvar = colnames(filtered)[1], yvar = colnames(filtered)[2],
                    options = list(
                      crosshair.trigger = 'selection',
                      enableInteractivity = FALSE,
                      hAxis.maxTextLines = 10,
                      tooltip.trigger = 'none'
                    ))
    } else if (input$filt == "cheby2") {
      ch <- cheby2(as.numeric(input$chebyn), as.numeric(input$chebyd), 
                   as.numeric(input$chebyf), type = input$chebyt)
      filtered = data.frame(timestamp = dataframe()[,1], 
                            count = as.numeric(filter(ch, dataframe()[,2])))
      gvisLineChart(filtered, xvar = colnames(filtered)[1], yvar = colnames(filtered)[2],
                    options = list(
                      crosshair.trigger = 'selection',
                      enableInteractivity = FALSE,
                      hAxis.maxTextLines = 10,
                      tooltip.trigger = 'none'
                    ))
    }
    
  })
  
  output$dataChartAnoms <- renderPlot({
    tab = dataframe()
    if (input$anomalyd != "none") {
      if (input$filt == "none") {
        tab = dataframe()
      } else if (input$filt == "butt") {
        bf <- butter(as.numeric(input$buttern), as.numeric(input$butterf), type = input$buttert)
        tab = data.frame(timestamp = dataframe()[,1], 
                              count = as.numeric(filter(bf, dataframe()[,2])))
      } else if (input$filt == "cheby2") {
        ch <- cheby2(as.numeric(input$chebyn), as.numeric(input$chebyd), 
                     as.numeric(input$chebyf), type = input$chebyt)
        tab = data.frame(timestamp = dataframe()[,1], 
                              count = as.numeric(filter(ch, dataframe()[,2])))
      }
      stamped = data.frame(timestamp = as.POSIXct(tab[,1]),
                           count = tab[,2])
      res = AnomalyDetectionTs(stamped, max_anoms=as.numeric(input$anomalym),
                               direction=input$anomalyd, 
                               alpha = as.numeric(input$anomalya),
                               plot=TRUE)
      print(res$plot)
    }
  })
  
  output$anomsit <- renderGvis({
    tab = dataframe()
    if (input$anomalyd != "none") {
      if (input$filt == "none") {
        tab = dataframe()
      } else if (input$filt == "butt") {
        bf <- butter(as.numeric(input$buttern), as.numeric(input$butterf), type = input$buttert)
        tab = data.frame(timestamp = dataframe()[,1], 
                         count = as.numeric(filter(bf, dataframe()[,2])))
      } else if (input$filt == "cheby2") {
        ch <- cheby2(as.numeric(input$chebyn), as.numeric(input$chebyd), 
                     as.numeric(input$chebyf), type = input$chebyt)
        tab = data.frame(timestamp = dataframe()[,1], 
                         count = as.numeric(filter(ch, dataframe()[,2])))
      }
      stamped = data.frame(timestamp = as.POSIXct(tab[,1]),
                           count = tab[,2])
      res = AnomalyDetectionTs(stamped, max_anoms=as.numeric(input$anomalym),
                               direction=input$anomalyd, 
                               alpha = as.numeric(input$anomalya),
                               plot=F)
      if (!is.null(dataframe()))
        gvisTable(res$anoms, 
                  options = list(page='enable'))
    }
  })
  
  output$downloadData <- downloadHandler(
      filename = function() { paste('output', '.csv', sep='') },
      content = function(file) {
        tab = dataframe()
        if (input$anomalyd != "none") {
          if (input$filt == "none") {
            tab = dataframe()
          } else if (input$filt == "butt") {
            bf <- butter(as.numeric(input$buttern), as.numeric(input$butterf), type = input$buttert)
            tab = data.frame(timestamp = dataframe()[,1], 
                             count = as.numeric(filter(bf, dataframe()[,2])))
          } else if (input$filt == "cheby2") {
            ch <- cheby2(as.numeric(input$chebyn), as.numeric(input$chebyd), 
                         as.numeric(input$chebyf), type = input$chebyt)
            tab = data.frame(timestamp = dataframe()[,1], 
                             count = as.numeric(filter(ch, dataframe()[,2])))
          }
          stamped = data.frame(timestamp = as.POSIXct(tab[,1]),
                               count = tab[,2])
          res = AnomalyDetectionTs(stamped, max_anoms=as.numeric(input$anomalym),
                                   direction=input$anomalyd, 
                                   alpha = as.numeric(input$anomalya),
                                   plot=F)
          write.csv(res$anoms, file, row.names = F)
        }
        
      }

  )
})
