shinyServer(function(input, output) {
  
  #The POV characters selected from input (the selections minus "All" and "Trend estimation")
  povs_selected <- reactive({
    input$povs[(input$povs != "All") & (input$povs != "Trend estimation")]
  })
  
  #Boolean for whether or not "All" is selected in input.
  all_selected <- reactive({
    "All" %in% input$povs
  })
  
  #If "All" is selected, we plot all chapters in grey.
  #If "All" is not selected, we plot all chapters in white (invisible).
  #This was the easiest way to make sure that the graphs did not change scale based on selection
  # (because all chapters were always plotted (though sometimes invisible)).
  all_plot <- reactive({
    if(all_selected())
      return("grey")
    else
      return("white")
  })
  
  #Boolean on if "Trend estimation" is selected
  trend_selected <- reactive({
    "Trend estimation" %in% input$povs
  })
  
  #If "Trend estimation" is selected, we plot a smooth approximation.
  trend_plot <- reactive({
    if(trend_selected())
      geom_smooth(se = F)
    else
      NULL
  })
  
  observe({
    output$plot <- renderPlotly({
      
      #Plot all timeseries of all chapters.
      g <- ggplot(asoiaf, aes(x = ChapterNum, y = Score, 
                              label = Chapter, label2 = Book)) +  #labels 1-2 are for tooltip
        geom_line(color = all_plot()) + geom_point(color = all_plot(), size = 1) + 
        trend_plot() + facet_grid(~Book, scales = "free") + theme_bw() + theme(strip.background = element_rect(fill='#FFB01F')) +
        scale_x_continuous(breaks = NULL) + xlab("Chapters") + scale_y_continuous(limits = c(5, 10))
       
      #Plot timeseries for POV characters selected.
      for (pov in povs_selected())
      {
        g = g +
          geom_line(data = dplyr::filter(asoiaf, POV == pov), stat = "identity",
                    color = povcolors[which(levels(asoiaf$POV) == pov)]) +
          geom_point(data = dplyr::filter(asoiaf, POV == pov),
                     color = povcolors[which(levels(asoiaf$POV) == pov)], size = 1)
      }
      ggplotly(tooltip = c("label","y", "label2"))   #Include ChapterName, Score, Book in tooltip
      
      
    })
  })
} )