library(shiny)
library(dplyr)
library(plotly)
library(reshape)
library(ggplot2)

fields_2014 = read.csv('data/fields_2014.csv', stringsAsFactors = F)
salaries_2014 = read.csv('data/salaries_2014.csv', stringsAsFactors = F)

shinyServer(
  function(input, output) {
    output$choropleth = renderPlotly({
      l = list(color = toRGB("white"), width = 2)
      g = list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      )
      values = switch(input$field,
                     'All' = fields_2014$Percent,
                     'Science and Engineering' = fields_2014$Science_Engineering,
                     'Science and Engineering Related' = fields_2014$Science_Engineering_Related,
                     'Business' = fields_2014$Business,
                     'Education' = fields_2014$Education,
                     'Arts, Humanities, and Other' = fields_2014$Arts_Humanities_Other)
      
      colors = switch(input$field,
                    'All' = 'RdGy',
                    'Science and Engineering' = 'BrBG',
                    'Science and Engineering Related' = 'PuRd',
                    'Business' = 'PRGn',
                    'Education' = 'PuOr',
                    'Arts, Humanities, and Other' = 'RdBu')
      
      plot_ly(fields_2014, z = format(values, nsmall = 1), text = Geography, locations = State, type = 'choropleth',
              locationmode = 'USA-states', color = Percent, colors = colors,
              marker = list(line = l), colorbar = 
                list(title = "% of Population 25<br>years and over with<br>Bachelor\'s Degree")) %>%
        layout(title = '2014 Percent of Population with Bachelor\'s Degrees by State<br>(Hover for %)',
               geo = g)
    })
    # Field of Bachelor's Degrees bar graph
    output$field_breakdown = renderPlotly({
      colors = switch(input$field,
                      'All' = 'RdGy',
                      'Science and Engineering' = 'BrBG',
                      'Science and Engineering Related' = 'PuRd',
                      'Business' = 'PRGn',
                      'Education' = 'PuOr',
                      'Arts, Humanities, and Other' = 'RdBu')
      
      gg = ggplot(melt(fields_2014[,c(4,7:11)]) %>% filter(State == input$state), aes(x = variable, y = value, fill = variable)) +
          geom_bar(stat = 'identity') + xlab(' ') + ylab('Percent of Bachelor\'s Degrees in State') +
          scale_fill_brewer(palette = colors, name = '',
                            labels = c('Science and Engineering', 'Science and Engineering Related',
                                       'Business', 'Education', 'Arts, Humanities, and Other')) +
          theme(axis.text.x = element_blank()) +
          ggtitle('2014 Fields of Bachelor\'s Degrees in State')
      ggplotly(gg)
      
    })
    # Earnings bar graph
    output$salaries = renderPlotly({
      colors = switch(input$field,
                      'All' = 'RdGy',
                      'Science and Engineering' = 'BrBG',
                      'Science and Engineering Related' = 'PuRd',
                      'Business' = 'PRGn',
                      'Education' = 'PuOr',
                     'Arts, Humanities, and Other' = 'RdBu')
      
      gg = ggplot(melt(salaries_2014[,c(4:15)]) %>%
        filter(State == input$state, variable %in% input$job),
        aes(x = reorder(variable, value), y = value, fill = variable)) +
        geom_bar(stat = 'identity') + xlab('Occupation') +
        ylab('Earnings in Inflation-Adjusted Dollars') +
        scale_fill_brewer(palette = colors, name = '') +
        theme(axis.text.y = element_blank()) +
        ggtitle('2014 Earnings by Occupation') + coord_flip()
      ggplotly(gg)
    })
    
  }
  
)