shinyServer(function(input, output, session) {

    plot_brand <- reactive({
      return (filter(top10, Brand %in% input$brandname))
    })
    plot_size <- reactive({
      return (filter(size, Size_Interval %in% input$sizeinterval))
    })
    plot_price <- reactive({
      return (filter(price, Price_Interval %in% input$priceinterval))
    })
    plot_package <- reactive({
      return (filter(package, Package %in% input$packagename))
    })
  
    
    output$top10plot <- renderPlotly({
      g <- ggplot(data = plot_brand(), aes(x=Time, y=Revenue)) + 
        geom_point(aes(color = Brand)) + 
        geom_line(aes(color = Brand, group = Brand)) + 
        ylab("Revenue in ￥10,000")
      ggplotly(g)
    })
    output$sizeplot <- renderPlotly({
      g_size <- ggplot(data = plot_size(), aes(x=Time, y=Revenue)) + 
        geom_point(aes(color = Size_Interval)) + 
        geom_line(aes(color = Size_Interval, group = Size_Interval)) + 
        ylab("Revenue in ￥10,000")
      ggplotly(g_size)
    })
    output$priceplot <- renderPlotly({
      g_price <- ggplot(data = plot_price(), aes(x=Time, y=Revenue)) + 
        geom_point(aes(color = Price_Interval)) + 
        geom_line(aes(color = Price_Interval, group = Price_Interval)) + 
        ylab("Revenue in ￥10,000")
      ggplotly(g_price)
    })
    output$packageplot <- renderPlotly({
      g_package <- ggplot(data = plot_package(), aes(x=Time, y=Revenue)) + 
        geom_point(aes(color = Package)) + 
        geom_line(aes(color = Package, group = Package)) +
        ylab("Revenue in ￥10,000")
      ggplotly(g_package)
    })
  
  })
  
