shinyServer(function(input, output) {
  
  output$text1 <- renderText({ 
    paste("You have selected", input$bond, input$date)
  })
  
  first_filtered<-reactive({
    filtered_data <- filter(bond.data, Tranche %in% input$bond, variable %in% input$date)
  })
  
  second_filtered<-reactive({
    delinquency_data <- filter(first_filtered(), Variable %in% input$delinquency)
  })
  
  third_filtered<-reactive({
    repayment_data <- filter(first_filtered(), Variable %in% input$repayment)
  })
  
  #-------------------------------------------------------------------------
  
  status_filtered<-reactive({
    filtered_data <- filter(bond.data, Tranche %in% input$bond1)
  })


  cpr1_filtered<-reactive({
    cpr1_data <- filter(status_filtered(), Variable %in% input$cpr)
  })
  
  
  
  #-------------------------------------------------------------------------
  output$p1 <- renderPlot({
    g <- ggplot(data = second_filtered(), aes(x = Variable, y = value)) + 
      geom_bar(aes(fill = Variable), stat = "identity", position = "dodge") +
      ggtitle("Delinquency Status") +
      xlab("") +
      ylab("") +
      theme(legend.position = "right") +
      scale_fill_discrete(name = "Repayment") +
      theme(axis.text.x= element_blank(), plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24)) 
    
    g
  })  
  output$p2 <- renderPlot({
    h <- ggplot(data = third_filtered(), aes(x = Variable, y = value)) + 
      geom_bar(aes(fill = Variable), stat = "identity", position = "dodge") +
      ggtitle("Payment Status") +
      xlab("") +
      ylab("") +
      scale_fill_discrete(name = "Repayment") +
      theme(axis.text.x= element_blank(), plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24)) 
    h
  })
  
  output$p3 <- renderPlot({
    j <- ggplot(cpr1_filtered(), aes(x=variable, y=value)) +
    geom_line(group = 1, color = "blue") +
    ggtitle("CPR Time Series") +
    xlab("") +
    ylab(" ") +
    theme(axis.text.x= element_blank(),plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24)) 
    j
  })
})
  