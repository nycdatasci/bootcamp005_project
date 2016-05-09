library(ggplot2)
shinyServer(function(input, output) {
  # a large table, reactive to input$show_vars 
  data_input= reactive({
    df=loan[loan$state_full==input$state,input$show_vars, drop = FALSE]
    return(df)
  })
  #data_radio=reactive({
  #  df_radio=loan[loan$state_full==input$state,input$radio,drop=FALSE]
   # return(df_radio)
 # })
 # radio_select=reactive({
  #  return(loan$input$radio)
    
 # })

  output$mytable1 = renderDataTable({
    data_input()
    #loan[loan$state_full==input$state,input$show_vars, drop = FALSE]
  })
  
  # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- renderPlot({
    #print (data_input())
    #print(input$radio)
    #data_input()$Borrower_Gender
    p = ggplot(data_input(),aes(x=data_input()$Borrower_Gender )) + 
      geom_bar(aes(fill= Borrower_Gender)) + 
      ggtitle("Bar Charts")+labs(y="Number",x="")
    p
  })   
  output$race <- renderPlot({
    #print (data_input())
    #print(input$radio)
    #data_input()$Borrower_Gender
    r = ggplot(data_input(),aes(x=data_input()$Borrower1_Race_National_Origin)) + 
      geom_bar(aes(fill= Borrower1_Race_National_Origin)) + 
      ggtitle("Bar Charts")+labs(y="Number",x="")
    r
  }) 
  # customize the length drop-down menu; display 5 rows per page by default 
  output$mytable3 = renderDataTable({
    loan
  }, options = list(aLengthMenu = c(10,50, 100), iDisplayLength = 50))
  
}) 