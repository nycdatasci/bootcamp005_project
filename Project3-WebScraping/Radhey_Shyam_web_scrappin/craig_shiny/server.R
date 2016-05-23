library(ggplot2)
library(dplyr)
library(wordcloud)
shinyServer(function(input, output) {
  # a large table, reactive to input$show_vars 
  data_input= reactive({
    #df=loan[loan$state_full==input$state,input$show_vars, drop = FALSE]
    df=filter(loan,(state.full==input$state))
              #&(Year==input$year)) %>% select_(.dots = input$show_vars)
    return(df)

  })
  data_input_co= reactive({
    #df=loan[loan$state_full==input$state,input$show_vars, drop = FALSE]
    df2=filter(loan2,(state.full==input$state))
    #print (input$state)
    #df2=loan2[loan2$state.full==input$state,]
    #&(Year==input$year)) %>% select_(.dots = input$show_vars)
    return(df2)
    
  })
  
  # output$sel_data = renderDataTable({
  #   data_input()
  #   #loan[loan$state_full==input$state,input$show_vars, drop = FALSE]
  # })
  # 
  # sorted columns are colored now because CSS are attached to them
  output$week <- renderPlot({
    
    #print(input$show_vars)
    #data_input()$Borrower_Gender
    w=ggplot(data=temp)+geom_bar(aes(x=reorder(week_day,week_day,function(x)length(x))),fill="#FF9999", colour="black") + ggtitle("Weekday of Posting Craiglist Ad")+ labs(y="Count",x="") + scale_fill_discrete(name ="")
    w
  })  
  output$time_posting <- renderPlot({
    
    h2=ggplot(data=temp)+geom_bar(aes(x=time_24,fill="#FF9999"))+
      coord_cartesian(xlim = c(0,24))+xlab("Time of posting Ad(round to hour)")+facet_wrap(~week_day,ncol(3))
    h2
  }) 
  
  output$week_time_violin <- renderPlot({
    t2 = ggplot(data = temp, aes(x =reorder(week_day,week_day,function(x)length(x)), y = time_24,fill=week_day)) + geom_violin()+xlab("")+ylab("count")+ scale_fill_discrete(name ="")
    t2
   
  }) 
  
  
  
 
  
  output$word_cloud <- renderPlot({
    #print (data_input())
    wordcloud(word_cloud, scale=c(10,3),min.freq=10,max.words=100, random.order=FALSE, random.color=FALSE, rot.per=0.15,colors= brewer.pal(8, "Paired"))
    
  }) 
  
  output$year_cloud <- renderPlot({
  wordcloud(title_years2, scale=c(4,2),min.freq=10,max.words=100, random.order=FALSE, random.color=FALSE, rot.per=0.15,colors= brewer.pal(8, "Paired"))
  }) 
  # customize the length drop-down menu; display 5 rows per page by default 
  output$data = renderDataTable({
    crg_cars
  }, options = list(aLengthMenu = c(10,50, 100), iDisplayLength = 50))
  
}) 