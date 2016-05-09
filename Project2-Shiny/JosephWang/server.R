library(googleVis)
shinyServer(function(input, output,session){
    output$plot <-renderGvis({
    gvisBarChart(df <- data.frame(city=c(input$From.city, input$To.city), 
                                  val=c(input$Salary,input$Salary*data$Composite.Index[which(data$Urban.Area==input$To.city)]
                                        /data$Composite.Index[which(data$Urban.Area==input$From.city)]),val.style=c('blue', 'red'))
                                 ,xvar="city",yvar=c("val","val.style"),options=list(title="Salary Comparison by Living Cost",
                                                          hAxes="[{title:'Dollars',textPosition:'out',viewWindow:{min:10000}}]"))
  })
   output$text1 <- renderText(c(as.character(Data.salary2016$city[which(Data.salary2016$city==input$From.city)])))
   
   output$text2 <-renderText(c(as.character(Data.salary2016$city[which(Data.salary2016$city==input$To.city)])))
   
   # output$c2<-renderText(which(Data.salary2016$city==input$To.city)
  #C1<-which(Data.salary2016$city==input$From.city)
  #C2<-which(Data.salary2016$city==input$To.city)
  #if (C1!=0 & C2!=0 ){
   output$plott <-renderGvis({        
    gvisBarChart(dff <- data.frame(city=c(input$From.city, input$To.city), 
                                  val=c(Data.salary2016$salary[which(Data.salary2016$city==input$From.city)]
                                        ,Data.salary2016$salary[which(Data.salary2016$city==input$To.city)]),val.style=c('Gold', 'Purple'))
                 ,xvar="city",yvar=c("val","val.style"),options=list(title="Actual Median Annual Salary for Data Scientist(2016)",
                                                                     hAxes="[{title:'Dollars',textPosition:'out',viewWindow:{min:10000}}]"))
  })
#}
  
  
    output$plot1<-renderGvis({
      gvisColumnChart(df1<-data.frame(
        c("Composite.Index", "Grocery.Items", "Housing,Utilities", 
          "Transportation", "Health.Care", "Miscellaneous", "Goods.and.Services"),From=dput(as.numeric(tdf[2:8,which(data$Urban.Area==input$From.city)])),
        To=dput(as.numeric(tdf[2:8,which(data$Urban.Area==input$To.city)]))
      ),options=list(title="Detailed Cost Comparison in index"))
      })
   # output$plot2<-renderGvis({
    #  (df <- data.frame(city=c(input$From.city, input$To.city), 
     #                          val=c(input$Salary,input$Salary*data$Composite.Index[which(data$Urban.Area==input$To.city)]
      #                               /data$Composite.Index[which(data$Urban.Area==input$From.city)]),locationvar="Urban.Area",tipvar="Composite.Index"))
      output$plot2<-renderGvis({
        gvisMap(df <- data.frame(city=c(input$From.city, input$To.city), 
                                 val=c(input$Salary,input$Salary*data$Composite.Index[which(data$Urban.Area==input$To.city)]
                                       /data$Composite.Index[which(data$Urban.Area==input$From.city)]),locationvar="Urban.Area",tipvar="Composite.Index")) 
    })
    
})