library(shiny)
library(ggplot2)

shinyServer(function(input, output){

  output$TestPlot1 <- renderPlot({
    ggplot(country.table, aes_string(x="country", y=input$nutrition))+
      geom_bar(stat= "identity")+
      theme_bw()+
      xlab("Focus County")+
      ylab(input$nutrition)+
      title("Average Nutrition Comsuption By Country")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
  
  output$TestPlot2 <- renderPlot({
    ggplot(category.table, aes_string(x="main_category_en", y=input$nutrition))+
      geom_bar(stat= "identity", colour = "grey")+
      theme_bw()+
      xlab("Top Category")+
      ylab(input$nutrition)+
      title("Average Nutrition Comsuption By Product Category")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$TestPlot3 <- renderPlot({
    ggplot(topcountry.food.facts, aes_string(x=input$nutrition2, y="nutrition_score_uk_100g")) +
      geom_jitter(colour = "darkgreen") +
      theme_light(base_size=1)+
      theme_bw()+
      xlab(input$nutrition)+
      ylab("nutrition_score_uk_100g")+
      title("Nutrition Scores by Nutrition Factors")
  })   
})

  