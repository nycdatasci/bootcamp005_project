library(shiny)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output){

  output$TestPlot1 <- renderPlot({
    ggplot(country.table, aes_string(x="country", y=input$nutrition))+
      geom_bar(stat= "identity", aes(fill = country))+
      theme_bw()+
      xlab("Focus Country")+
      ylab(input$nutrition)+
      title("Average Nutrition Comsuption By Country")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
  
  output$TestPlot2 <- renderPlot({
    ggplot(category.table, aes_string(x="main_category_en", y=input$nutrition))+
      geom_bar(stat= "identity", aes(fill = main_category_en))+
      theme_bw()+
      xlab("Top Category")+
      ylab(input$nutrition)+
      title("Average Nutrition Comsuption By Product Category")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$TestPlot3 <- renderPlot({
    ggplot(topcountry.food.facts, aes_string(x=input$nutrition2,y=input$nutrition3)) +
      geom_jitter(colour = "darkgreen") +
      theme_light(base_size=1)+
      theme_bw()+
      xlab(input$nutrition2)+
      ylab(input$nutrition3)+
      title("Nutrition Relationship")
  })   
  
  output$table <- renderTable({
    a <- select(filter(topcategory.food.facts, main_category_en == input$category & countries_en == input$country), 
                brands,nutrition_score_uk_100g, energy_100g)
    a <- filter(a, brands != "")
    a <- head(arrange(a, desc(nutrition_score_uk_100g)), 15)
    unique(a[,c("brands", "nutrition_score_uk_100g", "energy_100g")])
  })  
  
  output$text1 <- renderText({
    "This data comes from the Open Food Facts database, a free, open, and collaborative database of food products around the world."
  })
  
  output$text2 <- renderText({
    "Open Food Facts gathers information and data on food products from around the world."
  })
  
})

  