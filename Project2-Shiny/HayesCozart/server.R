library(shiny)
library(dplyr)
library(ggplot2)



shinyServer(function(input, output) {

  
#Tab 1  
    
  ranktotdata <- reactive({
    df <- tg.final %>%
      filter(Ranking.x >= input$rankingtot[1],
             Ranking.x <= input$rankingtot[2],
             TeamSize %in% input$teamstot)
  })
  
  output$teamgraphtot <- renderPlot(
    
    if(input$selectgraph == 1){
      
    ggplot(ranktotdata(),aes(x = as.factor(Ranking.x)))+
      geom_bar(aes(fill = TeamSize, weight = NewTeamSize), position ="dodge")+
      labs(title = "Kaggle Competion Rank by Team Size", x = "Kaggle Competition Rank",
           y = "Percent of Team Size")+ 
      theme(axis.title=element_text(size=16), plot.title =element_text(size=20),
            legend.title=element_text(size=14))+
      scale_fill_discrete(name = "Team Size")
    } else {
      ggplot(ranktotdata(),aes(x = as.factor(Ranking.x)))+
        geom_bar(aes(fill = TeamSize, weight = NewTeamSize), position ="fill")+
        labs(title = "Kaggle Competion Rank by Team Size", x = "Kaggle Competition Rank",
             y = "Weighted Proportion of Team Size")+ 
        theme(axis.title=element_text(size=16), plot.title =element_text(size=20),
              legend.title=element_text(size=14))+
        scale_fill_discrete(name = "Team Size")
    }
    
  )
  
  output$teamtable <- renderTable(tg.final %>%
                                group_by(.,TeamSize)%>%
                                summarise(., "Number of Teams" = n())
                              )
  
  
  
  
  
#Tab 2
    
  rankdata <- reactive({
    df <- weightdata() %>%
      filter(Ranking.x >= input$ranking[1],
             Ranking.x <= input$ranking[2],
             TeamSize %in% input$teams)
  })
  
  weightdata <- reactive({
  df <- tg.final %>%
    filter(Title.x == input$select)%>%
    group_by(.,TeamSize)%>%
    summarise(., TeamSizePop = n())%>%
    left_join(tg.final,.,by = c("TeamSize") )%>%
    mutate(., NewTeamSize = 1/TeamSizePop.y)
  })
  
  
  
  
  output$teamgraph <- renderPlot(
    
   ggplot(rankdata(),aes(x = as.factor(Ranking.x)))+
      geom_bar(aes(fill = TeamSize, weight = NewTeamSize), position ="stack")+
     labs(title = "Kaggle Competion Rank by Team Size", x = "Kaggle Competition Rank",
          y = "Weighted Proportion by Team Size")+ 
     theme(axis.title=element_text(size=16), plot.title =element_text(size=20),
           legend.title=element_text(size=14))+
     scale_fill_discrete(name = "Team Size")
     
  )

  output$value = renderText({ 
    unique(filter(tg.final,Title.x == input$select)%>%
             .$BriefDescription)
  })
  
  output$date = renderText({ 
    unique(filter(tg.final,Title.x == input$select)%>%
             .$DateEnabled)
  })
  
  
#Tab 3
  
  rankdata2tot <- reactive({
    df <- tc.final %>%
      filter(Ranking >= input$ranking2tot[1],
             Ranking <= input$ranking2tot[2],
             CompGroup %in% input$compstot)
  })
  
  output$compgraphtot <- renderPlot(
    
    if(input$selectgraph2 == 1){
    
    ggplot(rankdata2tot(),aes(x = as.factor(Ranking)))+
      geom_bar(aes(fill = CompGroup, weight = NewNComp), position ="dodge")+
      labs(title = "Kaggle Competion Rank by Team Leader Experience", x = "Kaggle Competition Rank",
           y = "Percent of Number of Competitions")+ 
      theme(axis.title=element_text(size=16), plot.title =element_text(size=20),
            legend.title=element_text(size=14))+
      scale_fill_discrete(name = "Number of \nCompetitions")
  
    }else{
      
      ggplot(rankdata2tot(),aes(x = as.factor(Ranking)))+
        geom_bar(aes(fill = CompGroup, weight = NewNComp), position ="fill")+
        labs(title = "Kaggle Competion Rank by Team Leader Experience", x = "Kaggle Competition Rank",
             y = "Proportion of Number of Competitions")+ 
        theme(axis.title=element_text(size=16), plot.title =element_text(size=20),
              legend.title=element_text(size=14))+
        scale_fill_discrete(name = "Number of \nCompetitions")
      
    }
      
      
      
      )
  
  output$comptable <- renderTable(tc.final %>%
                                    group_by(.,CompGroup)%>%
                                    summarise(., "Number of Team Leaders" = n())
  )
  
  
  
  
#Tab 4
  
  rankdata2 <- reactive({
    df <- expdata() %>%
      filter(Ranking >= input$ranking2[1],
             Ranking <= input$ranking2[2],
             CompGroup %in% input$comps)
  })
  
  expdata <- reactive({
    df <- tc.final %>%
      filter(Title == input$select2)%>%
      group_by(.,CompGroup)%>%
      summarise(., CompetitionsePop = n())%>%
      left_join(tc.final,.,by = c("CompGroup") )%>%
      mutate(., NewNComp = 1/CompetitionsePop.y)
  })
  

  
  
  output$compgraph <- renderPlot(
    
    ggplot(rankdata2(),aes(x = as.factor(Ranking)))+
      geom_bar(aes(fill = CompGroup, weight = NewNComp), position ="stack")+
      labs(title = "Kaggle Competion Rank by Team Leader Experience", x = "Kaggle Competition Rank",
           y = "Proportion by Number of Competitions")+ 
      theme(axis.title=element_text(size=16), plot.title =element_text(size=20),
            legend.title=element_text(size=14))+
      scale_fill_discrete(name = "Number of \nCompetitions")
  )
  
  output$value2 = renderText({ 
    unique(filter(tc.final,Title == input$select2)%>%
             .$BriefDescription)
  })
  
  output$date2 = renderText({ 
    unique(filter(tc.final,Title == input$select2)%>%
             .$DateEnabled)
  
  })
   
    
     
  })
