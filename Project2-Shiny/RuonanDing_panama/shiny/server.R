#####By Ruonan Ding#########
############################

library(shiny)
library(ggplot2)
library(countrycode)
library(googleVis)
library(shinydashboard)
library(shinythemes)


shinyServer(function(input, output, session) {
  
  output$timeline_overview <- renderPlot({
   ggplot(filter(entities.append2,incorp.year > 1975), aes(incorporation_date))+
      geom_freqpoly(binwidth = 300)+
      theme_bw()+
      xlab("Incorp. Year")+
      ylab("Number of Entities")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  })
  
  output$timeline_adjustment <- renderPlot({
    ggplot(filter(filter(entities.append2,incorp.year > 1975), jurisdiction == input$jurisdiction), aes(incorporation_date))+
      geom_freqpoly(binwidth = 300)+
      theme_bw()+
      xlab("Incorp. Year")+
      ylab("Number of Entities")+
      title("Number of Entities by Jurisdiscrion Over Time")+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  })
  
  output$GeoVis <- renderGvis({
  gvisMap(top20.geochart.data, "country.name", "number_of_entites",
                    options=list(width=600, height=500,
                                 showTip=TRUE,
                                 mapType='normal'))
  })
  
  output$PopTable <- renderGvis({
    gvisTable(mutate(top20.geochart.data, "% of All Entities" = number_of_entites/nrow(entities_finaltable)),
                          formats=list('% of All Entities'='#.#%'),
                          options=list(page='enable'))
  })
  
  output$statustable <- renderGvis({
     gvisTable(arrange(status_tablefinal, desc(number_of_entities)))
  })
 
  output$GeoLayer1 <- renderGvis({
    gvisGeoChart(e, "country.name", "index",
                 options=list(width=800,height=450, colors= "['green']",
                              title = "The Intermediaries Countries that faciliated USA Entities",
                              legend = 'none'))
    
  })
  
  output$GeoLayer2 <- renderGvis({
    gvisGeoChart(g, "juris", "index",
                 options=list(width=800,height=450, colors= "['red']",
                              title = "The Final Jurisdition Countries of USA Entities",
                              legend = 'none'))
  })
  
  output$dataintro <- renderText({
    "The International Consortium of Investigative Journalists will release on May 9 a searchable database with information on more than 200,000 offshore entities that are part of the Panama Papers investigation.
    The data comes from the Panamanian law firm Mossack Fonseca, one of the top players in the offshore world, and includes information about companies, trusts, foundations and funds incorporated in 21 tax havens, from Hong Kong to Nevada in the United States. It links to people in more than 200 countries and territories.
    " })
  
  output$disclaimer <- renderText({
    "There are legitimate uses for offshore companies and trusts. We do not intend to suggest or imply that any persons, companies or other entities included in the ICIJ Offshore Leaks Database have broken the law or otherwise acted improperly."})
  
  output$entites <- renderText({
  "A company, trust or fund created in a low-tax, offshore jurisdiction by an agent."
})
  
  output$intermediary <- renderText({
    "A go-between for someone seeking an offshore corporation and an offshore service provider -- usually a law-firm or a middleman that asks an offshore service provider to create an offshore firm for a client."
  })
  
  output$jurisdiction <- renderText({
"In this database, it is information that relates to the jurisdiction where an entity may have fiscal duties." })

  output$aboutme <- renderUI({
  str1 <- h3("Data Scientist at NYC Data Science Academy;")
  str2 <- h3("Actuarial and financial pricing across banking and insurance sectors;")
  str3 <- h3("Strong statistical analysis skills utilizing various programming tools;")
  str4 <- h3("Currently living in NYC with a rescue cat and an adopted dog.")
  str5 <- h3("Email: dingruonan@gmail.com")
  HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
})
})