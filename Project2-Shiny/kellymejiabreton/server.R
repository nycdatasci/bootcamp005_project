library(shiny)
library(ggplot2)
library(wordcloud)
library(leaflet)
library(ggthemes)

shinyServer(function(input, output) {
        
        
        y <- reactive({
                
                #w=as.factor(input$boro)
                select(filter(df2, Borough==input$boro), Complaint.Type, Longitude, Latitude, MONTH, ANNUAL, DAY)
                #switch(input$dataset,
                 #      "Borough" = Borough,
                  #     "Month" = start.MMM,
                   #    "Year" = start.yr)
        })

        #maybe create a function where input$boro is the input
       
        # Fill in the spot we created for a plot
        output$barPlot <- renderPlot({
                # Render a barplot
                
                #ggplot(data = y(), aes(x= input$Fre_q)) + geom_bar()
                
                #w=y()[,input$Fre_q] data[,input$Fre_q]
               
                ggplot(data = y(), aes_string(x= input$Fre_q)) + geom_bar(colour="Green", fill="Blue") +facet_grid(~Borough) + theme_gdocs()
        })
        
        #wordcloud_rep <- repeatable(wordcloud)
        
     

        
        
        
        output$wordCloud1 <- renderPlot({
                v <- y() 
                #par(mar=c(7,1,1,1))
                wordcloud(v$Complaint.Type, scale=c(15,5),min.freq=1,
                          max.words=100, random.order=FALSE, rot.per=0.15, colors= brewer.pal(8, "Paired"), vfont=c("sans serif","bold"))
                
                })
        
        
        output$map <- renderLeaflet({
                u <- y()
                leaflet() %>%
                setView(-73.94197, 40.73638, zoom = 12) %>% 
                addTiles() %>%  # Add default OpenStreetMap map tiles
                #addMarkers(lng=na.omit(df2$Longitude), lat=na.omit(df2$Latitude),  popup=df2$Complaint.Type)
                addMarkers(lng=na.omit(u$Longitude), lat=na.omit(u$Latitude), popup=u$Complaint.Type)
                })

        #select(filter(df2, Borough=="BROOKLYN"), Complaint.Type)
        
        
       
        output$table <- renderTable({
                head(group_by(filter(df, Borough==input$boro), Complaint.Type) %>%
                        summarise(Count=n()) %>%
                        arrange(desc(Count)), n=10) %>%
                        rename(Complaint = Complaint.Type)
                
                })
        })        
