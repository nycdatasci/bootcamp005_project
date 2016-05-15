# server.R
library(shiny)
library(maps)
library(mapproj)
library(shinydashboard)
library(plotly)
source("helpers.R")


shinyServer(
    function(input, output) {
        
        output$map <- renderPlot({
            args <- switch(input$var,
                           "Percent Adult Obese 2009" = list(health2$PCT_OBESE_ADULTS09, "darkgreen", "% Obese"),
                           "Percent Adult Obese 2010" = list(health2$PCT_OBESE_ADULTS10, "darkgreen", "% Obese"),
                           "Percent Adult Diabetic 2009" = list(health2$PCT_DIABETES_ADULTS09, "darkred", "% Diabetic"),
                           "Percent Adult Diabetic 2010" = list(health2$PCT_DIABETES_ADULTS10, "darkred", "% Diabetic"),
                           "Percent Poverty 2010" = list(socioeconomic2$POVRATE10, "blue", "% Poverty"))
                            
            args$min <- input$range[1]
            args$max <- input$range[2]
            args$name <- input$var
            do.call(percent_map, args)
        }, height = 510, width = 900 )
        
        output$trendPlot <- renderPlotly({
            args1 <- switch(input$var1,
                            "Percent Adult Obese 2010" = "PCT_OBESE_ADULTS10",
                            "Percent Adult Diabetic 2010" = "PCT_DIABETES_ADULTS10",
                            "Percent Poverty 2010" = "POVRATE10", 
                            "Median household income, 2010" = "MEDHHINC10", 
                            "Population, low access to store (%), 2010" ="PCT_LACCESS_POP10",
                            "Grocery stores/1,000 pop, 2012" = "GROCPTH12", 
                            "Convenience stores/1,000 pop, 2012" = "CONVSPTH12", 
                            "Fast-food restaurants/1,000 pop, 2012" = "FFRPTH12", 
                            "Full-service restaurants/1,000 pop, 2012" = "FSRPTH12", 
                            "Household food insecurity (%), 2010-12" = "FOODINSEC_10_12", 
                            "Farms with direct sales, 2007" = "DIRSALES_FARMS07", 
                            "Farmers' markets/1,000 pop, 2013" = "FMRKTPTH13", 
                            "Vegetable farms, 2007" = "VEG_FARMS07", 
                            "High schoolers physically active (%), 2009" = "PCT_HSPA09",
                            "Recreation & fitness facilities/1,000 pop, 2012" = "RECFACPTH12", 
                            "% Population 65 years or older, 2010" = "PCT_65OLDER10", 
                            "% Population under age 18, 2010" = "PCT_18YOUNGER10"
            )
            
            args2 <- switch(input$var2,
                            "Percent Adult Obese 2010" = "PCT_OBESE_ADULTS10",
                            "Percent Adult Diabetic 2010" = "PCT_DIABETES_ADULTS10",
                            "Percent Poverty 2010" = "POVRATE10", 
                            "Median household income, 2010" = "MEDHHINC10", 
                            "Population, low access to store (%), 2010" ="PCT_LACCESS_POP10",
                            "Grocery stores/1,000 pop, 2012" = "GROCPTH12", 
                            "Convenience stores/1,000 pop, 2012" = "CONVSPTH12", 
                            "Fast-food restaurants/1,000 pop, 2012" = "FFRPTH12", 
                            "Full-service restaurants/1,000 pop, 2012" = "FSRPTH12", 
                            "Household food insecurity (%), 2010-12" = "FOODINSEC_10_12", 
                            "Farms with direct sales, 2007" = "DIRSALES_FARMS07", 
                            "Farmers' markets/1,000 pop, 2013" = "FMRKTPTH13", 
                            "Vegetable farms, 2007" = "VEG_FARMS07", 
                            "High schoolers physically active (%), 2009" = "PCT_HSPA09",
                            "Recreation & fitness facilities/1,000 pop, 2012" = "RECFACPTH12", 
                            "% Population 65 years or older, 2010" = "PCT_65OLDER10", 
                            "% Population under age 18, 2010" = "PCT_18YOUNGER10"
            )
            
             
            # Create a convenience data.frame which can be used for charting
            plot.df <- data.frame(fulldb[,args1],
                                  fulldb[,args2],
                                  fulldb$County,
                                  fulldb$State,
                                  fulldb$PCT_OBESE_ADULTS10)
            
            # Add column names
            colnames(plot.df) <- c("x", "y", "County", "State", "Obese")
            
            p <- plot_ly(plot.df, x = x, y = y, 
                         text = paste(County, ",", State, "Adult % Obese:",Obese),
                         mode = "markers", color = Obese)
            
            layout(p,title = paste(input$var2, "vs ", input$var1),
                   xaxis = list(title = input$var1),
                   yaxis = list(title = input$var2))
        })
        
        output$tabdb <- DT::renderDataTable(tabledb, options = list(
            scrollX = TRUE))
    
        output$coefs <- DT::renderDataTable(coef, options = list(
            pageLength = 15))
        
        output$rendvifs <- renderDataTable(printvifs)    
        
        output$predtable <- renderUI({
            g <- predfunc(GROC = input$GROC, Conv = input$Conv, Full = input$Full, FF = input$FF, LACCESS = 
                         input$LACCESS, MEDHHIN = input$MEDHHIN, RECFAC = input$RECFAC,
                     PCT18 = input$PCT18, FOODINS = input$FOODINS, FARMRT = input$FARMRT, 
                     VEGFARM = input$VEGFARM, DIABETE = input$DIABETE, HSACT = input$HSACT,
                     POVRT = input$POVRT, PCT65 = input$PCT65)
            str1 = paste("Predicted obesity rate", round(g[1], 1), "%")
            str2 = paste("95% confident that prediction is within", round(g[2], 1), "%", "to", round(g[3], 1), "%")
            HTML(paste(str1, str2, sep = '<br/>'))
            })
    }
)