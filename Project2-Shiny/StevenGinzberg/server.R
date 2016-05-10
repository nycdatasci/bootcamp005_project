library(shiny)
library(googleVis)
library(dplyr)

source('.\\global.r')

shinyServer(function(input, output) {
     output$wthrChart <- renderGvis({
         NewData = data.frame(Yr=ChartData$Yr,Avg=SGAvg2(ChartData,input$moSlider[1],input$moSlider[2]))
         NewData2 = NewData[NewData$Yr>=input$yrSlider,]
         if (input$moSlider[2]>=4) {NewData2 = filter(NewData2,Yr<=2015)}
         
         SLR = lm(Avg ~ Yr, data = NewData2)
         NewData2$Fitted = SLR$fitted.values
         
         moTitle = paste0(MoHeader[input$moSlider[1]]," - ",MoHeader[input$moSlider[2]])
         
         gvisLineChart(data = NewData2,
                       xvar = 'Yr',
                       yvar = c('Avg','Fitted'),
                       options= list(
                           title=paste0('Average Nationwide Temps, ',moTitle),
                           titleTextStyle='{color:"Red",fontSize:18}',
                           hAxis='{title:"Year", format:"####"}',
                           series='[{color:"blue",lineWidth:2},{color:"red",lineDashStyle:[4,1]}]',
                           vAxis = '{title: "Avg Temp (F)"}',
                           width = 900,
                           height = 500))
     })
     
     
     output$wthrMap <- renderGvis({
         NewMapData = MapData[MapData$Yr == input$yrSlider,] 
         NewMapData$Avg = SGAvg2(NewMapData,input$moSlider[1],input$moSlider[2])
         NewMapData$Dmy = 1
         mapTitle = paste0(MoHeader[input$moSlider[1]]," - ",MoHeader[input$moSlider[2]],", ", input$yrSlider)
         gvisGeoChart(NewMapData,
                    locationvar = 'Loc',
                    colorvar = 'Avg',
                    sizevar = 'Dmy',
                    hovervar = 'Hover',
                    options = list(
                        title=paste0('Average Temperature (F) ',mapTitle),
                        dataMode='markers',
                        region='US',
                        colorAxis='{colors: ["blue", "orange"]}',
                        keepAspectRatio = TRUE,
                        markerOpacity=.5,
                        width=800,
                        height=500
                    ))
     })
})
