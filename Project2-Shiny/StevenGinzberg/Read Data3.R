
##### Initialize #####

setwd("C:/Users/steve/OneDrive/Documents/BootCamp/R Working Dir/Shiny Project")
library(dplyr)

    SGfixTemp = function(x) {
      return(ifelse(x == -9999,NA,((x/100)*9/5)+32))
    }
    
    SGAvg = function(ds, cols) {
        Total = 0
        Cnt = 0
        for (i in 1:length(cols)) {
            if (!is.na(ds[,cols[i]])) {
              Total = Total + ds[,cols[i]]
              Cnt = Cnt + 1
              }
        }
        ifelse(Cnt==0,NA,return(Total/Cnt))
    }

##### Load Data #####
  USACountryCode = 425
  MetaWidths = c(11,9,10,8,30,5,1,5,2,2,2,2,1,2,16,1)
  MetaCols = c('ID','Lat','Long','StElev','Name','GridElev','PopCls','PopSize','Topo','StVeg','StLoc','OcnDis','AirStn','TownDis','GrVeg','PopCss')
  
  DataWidths = c(11,4,4,5,1,1,1,5,1,1,1,5,1,1,1,5,1,1,1,5,1,1,1,5,1,1,1,5,1,1,1,5,1,1,1,5,1,1,1,5,1,1,1,5,1,1,1,5,1,1,1)
  DataCols = c('ID','Yr','Element','Val1','D1','Q1','S1','Val2','D2','Q2','S2','Val3','D3','Q3','S3','Val4','D4','Q4','S4',
               'Val5','D5','Q5','S5','Val6','D6','Q6','S6','Val7','D7','Q7','S7','Val8','D8','Q8','S8',
               'Val9','D9','Q9','S9','Val10','D10','Q10','S10','Val11','D11','Q11','S11','Val12','D12','Q12','S12')

  TMeta = read.fwf('tavg-unadj.inv',widths = MetaWidths,col.names = MetaCols)
  USAStations = TMeta[(substr(TMeta$ID,1,3)=='425') &
                          (TMeta$ID != 42570414000),]
  
  TAvgRaw = read.fwf('tavg-unadj.dat',widths=DataWidths,col.names = DataCols)

##### Filter and Join data #####
  Combined = TAvgRaw %>%
      left_join(USAStations, by = 'ID') %>%
      filter(!is.na(Lat) & !is.na(Long))
 
    rm(TAvgRaw, TMeta, DataWidths, MetaCols)
   # FIX ALL TEMP COLUMNS
    Combined$Val1 = sapply(Combined$Val1,SGfixTemp)
    Combined$Val2 = sapply(Combined$Val2,SGfixTemp)
    Combined$Val3 = sapply(Combined$Val3,SGfixTemp)
    Combined$Val4 = sapply(Combined$Val4,SGfixTemp)
    Combined$Val5 = sapply(Combined$Val5,SGfixTemp)
    Combined$Val6 = sapply(Combined$Val6,SGfixTemp)
    Combined$Val7 = sapply(Combined$Val7,SGfixTemp)
    Combined$Val8 = sapply(Combined$Val8,SGfixTemp)
    Combined$Val9 = sapply(Combined$Val9,SGfixTemp)
    Combined$Val10 = sapply(Combined$Val10,SGfixTemp)
    Combined$Val11 = sapply(Combined$Val11,SGfixTemp)
    Combined$Val12 = sapply(Combined$Val12,SGfixTemp)
    
    Combined$YrTemp = SGAvg(Combined,c('Val1','Val2','Val3','Val4','Val5','Val6',
                                     'Val7','Val8','Val9','Val10','Val11','Val12'))  
   
    Combined$Loc = paste0(Combined$Lat, ":",Combined$Long)
    
    # library(ggplot2)
    # ggplot(data=Combined, aes(x=Name)) + geom_density(stat='count')  
    # ggsave('Chart1.png')
    # ggplot(data=Combined, aes(x=Yr)) + geom_density(stat='count')
    # ggsave('Chart2.png')
        
    # Combined$Val1[is.na(Combined$Val1)] = mean(Combined$Val1, na.rm=TRUE)
    # Combined$Val2[is.na(Combined$Val2)] = mean(Combined$Val2, na.rm=TRUE)
    # Combined$Val3[is.na(Combined$Val3)] = mean(Combined$Val3, na.rm=TRUE)
    # Combined$Val4[is.na(Combined$Val4)] = mean(Combined$Val4, na.rm=TRUE)
    # Combined$Val5[is.na(Combined$Val5)] = mean(Combined$Val5, na.rm=TRUE)
    # Combined$Val6[is.na(Combined$Val6)] = mean(Combined$Val6, na.rm=TRUE)
    # Combined$Val7[is.na(Combined$Val7)] = mean(Combined$Val7, na.rm=TRUE)
    # Combined$Val8[is.na(Combined$Val8)] = mean(Combined$Val8, na.rm=TRUE)
    # Combined$Val9[is.na(Combined$Val9)] = mean(Combined$Val9, na.rm=TRUE)
    # Combined$Val10[is.na(Combined$Val10)] = mean(Combined$Val10, na.rm=TRUE)
    # Combined$Val11[is.na(Combined$Val11)] = mean(Combined$Val11, na.rm=TRUE)
    # Combined$Val12[is.na(Combined$Val12)] = mean(Combined$Val12, na.rm=TRUE)
    
    Combined[Combined$Val4>110 & !is.na(Combined$Val4),'Val4'] = NA
    Combined[Combined$Val7>110 & !is.na(Combined$Val7),'Val7'] = NA

    ChartData = filter(Combined,Yr >=1840) %>%
        group_by(Yr) %>%
        summarise(Avg1 = mean(Val1,na.rm = TRUE),Avg2 = mean(Val2,na.rm = TRUE), Avg3 = mean(Val3,na.rm = TRUE),
                  Avg4 = mean(Val4,na.rm = TRUE),Avg5 = mean(Val5,na.rm = TRUE), Avg6 = mean(Val6,na.rm = TRUE),
                  Avg7 = mean(Val7,na.rm = TRUE), Avg8 = mean(Val8,na.rm = TRUE), Avg9 = mean(Val9,na.rm = TRUE),
                  Avg10=mean(Val10,na.rm = TRUE), Avg11 = mean(Val11,na.rm = TRUE), Avg12=mean(Val12,na.rm = TRUE)) 
        
        ChartData[ChartData$Yr==2016,'Avg4'] = NA
        ChartData[ChartData$Yr==2016,'Avg5'] = NA
        ChartData[ChartData$Yr==2016,'Avg6'] = NA
        ChartData[ChartData$Yr==2016,'Avg7'] = NA
        ChartData[ChartData$Yr==2016,'Avg8'] = NA
        ChartData[ChartData$Yr==2016,'Avg9'] = NA
        ChartData[ChartData$Yr==2016,'Avg10'] = NA
        ChartData[ChartData$Yr==2016,'Avg11'] = NA
        ChartData[ChartData$Yr==2016,'Avg12'] = NA
    
        
    
    MapData = filter(Combined, Element == 'TAVG') %>%
        group_by(.,ID,Loc,Yr) %>%
        summarise(., Avg1 = mean(Val1,na.rm = TRUE),Avg2 = mean(Val2,na.rm = TRUE), Avg3 = mean(Val3,na.rm = TRUE),
                  Avg4 = mean(Val4,na.rm = TRUE),Avg5 = mean(Val5,na.rm = TRUE), Avg6 = mean(Val6,na.rm = TRUE),
                  Avg7 = mean(Val7,na.rm = TRUE), Avg8 = mean(Val8,na.rm = TRUE), Avg9 = mean(Val9,na.rm = TRUE),
                  Avg10=mean(Val10,na.rm = TRUE), Avg11 = mean(Val11,na.rm = TRUE), Avg12=mean(Val12,na.rm = TRUE)) %>%
        left_join(USAStations,by = 'ID')
    MapData$Hover = paste0(MapData$Name,', Alt=',MapData$StElev)

    OverallAvg = c()
    OverallAvg[1] = mean(Combined$Val1, na.rm = TRUE)
    OverallAvg[2] = mean(Combined$Val2, na.rm = TRUE)
    OverallAvg[3] = mean(Combined$Val3, na.rm = TRUE)
    OverallAvg[4] = mean(Combined$Val4, na.rm = TRUE)
    OverallAvg[5] = mean(Combined$Val5, na.rm = TRUE)
    OverallAvg[6] = mean(Combined$Val6, na.rm = TRUE)
    OverallAvg[7] = mean(Combined$Val7, na.rm = TRUE)
    OverallAvg[8] = mean(Combined$Val8, na.rm = TRUE)
    OverallAvg[9] = mean(Combined$Val9, na.rm = TRUE)
    OverallAvg[10] = mean(Combined$Val10, na.rm = TRUE)
    OverallAvg[11] = mean(Combined$Val11, na.rm = TRUE)
    OverallAvg[12] = mean(Combined$Val12, na.rm = TRUE)
    
    plot(x=1:12, y=OverallAvg, col='blue', type='l')
    
    
    
    
    write.csv(ChartData,'chartdata.csv')
    write.csv(MapData,'mapdata.csv')

    
