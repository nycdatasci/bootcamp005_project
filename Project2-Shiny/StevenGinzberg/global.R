
# GLOBALS FOR WEATHER DATA

SGAvg = function(ds, cols) {
    Total = 0
    Cnt = 0
    for (i in 1:length(cols)) {
        if (!is.na(ds[,cols[i]])) {
            Total = Total + ds[,cols[i]]
            Cnt = Cnt + 1
        }
    }
    return(ifelse(Cnt==0,NA,Total/Cnt))
}

SGAvg2 = function(ds, Start, End) {
    Result = c()
    if (Start == End) {
        return(ds[,paste0('Avg',Start)])
    } else {
        for (r in 1:nrow(ds)) {
            Total = 0
            Cnt = 0
            for (i in Start:End) {
                if (!is.na(ds[r,paste0('Avg',i)])) {
                    Total = Total + ds[r,paste0('Avg',i)]
                    Cnt = Cnt + 1
                }
            }
            Result[r] = ifelse(Cnt==0,NA,Total/Cnt)   
        }
        return(Result) 
    }
}

setwd("C:/Users/steve/OneDrive/Documents/BootCamp/R Working Dir/Shiny Project/App v1")

MoHeader = c('Jan','Feb','Mar','Apr','May','Jun',
             'Jul','Aug','Sep','Oct','Nov','Dec')

ChartData = read.csv('Data/Chartdata.csv',header = TRUE, sep = ',')
MapData = read.csv('Data/MapData.csv',header = TRUE, sep = ',')

ChartData$YrAvg = SGAvg(ChartData,c('Avg1','Avg2','Avg3','Avg4','Avg5','Avg6',
                                    'Avg7','Avg8','Avg9','Avg10','Avg11','Avg12'))


    
    
