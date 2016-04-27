library("ggplot2", lib.loc="~/R/win-library/3.2")
library(RColorBrewer)
library(scales)

# Histogram of tickets issued for all 5 boroughs
    ggplot(PVI2,aes(x=BoroughNm,fill=BoroughNm)) + 
      geom_bar() + 
      ggtitle('Parking Violations Issued by Borough') +
      xlab('Borough') + ylab('Tickets issued') +
      scale_x_discrete(limits=c("M","BK","BX","Q","SI"))  +
      scale_y_continuous(labels=comma) +
      scale_fill_brewer(palette='Set2') +
      theme(legend.position = 'none')
        # ggsave('Chart1.png',width=10,height=6)
    
    ManhattanOnly = mutate(filter(PVI2,BoroughNm=='M', Issuer.Code != 0),FPrecinct = as.factor(Violation.Precinct),
                           VCode = as.factor(Violation.Code))
    PVI3 = mutate(PVI2,FPrecinct = as.factor(Violation.Precinct),VCode = as.factor(Violation.Code))

#####################################    
# Change this to PVI3 for all       #
# boroughs, or ManhattanOnly        #

    SelectDB = ManhattanOnly
    SelectDBName = 'Manhattan'
    
    
# Top 10 Violation Types issued for Manhattan
    Top10Violations = group_by(SelectDB,VCode) %>%
                        summarise(Cnt = n()) %>%
                        arrange(desc(Cnt)) %>%
                        slice(1:10) 
    
    PlotLegendTextLookup = group_by(SelectDB,ShortDefn) %>%
                           summarise(Cnt=n()) %>%
                           arrange(desc(Cnt)) %>%
                           slice(1:10)

    PlotLegendText2 = PlotLegendTextLookup$ShortDefn
    Top10ViolationRatio = round(sum(Top10Violations$Cnt)/nrow(SelectDB) * 100)
    Top10ViolationText = paste0(Top10ViolationRatio, '% of all violations')
    
   
    ggplot(Top10Violations,aes(x=VCode,y=Cnt,fill=Cnt)) + 
       geom_bar(stat='sum') + 
       geom_text(label=PlotLegendText2,hjust=1,size=5,color='white') +  
       coord_flip() +
       ggtitle(paste0('Top 10 Violations in ',SelectDBName)) +
       xlab('Violation Code') + 
       ylab('Tickets Issued') +
       theme(legend.position = 'none') +
       annotate("text", x=10, y=300000, label= Top10ViolationText,size = 6) +
       scale_y_continuous(labels=comma,breaks=seq(0, 700000, 100000))
      
        #ggsave('Chart2.png',width=10,height=6)
    
# Count of all violations
# NOT USED
   ggplot(SelectDB,aes(x=VCode,fill=VCode)) + 
       geom_bar() + 
       coord_flip() + 
       scale_x_discrete() +
       theme(legend.position = 'none')

# Top 10 Dates of Ticket Issuances
     Top10Dates = group_by(SelectDB,Issue.Date) %>%
         summarise(Cnt = n()) %>%
         arrange(desc(Cnt)) %>%
         slice(1:10) 

     Top10DatesViolationRatio = round(sum(Top10Dates$Cnt)/nrow(SelectDB) * 100)
     Top10DatesViolationText = paste0(Top10DatesViolationRatio, '% of all violations')

     TicketsperDayGrp = group_by(SelectDB,Issue.Date) %>%
                        summarise(Cnt=n())
     AvgTicketsperDay = rep(mean(TicketsperDayGrp$Cnt),10)

     ggplot(Top10Dates,aes(x=Issue.Date, y = Cnt,fill=Cnt)) + 
        geom_bar(stat='sum') +
        ggtitle(paste0('Top 10 Dates for Ticket Issuances, ',SelectDBName)) + 
        xlab('Date') + 
        ylab('Tickets Issued') +
        geom_point(aes(x=Issue.Date,y=AvgTicketsperDay)) +
        theme(legend.position = 'none') +
        annotate("text", x=9, y=30000, label= Top10DatesViolationText,size=5) +
        scale_y_continuous(labels=comma,breaks=seq(0, 50000, 10000))
     
           #ggsave('Chart4.png',width=10,height=6)
     
# Violations for all Dates, All Manhattan               - NOT USED, for support only

     ggplot(SelectDB,aes(x=Issue.Date,fill=Issue.Date)) + 
         geom_bar() + 
         coord_flip() +
         theme(legend.position = 'none')
            #ggsave('Chart6.png',width=10,height=6)
     
# Top 20 Street Locations of Issuances
     Top20Streets = group_by(SelectDB,FeatName) %>%
       summarise(Cnt = n()) %>%
       filter((!is.na(FeatName) & !(FeatName == ''))) %>%
       arrange(desc(Cnt)) %>%
       slice(1:20) 
     
     StreetNames = Top20Streets$FeatName
     Top20StreetsViolationRatio = round(sum(Top20Streets$Cnt)/nrow(SelectDB) * 100)
     Top20StreetsViolationText = paste0(Top20StreetsViolationRatio, '% of all violations')
     
     ggplot(Top20Streets,aes(x=FeatName, y = Cnt,fill=Cnt),na.rm='TRUE') + 
        geom_bar(stat='sum') + 
        coord_flip() +
        geom_text(label=StreetNames, size=3,color='white') + 
        ggtitle(paste0('Top 20 Streets for Ticket Issuances, ',SelectDBName)) + 
        xlab('Street') + 
        ylab('Tickets Issued') +
        theme(legend.position = 'none',
              axis.text.y=element_blank(),
              axis.ticks = element_blank()) +              
        annotate("text", x=19, y=100000, label= Top20StreetsViolationText,size=5) +
        scale_y_continuous(label=comma,breaks=seq(0, 120000, 30000))
           #ggsave('Chart4.png',width=10,height = 6)
     
     
# HEAT MAP: by Precinct and Violation Code and graphed by lat-lon
     ViolationsbyPrecinct = group_by(ManhattanOnly,VCode,FPrecinct,lat,lon) %>%
                                summarise(Cnt = n()) 
     ggplot(ViolationsbyPrecinct,aes(x=lon,y=lat,color=VCode,size=Cnt)) + 
        geom_jitter() +
        coord_map() +
        ggtitle('Heat Map for Manhattan Island by Precinct') +
        xlab('') + ylab('') +
        theme(legend.position  = 'none',
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank())
           #ggsave('Chart5.png',width=10,height=6)
