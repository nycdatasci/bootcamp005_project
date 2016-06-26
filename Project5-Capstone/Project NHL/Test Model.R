library(dplyr)
          

# Load Data
    setwd("C:/Users/steve/OneDrive/Documents/BootCamp/NHL/NHLModel2")
    source('sgFunctions.R')  
     
    roster   <- read.csv('Players.csv', header = TRUE)
    events   <- read.csv('NHLData2016.csv', header = TRUE)
    events   <- rbind(events,read.csv('NHLData2015.csv'))
    events   <- rbind(events,read.csv('NHLData2014.csv'))
    events   <- rbind(events,read.csv('NHLData2013.csv'))
    
    newSchedule <- read.csv('NewGameInfo.csv', header = TRUE)
    newSchedule <- unique(newSchedule)
    newSchedule['nDate'] = as.Date(newSchedule$date, '%A %B %d %Y')
    newSchedule['homeABB'] = sgto_ABB(newSchedule$home)
    newSchedule['visABB'] = sgto_ABB(newSchedule$visitor)
    newSchedule['Season'] = sgtoSeason(newSchedule$nDate)
    newSchedule['gameType'] = sgtoGameType(newSchedule$gameId)
    
    jEvents  <- left_join(events,newSchedule, by='gameId') 

    jEvents['scoreTeam'] = ifelse(jEvents$evt=='GOAL',substring(jEvents$desc,1,3),'')
    jEvents['scoreTeam'] = ifelse(jEvents$scoreTeam=='N.J','NJD',
                            ifelse(jEvents$scoreTeam=='L.A','LAK',
                               ifelse(jEvents$scoreTeam=='T.B','TBL',
                                  ifelse(jEvents$scoreTeam=='S.J','SJS',jEvents$scoreTeam))))
    jEvents['scoreNo'] = ifelse(jEvents$evt=='GOAL',as.numeric(substring(jEvents$desc,6,7)),'')
    jEvents['Goal'] = ifelse(jEvents$scoreTeam==jEvents$homeABB,'hg',
                             ifelse(jEvents$scoreTeam==jEvents$visABB,'vg',0))
    jEvents['GoalVal'] = ifelse(jEvents$scoreTeam==jEvents$homeABB,1,
                             ifelse(jEvents$scoreTeam==jEvents$visABB,-1,0))
    
    # need to append scorer name, remove number!
    # fix parse event!
    
# Data Analysis
    library(ggplot2)
    library(RColorBrewer)
    availableGames = distinct(jEvents,gameId)$gameId
    eventTypes = levels(events$evt)
    summary(jEvents)
    
    # Histograms of database
        teamEvts = c('BLOCK','FAC','GIVE','GOAL','HIT','MISS','PENL','SHOT','STOP','TAKE')
        teamDataonly = jEvents[jEvents$evt %in% teamEvts,]
        ggplot(teamDataonly,aes(x=homeABB)) + geom_bar(aes(fill=evt)) +
            ggtitle('Events Records') + xlab('Team') + ylab('# of Events') +
            scale_fill_brewer(palette='Spectral')
        #ggsave('image1.png', width = 8, height = 5)
            
        ggplot(teamDataonly,aes(x=nDate)) + geom_histogram(aes(fill=evt)) +            # by date and event
            ggtitle('Games by Event Type') + xlab('Date') + ylab('# of Events') +
            scale_fill_brewer(palette = 'Spectral')
        ggsave('image2.png', width = 8, height = 5)
        
    # by event type    
        feature1 = 'GOAL'                                                       # event over time by period
        plt1 = ggplot(jEvents[jEvents$evt == feature1,], aes(x = nDate))
        plt1 + geom_bar(aes(fill=as.factor(period))) + 
            scale_fill_brewer(palette = 'Blues')
      
    # comparing 2 teams
        team1 = 'ANA'
        team2 = 'LAK'
        pltData = jEvents[(jEvents$homeABB==team1 & jEvents$visABB==team2) |
                              (jEvents$homeABB==team2 & jEvents$visABB==team1),]
        
        pltData = pltData[rowSums(is.na(pltData))==0,]
        ggplot(pltData, aes(x = evt)) + 
             geom_bar(aes(fill = homeABB), position = 'dodge') + 
             coord_polar() + ylab('') + xlab('') +
             scale_fill_brewer(palette = 'Set1')
        ggsave('image4.png', width = 5, height = 5)
        
    # GOAL CHARTS with GROUP BY
        Teams = c('NYR')
        hg = ifelse(jEvents$GoalVal==1,1,0)
        vg = ifelse(jEvents$GoalVal==-1,1,0)
        jEvents2 = jEvents
        jEvents2['hg'] = hg
        jEvents2['vg'] = vg
        goalData = jEvents2[jEvents2$evt =='GOAL' &
                            ((jEvents2$homeABB==Teams | jEvents2$visABB==Teams)) ,] %>%
                    group_by(nDate,homeABB, visABB) %>%
                    summarise(homeScore = sum(hg),visScore = sum(vg))
        
        ggplot(goalData, aes(x=nDate)) + 
            geom_jitter(aes(y = homeScore, color = homeABB)) + 
            geom_jitter(aes(y = visScore, color = visABB)) +
            ggtitle(paste(Teams,'Goals versus Opponents')) +
            xlab('Date') + ylab('Goals')
        
    # Moving Averages of goal data  
       source('movingAverage.R')
        
        ma = movingAverage(goalData$homeScore,n=10,centered=FALSE)
        ggplot(goalData, aes(x=nDate)) + 
            geom_jitter(aes(y = homeScore, color = homeABB)) + 
            geom_jitter(aes(y = visScore, color = visABB)) +
            geom_line(aes(y=ma)) +
            ggtitle(paste(Teams,'Goals versus Opponents')) +
            xlab('Date') + ylab('Goals')
        #ggsave('image5.png', width = 8, height = 5)
     library(scatterplot3d)
        
        evt.group = jEvents %>% 
                group_by(homeABB, visABB, evt) %>%
                summarise(N = n())
        scatterplot3d(x= as.factor(evt.group$homeABB), y= as.factor(evt.group$visABB), 
                      z=evt.group$evt, color = evt.group$N)
       

# Set Up Dataset for Modeling
    # drop redundant columns
        dropCols = c('homeNo1','homeNo2','homeNo3', 'homeNo4', 'homeNo5', 'homeNo6',
                     'awayNo1','awayNo2','awayNo3','awayNo4','awayNo5','awayNo6',
                     'home','visitor','date','scoreTeam', 'scoreNo', 'Goal', 'GoalVal')
        dataset = jEvents[ , !(names(jEvents) %in% dropCols)]
        
    # Set up dataset for Prediction
        # Set up Goal indicator Variables
        newGoalCol = jEvents$Goal
        newGoalCol = newGoalCol[2:length(newGoalCol)]
        newGoalCol = append(newGoalCol, '0')
        newGoalVal = jEvents$GoalVal
        newGoalVal = newGoalVal[2:length(newGoalVal)]
        newGoalVal = append(newGoalVal, 0)
        dataset['newGoals'] = newGoalCol
        dataset['newGoalVal'] = as.numeric(newGoalVal)
        
        # Remove Goal Records
        remRows = with(dataset, which(dataset$evt=='GOAL', arr.ind=TRUE))
        revdataset = dataset[-remRows, ]
        
        # Remove NA Rows (for now)
        revdataset = revdataset[rowSums(is.na(revdataset))==0,]
        
        revdataset['nDate'] = as.character(revdataset$nDate)
        revdataset['homeABB'] = as.factor(revdataset$homeABB)
        revdataset['visABB'] = as.factor(revdataset$visABB)
        
    # set up training and test sets
        train.recs = c(1:500000)
        test.recs = c(500000:750000)

# Naive Bayes Model
    library(tm)
    library(e1071)    
    
    x.train = revdataset[train.recs,-c(40,41)]
    y.train = as.factor(revdataset[train.recs,'newGoalVal'])
    x.test = revdataset[test.recs,-c(40,41)]
    y.test = as.factor(revdataset[test.recs,'newGoalVal'])
    
    NB.model = naiveBayes(x.train,y.train)
    NB.pred = predict(NB.model,y.test)
    table(NB.pred,y.test)
    
    write.csv(revdataset,'revdataset.csv')
# Logistic Model        
    rm(dataset,dropCols,events,newGoalCol,newSchedule,roster)

    glm.fields = c('period', 'playNo','evt','desc','time','homeNa1','homeNa2',
                   'homeNa3','homeNa4','homeNa5','homeNa6','awayNa1','awayNa2',
                   'awayNa3','awayNa4','awayNa5','awayNa6','att','loc','homeABB','visABB',
                   'newGoalVal')
                   
    glm.data = revdataset[train.recs, glm.fields]
    glm.model = glm(newGoalVal ~ ., data = glm.data)
    
    scatter.smooth(glm.model$fit,
                   residuals(glm.model, type = "deviance"),
                   lpars = list(col = "red"),
                   xlab = "Fitted Probabilities",
                   ylab = "Deviance Residual Values",
                   main = "Residual Plot for\nLogistic Regression of Admission Data")
    abline(h = 0, lty = 2)
