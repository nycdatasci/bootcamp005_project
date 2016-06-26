
sgSplit <- function(S2) {
    R = c()
    t = ''
    S3 = strsplit(S2,'')[[1]]
    for (c in S3) {
        if (c == ' ') {
            R = append(R,t)
            t = ''
        } else if (c == ',') {
            R = append(R,t)
            R = append(R,',')
            t = ''
        } else {
            t = paste0(t,c)
        }
    }
    if (length(t)>0) {
        R = append(R,t)
    }
    return(R)
} 


sgGoalTeam = function(g) {
    if (is.na(g)) {
        return('NA')
    } else {
        return(substring(g,1,3))
    }
}

sgGoalPlyr = function(g) {
    SL = sgSplit(g)
    C = match(',',SL)
    return(paste(SL[3:C]))
}


sgtoGameType <- function(C) {
    L = strsplit(toString(C), '')[[1]]
    return(as.numeric(L[6]))
}

sgtoSeason <- function(D) {
    tmp <-  as.Date(D)
    Y = as.numeric(format(tmp,'%Y'))
    M = as.numeric(format(tmp,'%m'))
    if (M <= 6) {
        Y = Y - 1
        M = M + 4
    } else {
        M = M - 8
    }
    return(Y + M/12)
}

NHLABB = c('ANA','ARI','BOS','BUF','CAR','CBJ','CGY','CHI','COL',
           'DAL','DET','EDM','FLA','LAK','MIN','MTL','NJD',
           'NSH','NYI','NYR','OTT','PHI','ARI','PIT','SJS',
           'STL','STL','TBL','TOR','VAN','WPG','WSH')

FullNames = c('anaheim ducks', 'arizona coyotes', 'boston bruins', 'buffalo sabres',
             'carolina hurricanes',  'columbus blue jackets', 'calgary flames' ,'chicago blackhawks', 'colorado avalanche', 
             'dallas stars', 'detroit red wings', 'edmonton oilers', 'florida panthers', 'los angeles kings', 
             'minnesota wild', 'montreal canadiens', 'new jersey devils', 'nashville predators', 
             'new york islanders', 'new york rangers', 'ottawa senators', 'philadelphia flyers','phoenix coyotes', 
             'pittsburgh penguins', 'san jose sharks','st louis blues', 'st. louis blues', 'tampa bay lightning', 
             'toronto maple leafs', 'vancouver canucks', 'winnipeg jets' , 'washington capitals')

sgto_ABB <- function(S) {
    t = tolower(S)
    i = match(t,FullNames)
    return(NHLABB[i])
}

# Columns Min, Mean, Median, Max, Type, NAs
# Factor Columns factors, counts, NAs
sgSummary <- function(d) {
    r = data.frame(names = names(d))
    for (x in d.names) {
        Ty = class(d[,x])
        r[x,'Type'] = Ty
        if (Ty %in% c('int', 'real','double','date')) {
            r[x,'Min'] = min(d[,x],na.rm=TRUE)
            r[x,'Max'] = max(d[,x],na.rm=TRUE)
            r[x,'Mean'] = mean(d[,x],na.rm=TRUE)
            r[x,'Median'] = median(d[,x],na.rm=TRUE)
            r[x,'NAs'] = sum(is.na(d[,x]))
        } else {
            
        }
        
    }
    return(r)
}