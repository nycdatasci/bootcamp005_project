########################################
#### Project 2 - Shiny Visualization####
#### Ho Fai Wong - May 8, 2016      ####
########################################

#################
####Data load####
#################

#Split range of ranks
split_rank <- function(rank) {
  if (grepl('-',rank)) {
    x = as.numeric(strsplit(rank, split='-')[[1]][1])
    y = as.numeric(strsplit(rank, split='-')[[1]][2])
    new_rank = round(x + (y-x)/2)
  } else {
    new_rank = rank
  }
  return(new_rank)
}

#Convert CWUR rank to score
ranktoscore <- function(x) {
  return((1-x/max(x)) * 100)
}

#######################
#### Country Stats ####
#######################

#Draw world map for stat by country
world_map <- function(var, stat, color) {
  df = df.2015.country[!is.na(var),]
  gvisGeoChart(data = df, 
               locationvar = "country", 
               colorvar = stat,
               options=list(
                 projection="kavrayskiy-vii",
                 colorAxis=color,
                 width='100%',
                 height='100%',
                 keepAspectRatio = TRUE
               )
  )}

#Draw bar chart for countries by metric
barcountry <- function(var, stat, color) {
  
  df = if (grepl('top',stat) | grepl('median',stat)) {
    df.2015.country[order(var),c('country',stat)]
  } else {
    df.2015.country[order(var, decreasing = TRUE),c('country',stat)]
  }
  df = df[complete.cases(df),]
  
  #Rename column for legend
  colnames(df)[2] = switch(stat,
                           'top_shanghai' = 'Shanghai Rankings - Top rank per country (smaller is better)',
                           'median_shanghai' = 'Shanghai Rankings - Median rank per country (smaller is better)',
                           'count_shanghai' = 'Shanghai Rankings - Count of ranked universities',
                           'alumni' = 'Shanghai Rankings - Mean of alumni score',
                           'award' = 'Shanghai Rankings - Mean of award score',
                           'hici' = 'Shanghai Rankings - Mean of HiCi score', 
                           'ns' = 'Shanghai Rankings - Mean of N&S score',
                           'pub' = 'Shanghai Rankings - Mean of PUB score',
                           'pcp' = 'Shanghai Rankings - Mean of PCP score',
                           
                           'top_times' = 'Times Rankings - Top rank per country (smaller is better)',
                           'median_times' = 'Times Rankings - Median rank per country (smaller is better)',
                           'count_times' = 'Times Rankings - Count of ranked universities',
                           'teaching' = 'Times Rankings - Mean of teaching score',
                           'international' = 'Times Rankings - Mean of international score',
                           'research' = 'Times Rankings - Mean of research score',
                           'citations_times' = 'Times Rankings - Mean of citations score',
                           'income' = 'Times Rankings - Mean of income score',
                           
                           'top_cwur' = 'CWUR Rankings - Top rank per country (smaller is better)',
                           'median_cwur' = 'CWUR Rankings - Median rank per country (smaller is better)',
                           'count_cwur' = 'CWUR Rankings - Count of ranked universities',
                           'quality_of_education' = 'CWUR Rankings - Mean of quality of education score*',
                           'alumni_employment' = 'CWUR Rankings - Mean of alumni employment score*',
                           'quality_of_faculty' = 'CWUR Rankings - Mean of quality of faculty score*',
                           'publications' = 'CWUR Rankings - Mean of publications score*',
                           'influence' = 'CWUR Rankings - Mean of influence score*',
                           'citations_cwur' = 'CWUR Rankings - Mean of citations score*',
                           'broad_impact' = 'CWUR Rankings - Mean of broad impact score*',
                           'patents' = 'CWUR Rankings - Mean of patents score*'
  )
  
  df = as.data.frame(df)
  # df = if (grepl('CWUR',colnames(df)[2]) & !grepl('Count',colnames(df)[2])) {
  #   df[order(df[,2]),]
  # } else {
  #   df
  # }
  
  gvisBarChart(data = df,
               xvar = "country",
               yvar = colnames(df)[2],
               options=list(legend="{position: 'top'}",
                            height=900,
                            align='top',
                            chartArea='{left:200, top:50, width:\'100%\', height:\'100%\'}',
                            fontSize=10)
  )
}



#######################
#### Uni Profile ####
#######################

#Draw bar chart for uni scores
baruni <- function(df, yvar) {
  baruni <- gvisColumnChart(df,
                            yvar=yvar,
                            options=list(height=500,
                                         legend="{position: 'top', maxLines: 10}",
                                         vAxis='{minValue:0, maxValue:100}',
                                         width='100%',
                                         height='100%')
  )
}