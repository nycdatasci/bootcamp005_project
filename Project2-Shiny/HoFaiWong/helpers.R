########################################
#### Project 2 - Shiny Visualization####
#### Ho Fai Wong - May 8, 2016      ####
########################################


## helpers.R ##

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

# #Convert rank to score
# ranktoscore <- function(x) { 
#   return((1-x/1000) * 100) #n=1000 obs in CWUR 2015
# }