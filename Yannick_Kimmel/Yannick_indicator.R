library(dplyr)
library(ggplot2)

indicators = read.csv("subbeddata.csv", header = TRUE, stringsAsFactors = FALSE)

counts <- indicators %>%
    group_by(IndicatorCode, IndicatorName) %>%
    summarise(NumCountries = n_distinct(CountryName),
              NumYears     = n_distinct(Year),
              FirstYear    = min(Year),
              LastYear     = max(Year))

#Filter indicator results to R&D expenditures % as GDP from 1996 to 2014
#European Union = EUU, world = WLD,OECD members = OED, US = USA, China = CHN,
#Japan = JPN, Russia = RUS, India = IND, Korea = KOR
countries = c("EUU", "USA", "CHN", "JPN", 
              "RUS", "IND", "KOR") #WLD, OED

indicatorcodes = c("GB.XPD.RSDV.GD.ZS", "SP.POP.SCIE.RD.P6", 
                   "IP.JRN.ARTC.SC", "IP.PAT.NRES", "IP.PAT.RESD", "IP.TMK.TOTL",
                   "NV.IND.MANF.ZS")

#Function to subset data depending on the indicator of interest
subdata = function(indicatorCode) {
    filter(indicators, IndicatorCode == indicatorCode) %>%
        filter(CountryCode %in% countries)
}

researchexpn =  subdata("GB.XPD.RSDV.GD.ZS")
res <- ggplot(researchexpn, aes(x = Year, y = Value)) +geom_line(aes(color = CountryCode), size = 1)
res + theme_bw() + ggtitle("R&D expenditures as a percentage of GDP") +
    scale_color_discrete(labels = c("China","Euro Union", "India",
                                    "Japan", "Korea", "Russia", "USA")) + 
    guides(color=guide_legend(title="Countries")) 

researchers = subdata("SP.POP.SCIE.RD.P6")
respeople <- ggplot(researchers, aes(x = Year, y = Value)) +geom_line(aes(color = CountryCode), size = 1)
respeople + theme_bw() + ggtitle("Number of Researchers in R&D per million people") +
    scale_color_discrete(labels = c("China","Euro Union", "India",
                                    "Japan", "Korea", "Russia", "USA")) + 
    guides(color=guide_legend(title="Countries")) 

publications = subdata("IP.JRN.ARTC.SC")
pub = ggplot(publications, aes(x = Year, y = Value)) +geom_line(aes(color = CountryCode), size = 1)
pub + theme_bw() +  ggtitle("Number of scientific and technical journal articles") +
    scale_color_discrete(labels = c("China","Euro Union", "India",
                                    "Japan", "Korea", "Russia", "USA")) + 
    guides(color=guide_legend(title="Countries")) 

#data seperates patents by residents and non residents of a country, 
#grouping data to total patents by both residents and non residents 
patents = filter(indicators, IndicatorCode == "IP.PAT.NRES" | 
                     IndicatorCode == "IP.PAT.RESD") %>%
    filter(CountryCode %in% countries) %>% group_by(CountryCode, Year) %>%
    summarise(totpap = sum(Value))
pat = ggplot(patents, aes(x = Year, y = totpap)) + geom_line(aes(color = CountryCode), size = 1)
pat + theme_bw() + ggtitle("Number of patents") + ylab("total patents") +xlim(1980, 2014) +
    scale_color_discrete(labels = c("China","Euro Union", "India",
                                    "Japan", "Korea", "Russia", "USA")) + 
    guides(color=guide_legend(title="Countries")) 

#Total number of trademark applications
trademark = subdata("IP.TMK.TOTL")
trade = ggplot(trademark, aes(x = Year, y = Value)) + geom_line(aes(color = CountryCode), size = 1)
trade + theme_bw() + ggtitle("Number of trademark applications") +
    scale_color_discrete(labels = c("China","Euro Union", "India",
                                    "Japan", "Korea", "Russia", "USA")) + 
    guides(color=guide_legend(title="Countries")) 

#manufacturing, value added (% of GDP)
manufacturing = subdata("NV.IND.MANF.ZS")
manufact = ggplot(manufacturing, aes(x = Year, y = Value)) + geom_line(aes(color = CountryCode), size = 1)
manufact + theme_bw() + ggtitle("Manufacturing, value added (% of GDP)") +
    scale_color_discrete(labels = c("China","Euro Union", "India",
                                    "Japan", "Korea", "Russia", "USA")) + 
    guides(color=guide_legend(title="Countries")) 