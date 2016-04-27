
    setwd("C:/Users/steve/OneDrive/Documents/BootCamp/R Working Dir/Visualization Project")
# Load the packages
    library("dplyr", lib.loc="~/R/win-library/3.2")
    
# READ THE RAW DATA FILES........
    PVI <- read.csv("Parking_Violations_Issued_-_Fiscal_Year_2016.csv", stringsAsFactors = FALSE)
    ViolCodes <- read.csv('DOF_Parking_Violation_codes.csv', stringsAsFactors = FALSE)
    StreetDB <- read.fwf('snd15Dcow.txt', widths = c(1,1,32,1,1,1,5,2,3,2,1,1,2,32,2,20,1,92))

    names(StreetDB) = c('F1','BoCode','FeatName','Primary','Princ','BoCode2','Street.Code1','GrpCode','SpellVar','F2','NNInd',
                        'FeatType','LenProg','Prog','MinNameLen','StName2','HTTCode','F3')
    FeatType <- read.csv('GeoFeatureTypes.csv', stringsAsFactors = FALSE) 
    PctBoroughs = read.csv('PctBorough.csv', stringsAsFactors = FALSE)
    ShortPVCodes = read.csv('Short PV Codes.csv')
# Rename some fields to make the joins easy
    names(ViolCodes)[3] = 'Manhattan'
    names(PctBoroughs)[1] = 'Violation.Precinct'
    names(FeatType)[2] = 'FeatType'

# Need to convert ViolCodes.Violation.Code to INTEGER
    ViolCodesa1 = mutate(ViolCodes,Violation.Code = as.numeric(ViolCodes$CODE))
    ViolCodes2 = left_join(ViolCodesa1,ShortPVCodes,by = c('Violation.Code'='CODE_Num'))

# JOINS!
    StreetDB2 = filter(StreetDB,Primary=='P') %>%
            left_join(.,FeatType,by = 'FeatType')

    PVIRemoveColumns = c(31:33,37:42)
    PVIa1 = mutate(PVI,DDate = as.Date(Issue.Date, format='%m/%d/%Y'))
    PVIa2 = select(PVIa1,-PVIRemoveColumns)
    PVIa3 = left_join(PVIa2,PctBoroughs, by = 'Violation.Precinct')
    PVIa4 = left_join(PVIa3,ViolCodes2,by = 'Violation.Code') 
    
    PVI2 = left_join(PVIa4, StreetDB2,by = c('Street.Code1','BoCode'))
    
# Finally, clean up original tables
    rm(PVI,PVIa1,PVIa2,PVIa3,PVIa4,StreetDB,ViolCodes,ViolCodesa1,FeatType)

