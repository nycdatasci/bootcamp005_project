#Zachary Escalante
#Shiny Project
install.packages("ggplot2")
install.packages("reshape2")
install.packages("dplyr")
install.packages("ggthemes")
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)

#Import and clean data
navient <- read.csv("/Users/zacharyescalante/Documents/Navient/Dollars-Table\ 1.csv", skip = 3)
navient <- navient[,-1]


t <- which(navient[,1] == "")
navient = navient[-t,]

data <- matrix(nrow = (nrow(navient)), ncol = ncol(navient))

class(navient[,1])                         #Copy the first two factor columns from navient
data[,1] = as.character(navient[,1])       #into our dataframe in the character format
class(data[,1])
data[,2] = as.character(navient[,2])
class(data[,2])

x <- colnames(navient)                     #change the column names in our new data frame to those
x[1] = "Tranche"                           #of the navient data frame
x[2] = "Variable"
colnames(data) <- c(x)


others <- list()                          #Create a list that we will use to store our values that we 
others                                    #cannot reformat easily
k = 1


for(j in 3:ncol(navient))                        #These two 'for' loops will copy all the 
{                                                #data from 'navient' into 'data' matrix
  for(i in 1:nrow(navient))                      #in the proper format (numeric and characters)
  {   
    cat(i, " ", j, "\n")
    if(as.character(navient[i,j] %in% c("-", "") | is.na(navient[i,j] == TRUE)))  #if our cell value is any character denothing null, replace 
    {                                                                             #with a negative 5
      data[i, j] = -5
    }
    else if(grepl(" ", navient[i,j]) == 1)
    {
      as.character(navient[i,j])
      a <- gsub(" ", "", navient[i,j])
      b <- gsub(",", "", a)
      data[i, j] = as.numeric(b)
    }
    else
    {
      data[i,j] = -9999
      others[[k]] = c(i, j)
      k = k+1
    }
  }
}

others
for(i in 1:length(others))
{
  print(navient[others[[i]][1], others[[i]][2]])    #See the values stored in my 'others' list
}                                                   #These values will have to be handled manually

others[[84]]        #I knew that I only had 84 problem values, so I checked these manually
x = others[[84]][1]
y = others[[84]][2]
data[x,y]
navient[x,y]
data[x,y] <- -1.20
data[x,y]



data.long <- melt(as.data.frame(data), id=c("Tranche", "Variable"))   #Use the 'melt' function to reformat
data.long[,3] <- paste("1.", data.long[,3])                           #the data frame into long format
data.long[,3] <- as.Date(data.long[,3], "%d. %b.%y")
data.long[,4] <- as.numeric(data.long[,4])
class(data.long[,3])
class(data.long[,4])

for(i in 1:nrow(data.long))
{
  cat(i, "\n")
  if(data.long[i,4] == -5)
  {
    data.long[i, 4] = NA
  }
}
head(data.long)
class(data.long[,4])

p <- format(data.long[,4], big.mark=",", nsmall = 2)    #Format the numeric column 
data.long[,4] <- p

#Our data is now clean and in the proper format. 
#From here down I test the different graphs I'll put into Shiny
#--------------------------------------------------------------
v = c("2004-4", "2004-06-01")
#We can begin filtering and graphing.
df2 <- group_by(data.long, Tranche, variable)
df3 <- filter(df2, Tranche == v[1], variable == v[2])

#Repayment Data Graph
#ask the users to select the variables they want to chart using a check box
#pass these variables in as a vector to refilter the graph to the necessary variables
v1 <- c("Grand Total ($)", "Total Repayment ($)", "Forbearance ($)", "Deferment ($)", "School ($)", "Since Issuance CPR (%)")
df4 <- filter(df3, Variable %in% v1)   #define a data frame based on the passed variables

#Create our Payment Status Bar graph
g <- ggplot(data = df4, aes(x = Variable, y = value))
g <- g + geom_bar(aes(fill = Variable), stat = "identity", position = "dodge") +
  ggtitle("Payment Status") + 
  scale_fill_discrete(name = "Repayment") +
  xlab("") + 
  ylab("") #+ theme(axis.text.x= element_blank()) 
g
#Create our delinquency Rose Graph
##ask the users to select the variables they want to chart using a check box
#pass these variables in as a vector to refilter the graph to the necessary variables
delinq.var <- c("31-60 Days Delinquent ($)", "61-90 Days Delinquent ($)", "91-120 Days Delinquent ($)", "121-150 Days Delinquent ($)",
                "151-180 Days Delinquent ($)", "181-210 Days Delinquent ($)", "211-240 Days Delinquent ($)", "241-270 Days Delinquent ($)",
                "271-300 Days Delinquent ($)", "301-330 Days Delinquent ($)", "Current ($)")
delinq.var1 <- c("Current ($)", "Total Delinquent (>30 days) ($)", "> 360 Days Delinquent ($)", "31-60 Days Delinquent ($)")
delinq.df <- filter(df3, Variable %in% delinq.var)

g <- ggplot(data = delinq.df, aes(x = Variable, y = value))
g + geom_bar(aes(fill = Variable), stat = "identity", position = "dodge") +
  ggtitle("Delinquency Status") +
  scale_fill_discrete(guide = FALSE) +
  xlab("") + 
  ylab("") +
  coord_polar(theta = "y")

#---------------------------------------------------------------------------------------
#Creating the time series graph
#---------------------------------------------------------------------------------------
df5 <- filter(df2, Tranche == v[1], Variable == v1[6])
filtered_data <- filter(bond.data, Tranche %in% input$bond1)

cpr1_data <- filter(status_filtered(), Variable == "Since Issuance CPR (%)")

j <- ggplot(df5, aes(x=variable, y = value, group = 1)) +
  geom_line() +
  ggtitle("CPR(1) Time Series") +
  ylab(" ")
j

    








