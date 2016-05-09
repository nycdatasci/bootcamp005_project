library(gdata) # for reading xls file 
library(RSQLite) # using rsql daabase 
library(VIM) #For the visualization and imputation of missing values.
library(ggplot2) # for ggplot2
library(VIM)# imputing values
library(Hmisc) #Load the Harrell miscellaneous library.
home_loan=read.table("~/Documents/shiny_project/fnma_sf2014c_loans.txt",header=FALSE,sep="", nrows = 1000)

msa = read.table("~/Documents/shiny_project/2014_SFCensusTractFNM2014/MSA_2014.txt")
names(home_loan)
home_loan

#Changing the col names 
col_xls=read.xls("~/Documents/shiny_project/col_names2.xlsx", sheet = 1)
home_col_names=col_xls$Field.Name.
colnames(home_loan)=home_col_names
colnames(home_loan_full)=home_col_names
#sum(home_loan_full$US_Postal_State_Code == 00)
# 171
#

con <- dbConnect(RSQLite::SQLite(), 'home.db')
dbWriteTable(con, "home_loan", home_loan)
dbReadTable(con, "home_loan")

# A zero row data frame just creates a table definition.
dbWriteTable(con, "mtcars2", mtcars[0, ])
dbReadTable(con, "mtcars2")

dbDisconnect(con)

#####Visualizing Missing Data#####
##################################

sum(is.na(home_loan))
#which(is.na(postal_code_full_state2))

#Impute the age of Borrower with mean age
home_loan$Age_of_Borrower=impute(home_loan$Age_of_Borrower,mean)
which(home_loan$Age_of_Co_Borrower==999)
home_loan[which(home_loan$Age_of_Borrower==999), "Age_of_Borrower"]= NA
sum(home_loan$Age_of_Borrower==999)
home_loan[[which(home_loan$Age_of_Borrower==999),home_loan$Age_of_Borrower]]= NA


#Imputing Age of Borrower with mean value 
home_loan_full[which(home_loan_full$Age_of_Borrower==999),33]=NA
home_loan[which(home_loan$Age_of_Borrower==999),33]=NA
home_loan_full$Age_of_Borrower=impute(home_loan_full$Age_of_Borrower,mean)
sum(is.na(home_loan_full$Age_of_Borrower))

#Imputing Age of Co_Borrower with mean
#sum(home_loan_full$Age_of_Co_Borrower==999)
#home_loan_full[which(home_loan_full$Age_of_Co_Borrower==999),34]=NA
#home_loan_full$Age_of_Co_Borrower=impute(home_loan_full$Age_of_Co_Borrower,mean)


## trying to plot ggplot 
sex_vec=c(1,2,3,4,9)
sex_desc=c("Male","Female","No Info","Not Applicable","Not available")

#Borrower Ethnicity
race_vec=c(1:7,9)
race_desc=c("American_Indian","Asian","African American","Native Hawai","White","No Information","Not App","Not Avail")

home_loan_msa_state_tmp$Borrower_Gender=factor(x=home_loan_msa_state_tmp$Borrower_Gender,levels=sex_vec,labels=sex_desc)


# Changing the Borrowers Sex to factors in full table
home_loan_msa_state_full$Borrower_Gender=factor(x=home_loan_msa_state_full$Borrower_Gender,levels=sex_vec,labels=sex_desc)
home_loan_msa_state_tmp=home_loan_msa_state
home_loan_msa_state_tmp$Borrower_Gender=factor(x=home_loan_msa_state_tmp$Borrower_Gender,levels=sex_vec,labels=sex_desc)

# Changing Borrowers Race to Factors
home_loan_msa_state_random$Borrower1_Race_National_Origin=factor(x=home_loan_msa_state_tmp$Borrower1_Race_National_Origin,levels=race_vec,labels=race_desc)

library(ggplot2)
s5=ggplot(data = home_loan_msa_state_tmp,aes(x=Borrower_Gender)) + 
  geom_bar(aes(fill= Borrower_Gender)) + 
  ggtitle("Gender Profile of Borrower")
  
s5
home_loan[which(home_loan$Age_of_Borrower==999), 33]=NA
home_loan$Age_of_Borrower= impute(home_loan$Age_of_Borrower, mean)

#Reading MSA table
msa_code=read.table("~/Documents/shiny_project/2014_SFCensusTractFNM2014/MSA_2014.txt",stringsAsFactors = FALSE)
msa_2=msa_code[,-2]  # remving the second column having = sign
#Renaming before applying the inner join
colnames(msa_2)=c("MSA_Code","MSA_Area")

#Attaching 40 column to our home_loan having MSA_Name
home_loan_msa=inner_join(home_loan,msa_2,by="MSA_Code")
#checking the dimenstion of dim(home_loan_msa) as 1000X40
# doing the same on our full table
home_loan_msa_full=inner_join(home_loan_full,msa_2,by="MSA_Code")
#checked the dimenionalty dim(home_loan_msa_full) = 1899729      40   Great !! 
 
# Reading city table
state_code=read.xls("~/Documents/shiny_project/2014_SFCensusTractFNM2014/state_city.xlsx")

#Renaming state codes 

colnames(state_code)=c("US_Postal_State_Code","state","county_code","county")
# cheching the state-code dimensionality as 3224 X 4

#finding disctinct state codes in state_code table

state2=state_code %>% distinct(US_Postal_State_Code)

# Checking the dimenionality of state_2 54X4

# Now select only US_Postal_State_Code and state

state_code_name=select(state2,1:2)

# check the dimenionality 54 X 2





#home_loan_state=left_join(home_loan,state2,by="US_Postal_State_Code")

state_full=data.frame(state.abb,state.name,stringsAsFactors = FALSE)

# dimenion of state_full as 50 X 2 

colnames(state_full)=c("state","state_full")

# now join the stae_code_name with state_full ### also check the colnames of both 

postal_code_full_state=inner_join(state_code_name,state_full,by="state")

# lost four postal_codes now try left join to have all 54 codes 

postal_code_full_state2=left_join(state_code_name,state_full,by="state")


# check dim of result as 54 X 3 having four NA values 
# Fixing them manually

#postal_code_full_state2[9,3]="DISTRICT OF COLUMBIA"
#postal_code_full_state2[52,3]="GUAM"
#postal_code_full_state2[53,3]="ADJUNTAS MUNICIPIO"
#postal_code_full_state2[54,3]="ST CROIX ISLAND"

#renaming as postal_code_state
postal_code_state=postal_code_full_state2
# Now join that with home_loan_msa
home_loan_msa_state=inner_join(home_loan_msa,postal_code_state,by="US_Postal_State_Code")
# can try with full data !!!!
home_loan_msa_state_full=inner_join(home_loan_msa_full,postal_code_state,by="US_Postal_State_Code")

# Finally reselcting random samples from full data
home_loan_msa_state_random=dplyr::sample_n(home_loan_msa_state_full,size=1000,replace=FALSE)
