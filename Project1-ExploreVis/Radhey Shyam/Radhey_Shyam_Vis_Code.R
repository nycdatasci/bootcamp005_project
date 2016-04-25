# Reading xls file
library(gdata)
credit_data=read.xls("~/credit_project/credit2.xls", sheet = 1)
credit_tbl=tbl_df(credit_data)
credi_tbl_temp=credit_tbl
#credit_tbl2=credit_tbl[,2:ncol(credit_tbl)]

# changing "sex" numerical values to factors
sex_vec=c(1,2)
sex_desc=c("male","female")
credi_tbl_temp$SEX=factor(x=credit_tbl$SEX,levels=sex_vec,labels=sex_desc)

# Creating Bar Plots for Gender as factors for fill and default as factor on x-axis
s=ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month)) + 
  geom_bar(aes (fill = SEX), position = "fill") + 
  ggtitle("Gender Profile of Default Payment Vs Non-Default")+
  xlab("")+ylab("")
s
s1=ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month)) + 
  geom_bar(aes (fill = SEX), position = "dodge") + 
  ggtitle("Gender Profile of Default Payment Vs Non-Default")+
  xlab("")+ylab("")
s1



#Changing Education numerical values to facotrs
edu_vec=c(1,2,3,4)
edu_desc=c("graduate school","university","high school","others")
credi_tbl_temp$EDUCATION=factor(x=credit_tbl$EDUCATION,levels=edu_vec,labels=edu_desc)

#Creating Bar Plots for Education as factors for fill and default as factor on x-axis
e2=ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month)) + 
  geom_bar(aes (fill = EDUCATION), position = "dodge") +
  ggtitle("Education Profile of Default Payment Vs Non-Default")+
  xlab("")+ylab("Number of Individuals")
e2
# Education bar with fill 
e3=ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month)) + 
  geom_bar(aes (fill = EDUCATION), position = "fill") + 
  ggtitle("Education Profile of Default Payment Vs Non-Default")+
  xlab("")+ylab("")
e3

# Changing Marital status numerical values to facors
married_vec=c(1,2,3)
married_desc=c("married","single","others")
credi_tbl_temp$MARRIAGE=factor(x=credit_tbl$MARRIAGE,levels=married_vec,labels=married_desc)
# Crearing Marrital Status bar chart with fill
m=ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month)) + 
  geom_bar(aes (fill = MARRIAGE), position = "fill") + ggtitle("Marital Status of Default Vs. Non Default")+ xlab("")
m
# Creating box for the marrital status

m2=ggplot(data = mpg, aes(x =
                         reorder(class,  hwy), y = hwy))
+ geom_boxplot()

# Changing "default.payment.next.month" tp factors
credit_vec=c(0,1)
credit_desc=c("Default Payment","No Default Payment")
credi_tbl_temp$default.payment.next.month=factor(x=credi_tbl_temp$default.payment.next.month,levels=credit_vec,labels=credit_desc)

# Creting Box PLot

h=ggplot(data = credi_tbl_temp,aes(x = SEX))
# h2= ggplot(data = credi_tbl_temp,aes(x = SEX))

#h1=ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month,y=AGE))

# GENDER Bar Plots 
h2=ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month)) + geom_bar(aes
                                                   (fill = SEX), position = "dodge")



# AGE Box Plot  Plots 
h3=ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month,y=AGE)) + geom_boxplot()
h3

# Violin Plot 
p= ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month,y=AGE)) +
  geom_violin(aes(fill =default.payment.next.month ))+ ggtitle("Age Profile of Default Payment Vs.Non Default")+xlab("")
p
# Violin Plot with Dotplot 
p2= ggplot(data = credi_tbl_temp, aes(x = default.payment.next.month,y=AGE))+
  geom_violin(fill = 'gray', alpha = 0.5) +
  geom_dotplot(aes(fill = default.payment.next.month), binaxis = "y", stackdir = "center")
p2

# Creating Age Histogram
g = ggplot(data = credi_tbl_temp, aes(x = AGE))
g6=g + geom_histogram(aes(binwidth = 10))+ facet_wrap(~default.payment.next.month )
g6

g7=

g = ggplot(data = credi_tbl_temp, aes(x = AGE))
g1=g + geom_histogram(aes(fill= default.payment.next.month),binwidth = 5)+facet_wrap(~default.payment.next.month )
g1


# Creating frequency poly
g2=g + geom_freqpoly() 
g2

# creating another color frequency poly with factes 

g3= g + geom_freqpoly(aes(color = default.payment.next.month )) +
  facet_wrap( ~ default.payment.next.month)
g3

# creating another color frequency poly without facets
g5= g + geom_freqpoly(aes(color = default.payment.next.month )) 
g5

# Creating Density plots 

g4= g + geom_density(aes(color = default.payment.next.month ))+ggtitle("Density Vs. Age Profile")+ylab("Denisty")
g4

# Calculating the default rates

total_rows=nrow(credi_tbl_temp)
group_by(credi_tbl_temp,default.payment.next.month) %>% 
  summarise(Default_Rate=percent((n()/total_rows)))
##Another
### default_tab=filter(credi_tbl_temp,default.payment.next.month=="Default Payment")
###nrow(default_tab)

##Calculating monthly default rate from April 2005 to September 2005
default_rate=vector(length=6)
names(default_rate)= c("April 2005","May 2005","June 2005","July 2005","August 2005","September 2005")
  c("April 2005","May 2005","June 2005","July 2005","August 2005","September 2005")
default_rate[6]= percent(nrow(filter(credi_tbl_temp,PAY_0 > 2 ))/total_rows)
default_rate[5]= percent(nrow(filter(credi_tbl_temp,PAY_2 > 2 ))/total_rows)
default_rate[4]= percent(nrow(filter(credi_tbl_temp,PAY_3 > 2 ))/total_rows)
default_rate[3]= percent(nrow(filter(credi_tbl_temp,PAY_4 > 2 ))/total_rows)
default_rate[2]= percent(nrow(filter(credi_tbl_temp,PAY_5 > 2 ))/total_rows)
default_rate[1]= percent(nrow(filter(credi_tbl_temp,PAY_6 > 2 ))/total_rows)

# Calculation the default rate for





#Separating months names
months_name=names(default_rate)
names(default_rate)= c("04-2005","05-2005","06-2005","07-2005","08-2005","09-2005")
#c("April 2005","May 2005","June 2005","July 2005","August 2005","September 2005")
# Plot of Default(rate)=default > 60 days vs months
data_rate = data.frame(months=months_name, default_rate=default_rate)
d=ggplot(data=data_rate,aes(x=months, y=default_rate))
d1=d+geom_point()+ggtitle("Default rate(default payment > 2 months) Vs. Months")+
  xlab("Months")+ylab("Default Rate")
d1
d2=d1+geom_point()+geom_line()
d2
d3=ggplot(data=data_rate,aes(x=months, y=default_rate))+ geom_smooth(aes( se = FALSE))
d3


# Correlation
library(corrplot)
corrplot(cor(credi_tbl_temp[,c(2,13:24)]), order = "hclust")



