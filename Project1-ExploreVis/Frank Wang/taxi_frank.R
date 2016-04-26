## 
##2014 Yellow Taxi Trip Data:  https://data.cityofnewyork.us/view/gn7m-em8n
##
library(dplyr)
library(ggplot2)
library(openxlsx)
library("reshape2")
library("ggplot2")
library(leaflet)


taxi<-read.xlsx('nyc_taxi_data.xlsx',sheet=1) 
names(taxi)

## ------------------------
## Part I: Data wrangling
## ----------------------
taxi$pickup_datetime <- 
  as.POSIXct(taxi$pickup_datetime * (60*60*24)
             , origin="1899-12-30"
             , tz="GMT")        
taxi$dropoff_datetime <- 
  as.POSIXct(taxi$dropoff_datetime * (60*60*24)
             , origin="1899-12-30"
             , tz="GMT")        
taxi<-filter(taxi,pickup_latitude!=0,dropoff_latitude!=0)[-which(is.na(taxi$dropoff_latitud)),]

# add more col
taxi$myhours = as.numeric(as.POSIXlt(taxi$pickup_datetime)$hour)
b1=ISOdate(2014,01,09,00,00,00) 
taxi$day = as.numeric(floor(difftime(taxi$pickup_datetime,b1,unit="day")))
taxi$triptime =as.numeric(difftime(taxi$dropoff_datetime,taxi$pickup_datetime,unit="mins"))
taxi$tipp =as.numeric(taxi$tip_amount/taxi$total_amount)

## regime selection
rx1=-74.1
rx2=-73.8
ry1=40.5
ry2=41
taxi<-filter(taxi,pickup_latitude<ry2,pickup_latitude>ry1)
taxi<-filter(taxi,pickup_longitude<rx2,pickup_longitude>rx1)
taxi<-filter(taxi,dropoff_latitude<ry2,dropoff_latitude>ry1)
taxi<-filter(taxi,dropoff_longitude<rx2,dropoff_longitude>rx1)
max_day=3
taxi<-filter(taxi,day<=max_day,day>0)
taximo<-filter(taxi,myhours>8 & myhours<12)
taxiaf<-filter(taxi,myhours>18 & myhours<23)

# ------------------------------
# Part II: location dependence
# -----------------------------
# plot the pickup and drop off locations

nbin_pl=800
zoom1=coord_cartesian(xlim = c(-74.05, -73.85),ylim = c(40.6, 40.90))
zoom2<-coord_cartesian(xlim(-74.1,-73.8)+ylim(40.5,41))
g3<-ggplot(data=taxi,aes(y=pickup_latitude,x=pickup_longitude))+geom_bin2d(bins=nbin_pl,drop=T) +zoom1
g3+theme_bw()+scale_fill_continuous(low="blue", high="red",trans = "log")+
  ggtitle("")+xlab("Longitude")+ylab("Latitude")+theme(legend.position = "top")+
  guides(fill = guide_legend(title = "Log(count)", title.position = "left"))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
g3
#ggsave('pick.png',width=6,height = 5,dpi=500)


g4<-ggplot(data=taxi,aes(y=dropoff_latitude,x=dropoff_longitude))+geom_bin2d(bins=nbin_pl,drop=T) +zoom1
g4+theme_bw()+scale_fill_continuous(low="blue", high="red",trans = "log")+
  ggtitle("")+xlab("Longitude")+ylab("Latitude")+theme(legend.position = "top")+
  guides(fill = guide_legend(title = "Log(count)", title.position = "left"))+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
g4

#leaflet(data = taxis[1:20000,]) %>% addTiles() %>%
#  addMarkers(~pickup_longitude, ~pickup_latitude, clusterOptions = markerClusterOptions())

leaflet(data = taxi[1:1000,]) %>% addTiles() %>%
  addMarkers(~pickup_longitude, ~pickup_latitude, clusterOptions = markerClusterOptions())

#leaflet(taximo[1:20000,]) %>% addTiles() %>%
#  addCircleMarkers(~pickup_longitude, ~pickup_latitude,
#    radius =2,
#    color = 'red',
#    stroke = FALSE, fillOpacity = 0.5)

#leaflet(taximo[1:20000,]) %>% addTiles() %>%
#  addCircleMarkers(~dropoff_longitude, ~dropoff_latitude,
#                   radius =2,
#                   color = 'blue',
#                   stroke = FALSE, fillOpacity = 0.5)

    
#ggsave('drop.png',width=6,height = 5,dpi=500)

##  ---------------------------------------------------------------------------------
##  Part II NET flow
##function to calculate the density, expolre the net flow for a particlur time range and location
##  (density plot vis location)
##  -------------------------------------------------------------------------------
pmesh<-function(x1,x2,y1,y2,nx,ny,samplex,sampley){
  xstep<-(x2-x1)/nx
  ystep<-(y2-y1)/ny
  nmesh<-(nx+1)*(ny+1)
  x<-rep(0,nmesh)
  y<-rep(0,nmesh)
  d<-rep(0,nmesh)
  for (i in 1:(nx+1)){
    for (j in 1:(ny+1)){
      n<-i+(j-1)*(nx+1)
      x[n]<-x1+xstep*(i-1)
      y[n]<-y1+ystep*(j-1)
    }
  }
  np=length(samplex)
  for (i in 1:np) {
    # if(samplex<x2 | samplex>x1 | sampley>y1 | sampley<y2){
    id=floor((samplex[i]-x1)/xstep)+1+floor((sampley[i]-y1)/ystep)*(nx+1)
    d[id]=d[id]+1
    #}
  }
  return(data.frame(x,y,d))
}
nx=100
ny=100

x1=rx1
x2=rx2
y1=ry1
y2=ry2
zoom1=coord_cartesian(xlim = c(-74.05, -73.85),ylim = c(40.6, 40.90))

day_selected=1  # 1 Friday
taxis<-filter(taxi,pickup_latitude<ry2,pickup_latitude>ry1)
taxis<-filter(taxis,pickup_longitude<rx2,pickup_longitude>rx1)
taxis<-filter(taxis,dropoff_latitude<ry2,dropoff_latitude>ry1)
taxis<-filter(taxis,dropoff_longitude<rx2,dropoff_longitude>rx1)
taxis<-filter(taxis,day==day_selected)
taximo<-filter(taxis,myhours>8 & myhours<12)
taxiaf<-filter(taxis,myhours>18 & myhours<23)


## Morning Time
xyd_pick<-pmesh(rx1,rx2,ry1,ry2,nx,ny,taximo$pickup_longitude,taximo$pickup_latitude)
xyd_drop<-pmesh(rx1,rx2,ry1,ry2,nx,ny,taximo$dropoff_longitude,taximo$dropoff_latitude)

xyd_tot<-xyd_pick
xyd_tot$drop<-xyd_drop$d
xyd_tot$dif<-(xyd_drop$d-xyd_pick$d)
xyd_tot$base<-ifelse(xyd_tot$dif>0,1,-1)
ggplot(xyd_tot,aes(x=x,y=y))+geom_point(aes(size=abs(dif),color=factor(base))) +scale_size_area()+
  xlab("Long")+ylab("Lat")+theme_bw()+theme(legend.position = "right")+theme(legend.position="none")+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))

#ggsave('Moveinout_Friday_MO.png',width=6,height = 5,dpi=500)

## Evening time 
xyd_pick<-pmesh(rx1,rx2,ry1,ry2,nx,ny,taxiaf$pickup_longitude,taxiaf$pickup_latitude)
xyd_drop<-pmesh(rx1,rx2,ry1,ry2,nx,ny,taxiaf$dropoff_longitude,taxiaf$dropoff_latitude)

xyd_tot<-xyd_pick
xyd_tot$drop<-xyd_drop$d
xyd_tot$dif<-(xyd_drop$d-xyd_pick$d)
xyd_tot$base<-ifelse(xyd_tot$dif>0,1,-1)
ggplot(xyd_tot,aes(x=x,y=y))+geom_point(aes(size=abs(dif),color=factor(base))) +scale_size_area()+
  xlab("Long")+ylab("Lat")+theme_bw()+theme(legend.position = "right")+theme(legend.position="none")+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"))

leaflet(data = taxis[1:20000,]) %>% addTiles() %>%
  addMarkers(~pickup_longitude, ~pickup_latitude, clusterOptions = markerClusterOptions())

leaflet(data = taxis[1:20000,]) %>% addTiles() %>%
  addMarkers(~dropoff_longitude, ~dropoff_latitude, clusterOptions = markerClusterOptions())

ggsave('pick_Friday_MO_map.png',width=6,height = 5,dpi=500)

####------------------------------------------------------------------- 
### Part 3: Airport activity, in and out vs time
####------------------------------------------------------------------- 
### regime
##LA
rx1=-73.8834
rx2=-73.8556
ry1=40.668
ry2=40.7740

##JFK
rx1=-73.8312
rx2=-73.7426
ry1=40.622
ry2=40.684


day_selected=3  # 1 Friday
taxip<-filter(taxi,pickup_latitude<ry2,pickup_latitude>ry1)
taxip<-filter(taxip,pickup_longitude<rx2,pickup_longitude>rx1)
taxid<-filter(taxi,dropoff_latitude<ry2,dropoff_latitude>ry1)
taxid<-filter(taxid,dropoff_longitude<rx2,dropoff_longitude>rx1)
taxip<-filter(taxip,day==day_selected)
taxid<-filter(taxid,day==day_selected)



hourdis<-taxip  %>%
  group_by(myhours)%.%    #,day) %>%
  summarise(pick=n())
hourdrop<-taxid  %>%
  group_by(myhours ) %>% #,day) %>%
  summarise(drop=n())

hourdis$drop<-hourdrop$drop
pickdrop <- melt(hourdis, id="myhours")

pad<-ggplot(data=pickdrop,aes(x=myhours, y=value, colour=variable))+geom_line(size=2)+geom_point(size=3)
pad<-pad+ggtitle("Passager pickup and dropoff at LaGuardia Airport")+
  xlab("Hours")+ylab("count")+theme_bw()+theme(legend.position = "top")+ 
  scale_colour_discrete(name ="",labels = c("Pickup","Dropoff"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
pad 
#if (day_selected==1 ){
#  ggsave('Airport_friday.png',width=6,height = 5,dpi=500)
#} else if (day_selected==2) {
#  ggsave('Airport_saturday.png',width=6,height = 5,dpi=500)
#} else {
#  ggsave('Airport_Sunday.png',width=6,height = 5,dpi=500)
#}

##tips
##normalized one
taxis<-filter(taxi,pickup_latitude<ry2,pickup_latitude>ry1)
taxis<-filter(taxis,pickup_longitude<rx2,pickup_longitude>rx1)
taxis<-filter(taxis,day==day_selected)

hourdis<-filter(taxis,tip_amount>0)  %>%
  group_by(myhours) %>%
  summarise(sur=sum(surcharge)/sum(total_amount),mta=sum(mta_tax)/sum(total_amount),tip=sum(tip_amount)/sum(fare_amount),fare=sum(fare_amount)/sum(total_amount),ttt=sum(total_amount)/sum(total_amount))

hourdis_long<-melt(hourdis, id="myhours")
tiptime<-ggplot(data=hourdis, aes(x=myhours, y=tip))+geom_point(size=3)+geom_line(size=2)+ylim(0.16,0.25)
tiptime+xlab("Hours")+ylab("tip")+theme_bw()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
ggsave('Tip_hr_airport_Sunday.png',width=6,height = 5,dpi=500)

### three day

taxis<-filter(taxi,pickup_latitude<ry2,pickup_latitude>ry1)
taxis<-filter(taxis,pickup_longitude<rx2,pickup_longitude>rx1)

hourdis<-filter(taxis,tip_amount>0)  %>%
  group_by(myhours,day) %>%
  summarise(sur=sum(surcharge)/sum(total_amount),mta=sum(mta_tax)/sum(total_amount),tip=sum(tip_amount)/sum(fare_amount),fare=sum(fare_amount)/sum(total_amount),ttt=sum(total_amount)/sum(total_amount))

tip<-ggplot(data=hourdis,aes(x=myhours,y=tip,group=day,color=as.factor(day)))+geom_line(size=2)+geom_point(size=2)
tip<-tip+xlab("Hours")+ylab("Tip ($)")+theme_bw()+theme(legend.position = "top")
tip + scale_colour_discrete(name ="",labels = c("Friday","Saturday","Sunday"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

ggsave('Tip_hr_JFKairport3days.png',width=6,height = 5,dpi=500)


##----------------------------------------
## Part 4 : passager number and paymenth method  
##-------------------------------------------
## plot the passager inforimation

passage<-taxi %>%
group_by(passenger_count) %>%
  summarise(.,grp=n())
pp<-ggplot(data=passage,aes(x=passenger_count,y=grp))+geom_bar(stat = "identity")
pp
#ggsave('passenger.png',width=6,height = 5,dpi=500)
## payment methods
paymentmethod<-taxi %>%
  group_by(payment_type) %>%
  summarise(.,pay=n())
pa<-ggplot(data=paymentmethod,aes(x=payment_type,y=pay))+geom_bar(stat = "identity")
pa +xlab("Pay Method")+ylab("count")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
ggsave('paymethod.png',width=6,height = 5,dpi=500)


##   --------------------------------------------------------------
##  Part5: 1D distribution for 3days
##  3 days distri=ibution of trip distance, trip time and tips
##  ---------------------------------------------------------------

dis<-ggplot(data=taxi,aes(x=trip_distance,group=day,color=as.factor(day)))+geom_freqpoly(bins=99,size=3,aes(y=..density..)) 
dis+ggtitle("")+xlab("Trip distance(mi)")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  xlim(0,20)+theme_bw()+theme(legend.position = "top")+ 
  scale_colour_discrete(name ="",labels = c("Friday","Saturday","Sunday"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
dis
#ggsave('trip3_distance.png',width=6,height = 5,dpi=500)
summary(taxi$trip_distance)

tri<-ggplot(data=taxi,aes(x=triptime,group=day,color=as.factor(day)))+geom_freqpoly(bins=99,size=3,aes(y=..density..)) 
tri+ggtitle("")+xlab("Trip time(min)")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+xlim(0,60)+theme_bw()+theme(legend.position = "top")+ 
  scale_colour_discrete(name ="",labels = c("Friday","Saturday","Sunday"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

#ggsave('trip3_time.png',width=6,height = 5,dpi=500)
summary(taxi$triptime)

##$$tip
num_zero_tip<-dim(filter(taxi,tip_amount==0))
num_zero_tip[1]/dim(taxi)[1]

taxi<-filter(taxi,tipp>0)
ti<-ggplot(data=taxi,aes(x=tipp,group=day,color=as.factor(day)))+geom_freqpoly(bins=99,size=1,aes(y=..density..)) #nbin_pl)
ti+ggtitle("")+xlab("Tip")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+xlim(0.001,0.5)+theme_bw()+theme(legend.position = "top")+ 
  scale_colour_discrete(name ="",labels = c("Friday","Saturday","Sunday","Monday"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

#ggsave('tip3_total_time.png',width=6,height = 5,dpi=500)
summary(taxi$tipp)

## how much spend for each trip
cost<-ggplot(data=taxi,aes(x=total_amount,group=day,color=as.factor(day)))+geom_freqpoly(bins=99,size=3,aes(y=..density..)) #nbin_pl)
cost+ggtitle("")+xlab("Cost per trip ($)")+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+xlim(0.00,100)+theme_bw()+theme(legend.position = "top")+ 
  scale_colour_discrete(name ="",labels = c("Friday","Saturday","Sunday"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

#ggsave('tip_cost.png',width=6,height = 5,dpi=500)


##
##PRAT 6 Hours distribution : picup/dropoff,cost,Fare...
##

hourdis<-taxi  %>%
  group_by(myhours,day) %>%
  summarise(hn=n(),fare=sum(fare_amount),sur=sum(surcharge),mta=sum(mta_tax),tip=sum(tip_amount),tot=sum(total_amount))

pa<-ggplot(data=hourdis,aes(x=myhours,y=hn,group=day,color=as.factor(day)))+geom_line(size=2)+geom_point(size=3)
pa<-pa+ggtitle("passager pickup")+xlab("Hours")+ylab("count")+theme_bw()+theme(legend.position = "top")
pa + scale_colour_discrete(name ="",labels = c("Friday","Saturday","Sunday"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

ggsave('pickup_hour_day.png',width=6,height = 5,dpi=500)


##cost
tot<-ggplot(data=hourdis,aes(x=myhours,y=tot,group=day,color=as.factor(day)))+geom_line(size=2)+geom_point(size=2)
tot<-tot+xlab("Hours")+ylab("Total ($)")+theme_bw()+theme(legend.position = "top")
tot + scale_colour_discrete(name ="",labels = c("Friday","Saturday","Sunday"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

ggsave('TotalNew.png',width=6,height = 5,dpi=500)

Far<-ggplot(data=hourdis,aes(x=myhours,y=fare,group=day,color=as.factor(day)))+geom_line(size=2)+geom_point(size=2)
Far<-Far+xlab("Hours")+ylab("Fare ($)")+theme_bw()+theme(legend.position = "top")
Far + scale_colour_discrete(name ="",labels = c("Friday","Saturday","Sunday"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))

ggsave('Fare.png',width=6,height = 5,dpi=500)

tip<-ggplot(data=hourdis,aes(x=myhours,y=tip,group=day,color=as.factor(day)))+geom_line(size=2)+geom_point(size=2)
tip<-tip+xlab("Hours")+ylab("Tip ($)")+theme_bw()+theme(legend.position = "top")
tip + scale_colour_discrete(name ="",labels = c("Friday","Saturday","Sunday"))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
tip
ggsave('tip.png',width=6,height = 5,dpi=500)


#### Tip hour






### ---------------------
###focus one small regime
### -------------------
##regime
rx1=-74.05
rx2=-73.85
ry1=40.6
ry2=40.9

zoom1=coord_cartesian(xlim = c(-74.05, -73.85),ylim = c(40.6, 40.90))

day_selected=3  # 1 Friday

taxis<-filter(taxi,pickup_latitude<ry2,pickup_latitude>ry1)
taxis<-filter(taxis,pickup_longitude<rx2,pickup_longitude>rx1)
taxis<-filter(taxis,dropoff_latitude<ry2,dropoff_latitude>ry1)
taxis<-filter(taxis,dropoff_longitude<rx2,dropoff_longitude>rx1)
taxis<-filter(taxis,day==day_selected)
taximo<-filter(taxis,myhours>8 & myhours<12)
taxiaf<-filter(taxis,myhours>18 & myhours<23)


##normalized one
hourdis<-filter(taxis,tip_amount>0)  %>%
  group_by(myhours) %>%
  summarise(sur=sum(surcharge)/sum(total_amount),mta=sum(mta_tax)/sum(total_amount),tip=sum(tip_amount)/sum(fare_amount),fare=sum(fare_amount)/sum(total_amount),ttt=sum(total_amount)/sum(total_amount))

hourdis_long<-melt(hourdis, id="myhours")

##line plot
g1<-ggplot(data=hourdis_long, aes(x=myhours, y=value, colour=variable)) +geom_line(size=3)+geom_point(size=4)
g1<-g1+xlab("Hours")+ylab(" ($)")+theme_bw()+theme(legend.position = "top")
g1+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))+
  scale_colour_discrete(name ="",labels = c("Surcharge","MTA","Tip","Fare","Total"))
ggsave('SaturdayN.png',width=6,height = 5,dpi=500)

# area plot
ggplot(data=hourdis_long, aes(x=myhours, y=value, colour=variable)) +geom_area(aes(fill=variable))+
  xlab("Hours")+ylab(" ($)")+theme_bw()+theme(legend.position = "top")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
# scale_color_discrete(name ="",labels = c   ("Surcharge","MTA","Tip","Fare","Total"))
ggsave('SunArea.png',width=6,height = 5,dpi=500)

tiptime<-ggplot(data=hourdis, aes(x=myhours, y=tip))+geom_point(size=3)+geom_line(size=2)+ylim(0.16,0.25)
tiptime+xlab("Hours")+ylab("tip")+theme_bw()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
ggsave('Tip_hr_Sunday.png',width=6,height = 5,dpi=500)

hm<-ggplot(data=taxis,aes(x=pickup_datetime))+geom_freqpoly(bins = 500)
hm+xlab("Hours")+ylab("count")+theme_bw()+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
hm+xlim(0,16)
ggsave('pickuptimeFriday.png',width=6,height = 5,dpi=500)


