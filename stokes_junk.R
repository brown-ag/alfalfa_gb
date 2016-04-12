#andrew brown
install.packages("zoo")
library(zoo)
install.packages("hydroTSM")
library(hydroTSM)
foo1=read.csv("tensio_data\\tensio_4_5_14.csv")
names(foo1)=c("i","ts","minutes","matric","irrig")
#write.csv(file="tensio_data\\tensio_4_5_14_zoo.csv",x=foo1[,2:length(names(foo1))])

zoo1=read.zoo("tensio_data\\tensio_4_5_14_zoo.csv",sep=",",tz="",format="%m/%d/%Y %H:%M")

hydroplot(zoo1)

foo1=read.csv("tensio_data\\tensio_4_5_14.csv")
foo2=read.csv("tensio_data\\tensio_4_6_14.csv")
foo3=read.csv("tensio_data\\tensio_4_7_14.csv")
foo4=read.csv("tensio_data\\tensio_4_8_14.csv")
#foo1=read.csv("tensio_data\\tensio_4_5_15.csv")
#foo2=read.csv("tensio_data\\tensio_4_6_15.csv")
#foo3=read.csv("tensio_data\\tensio_4_7_15.csv")
#foo4=read.csv("tensio_data\\tensio_4_8_15.csv")
events=read.csv("ct_alfalfa_events.csv") 

names(foo1)=c("i","ts","minutes","matric","irrig")
names(foo2)=c("i","ts","minutes","matric","irrig")
names(foo3)=c("i","ts","minutes","matric","irrig")
names(foo4)=c("i","ts","minutes","matric","irrig")
foo2$minutes=foo1$minutes[length(foo1$minutes)]+foo2$minutes
foo=rbind(foo1,foo2,foo3,foo4)


tensio_calib=read.csv("tensiometer_calibration.csv")
cur=tensio_calib[8,]
theta_s=cur$vgm_ts
theta_r=cur$vgm_tr
alpha=cur$vgm_alpha
n=cur$vgm_n
m=1-(1/n)
K_s=cur$vgm_ksat

Se=((1+(alpha*foo$matric)^n)^(-m)) #Calculate effective saturation for all matric potentials
hh=(((cur$sat_eff^(1/-m)-1)^(1/n))/alpha) #Calculate hydraulic head from calibration effective saturation

Sefc=((1+(alpha*sumarow$fcend)^n)^(-m)) #Calculate effective saturation at field capacity (nighttime)

fal=c(initial,final)
sefl=((1+(alpha*fal)^n)^(-m)) #Calculate initial and final water contents 
deltawc=sefl[2]-sefl[1]       #Calculate change in water content

plot(as.POSIXct(foo$ts),Se*cur$vgm_ts)
which(foo$irrig==1)
abline(v=as.POSIXct(foo$ts[which(foo$irrig==1)]))

plot(Se[which(foo$irrig==1)])
plot(Se[which(foo$irrig==0)])
plot(Se)

