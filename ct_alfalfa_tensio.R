
require(ggplot2)
require(reshape)
library(scales)
library(data.table)
#SETTINGS
foo=read.csv("C:/Campbellsci/PC200W/CR1000-AB2_Table1.csv") #path to comma-separated data file (one line header)
events=read.csv("../alfalfa_gb_git//ct_alfalfa_events.csv")
bar=read.csv("C:/Campbellsci/PC200W/CR1000-AB2_Table2.csv")
PT_id = 2:21  # Range for columns containing pressure transducer data (20 transducers)
T_id = 43:51  # Range for columns containing temperature data (9 thermistors)
major_x = "1 week" #Increment between labeled gridlines
minor_x = "1 day"  #Increment between minor gridlines
origin_time="02/16/2015 10:00"
#origin_time="03/07/2015 15:00" #Time zero for data frames & graphs

#Check Battery

battime=(((seq(0,length(bar[,1])))/24))
df=data.frame(cbind(as.numeric(bar[100:1400,3])-mean(bar[100:1400,3]),battime[100:1400]))
batfit=lm(df[,1]~df[,2],data=df)
plot(df[,2],df[,1])
abline(batfit)

#Construct time series
TIMESTAMP = as.POSIXct(strptime(foo[,1],"%m/%d/%Y %H:%M"))
event_start = as.POSIXct(strptime(events[,4],"%m/%d/%Y %H:%M"))
event_end = as.POSIXct(strptime(events[,5],"%m/%d/%Y %H:%M"))
origin=as.POSIXct(strptime(origin_time,"%m/%d/%Y %H:%M")) #initial time for series
tindex=seq(1,length(TIMESTAMP))*15
hour=as.numeric(substr(TIMESTAMP,12,13))#
keep=((as.numeric(TIMESTAMP)-as.numeric(origin) > 0))
#keep=((hour >= 18) | (hour <= 6))
#keep = (substr(TIMESTAMP, 12, 16)=="6:00")
n=0:23
dindex=floor(tindex/1440)
rad_time=((tindex/1440)%%1)*2*pi
day=data.frame(dindex,rad_time)
head(data[day[,1]==4,])
TIMESTAMPa=TIMESTAMP[keep]
TIMESTAMPn=seq(0,length(TIMESTAMPa))*15



#Remove convert to mBar, remove NaN values and values prior to specified origin
foo2=foo
foo2[,PT_id+1]=(foo[,PT_id+1]-370)/1.742
data=foo2[keep,]

#treatment information
  #timing
tfeb = 1:20
tmar = 21:40
 #intensity
high=c(1,1,0,0,0)
low=c(0,0,1,1,0)
feb=c(0,1,0,1,0)
mar=c(1,0,1,0,0)
 #control
control_plot=c(0,0,0,0,1)
zero=data.frame(rep(0,5))

plot=c(1,2,3,4,5)
plot[(feb+low)==2]
 
#retention curve
soil=c('loam','sandy loam')
theta_s=c(0.44,0.41)
theta_r=c(0.07, 0.06)
alpha=c(0.009,0.028)
n=c(1.53,1.46)
m=1-(1/n)
K_s=c(18.84/24,126/24)
Se_l=(1+(alpha[1]*seq(0,1000,1))^n[1])^(-m[1])*(theta_s[1]-theta_r[1])+theta_r[1]
Se_s=(1+(alpha[2]*seq(0,1000,1))^n[2])^(-m[2])*(theta_s[2]-theta_r[2])+theta_r[2]

#treatmentslice
foobah=df_PT[(day$dindex>=20 & day$dindex<=40),3]
clazz=SAX(foobah,alphabet=2,PAA=21,breakpoints="quantiles",collapse=NULL)
dt=length(foobah)/21
plot(df_PT[(day$dindex>=20 & day$dindex<=40),3])
last=-1
for(i in 1:length(clazz)) {
  if(last == -1) {
   last=clazz[i]
  }
  if(clazz[i] != last) abline(v=(i+1)*dt)
  last=clazz[i]
}

#Data formatting - Pressure
PT_idl = factor(PT_id %% 4)
levels(PT_idl)=c("D","D","S","S")
PT_loc = vector()
for(i in 1:5) {  PT_loc=c(PT_loc, rep(i,4)) }
#PT_loc
PT_map=data.frame(id=PT_id, depth=PT_idl,plot=PT_loc)
PT_map
df_PT=as.data.frame(cbind(TIMESTAMPa,data[,PT_id+1]))
df_PTn=as.data.frame(cbind(seq(1,length(TIMESTAMPa))*15,data[,PT_id+1]))
df_PT[,PT_map[PT_map$depth=="S",]$id]=df_PT[ ,PT_map[PT_map$depth=="S",]$id]+(6)
df_PT[,PT_map[PT_map$depth=="D",]$id]=df_PT[ ,PT_map[PT_map$depth=="D",]$id]+(12)
head(df_PT[,PT_map[PT_map$depth=="D",]$id]-df_PT[,PT_map[PT_map$depth=="S",]$id])
fooober=data.frame(df_PTn)
shallowtemp=df_T[T_map$depth=="5cm",3]
df_PTn=data.frame(df_PTn,shallowtemp)

#Percentage saturation (for extension blurb)
SATp=df_PT[2:20]

convertToSE=function(x,alpha,n) {
  return((1+(alpha*x)^n)^(-(1-(1/n))))
}




radian_time=((TIMESTAMPn/1440)%%1)*2*pi

day_time=floor((TIMESTAMPn/1440))

#Plot pressure data - by plot
j=1
for(i in seq(1,20,by=4)) {
  t=list()
  t = PT_map[PT_map$plot==j,][sum(PT_map$id==PT_id[(i):(i+3)])>0,2]
  d = ggplot(df_PT, aes(x=df_PT[,1],y=df_PT[,PT_id[i]]))+
    geom_line(aes(x=df_PT[,1],y=df_PT[,PT_id[i]],group=t[1],colour=t[1]))+
    geom_line(aes(x=df_PT[,1],y=df_PT[,PT_id[i+1]],group=t[2],colour=t[2]))+
    geom_line(aes(x=df_PT[,1],y=df_PT[,PT_id[i+2]],group=t[3],colour=t[3]))+
    geom_line(aes(x=df_PT[,1],y=df_PT[,PT_id[i+3]],group=t[4],colour=t[4]))+
    labs(x="Time",y="Matric Potential, mBar")+
    scale_y_continuous(limits = c(0,1000))+
    scale_x_datetime(breaks = major_x, minor_breaks = minor_x, labels=date_format("%d/%m %H:%M"))+
    scale_colour_discrete(name="Depth")+
    theme_classic()+
    theme(panel.grid.major=element_line(size=.5, color = "dark grey"),panel.grid.minor=element_line(size=.2, color = "light grey"))+
    geom_rect(data=df_PT, mapping=aes(xmin=event_start[16],xmax=event_end[16],ymin=0,ymax=1000),alpha=0.5)
  print(d)
  print(j)
  j=j+1
}
i=1


#graph for extension blurb
j=1
i=1
t=list()
t=factor(c("2 ft", "2 ft", "5 ft", "5 ft"))
origin_time2="03/16/2015 00:00"
endtime="03/23/2015 00:00"
origin2=as.POSIXct(strptime(origin_time2,"%m/%d/%Y %H:%M")) #initial time for series
endtime2=as.POSIXct(strptime(endtime,"%m/%d/%Y %H:%M")) #initial time for series
keep2=((as.numeric(TIMESTAMP)-as.numeric(origin2) > 0) & (as.numeric(TIMESTAMP)-as.numeric(endtime2) < 0)
)
dates=df_PT[keep2,1]
d = ggplot(df_PT[keep2,], aes(x=dates,y=convertToSE(df_PT[keep2,PT_id[i]],0.01,1.5)))+
  geom_rect(mapping=aes(xmin=event_start[15],xmax=event_end[15],ymin=60,ymax=100),linetype="blank",alpha=0.01)+
  geom_rect(mapping=aes(xmin=event_start[16],xmax=event_end[16],ymin=60,ymax=100),linetype="blank",alpha=0.01)+
  geom_line(aes(x=dates,y=convertToSE(df_PT[keep2,PT_id[i]],0.01,1.53)*100,group=t[1],colour=t[1]),linetype=11,size=1.75)+
  geom_line(aes(x=dates,y=convertToSE(df_PT[keep2,PT_id[i+1]],0.01,1.53)*100,group=t[2],colour=t[2]),linetype=11,size=1.75)+
  #geom_line(aes(x=dates,y=convertToSE(df_PT[keep2,PT_id[i+2]],0.03,1.46)*100,group=t[3],colour=t[3]))+
  #geom_line(aes(x=dates,y=convertToSE(df_PT[keep2,PT_id[i+3]],0.03,1.46)*100,group=t[4],colour=t[4]))+
  labs(x="Time",y="% Saturation")+
  scale_y_continuous(limits = c(60,100))+
  scale_x_datetime(breaks = "1 week",minor_breaks="1 day", labels=date_format("%m/%d/%Y"))+
  scale_colour_discrete(name="Depth")+
  theme_classic()+
  theme(text = element_text(size=72),panel.grid.major=element_line(size=.5, color = "dark grey"),panel.grid.minor=element_line(size=.2, color = "light grey"))
print(d)

integrateHeadTime = function(x,dt) {
  l=length(x)
  l
  max=x[l]
  max
  sum((max-x[1:l])*dt)  
}

day_min=day_max=day_mean=day_var=day_dh=day_fft=iht=day_dtheta=data.frame(0:max(dindex))
for(i in PT_id) {
  day_min[,i]=aggregate(df_PT[,i]~day[,1],data,function(x) min(x))[,2]
  day_max[,i]=aggregate(df_PT[,i]~day[,1],data,function(x) max(x))[,2]
  day_mean[,i]=aggregate(df_PT[,i]~day[,1],data,function(x) mean(x))[,2]
  day_var[,i]=(aggregate(data[,i]~day[,1], data, function(x) sd(x))[,2])^2
  day_dh[,i]=aggregate(df_PT[,i]~day[,1],data,function(x) (x[length(x)]-x[1]))[,2]
  day_dtheta[,i]=aggregate(day_mean_vwc[,i]~day[,1],data,function(x) (sum(x)*15)/1440)[,2]
  day_fft[,i]=fft(as.numeric(day_mean[,i]))
}
sum(1-day_mean_vwc[20:40,3])
day_mean_vwc=data.frame(1:5357)
for(i in PT_id) {
  if(sum(i == PT_map[PT_map$depth=="S",]$id)==1)
    day_mean_vwc[,i]=(1+(alpha[1]*df_PT[,i])^n[1])^(-m[1])*(theta_s[1]-theta_r[1])+theta_r[1]
  else
    day_mean_vwc[,i]=(1+(alpha[2]*df_PT[,i])^n[2])^(-m[2])*(theta_s[2]-theta_r[2])+theta_r[2]
}
matplot(day_mean_vwc[,PT_map[PT_map$depth=="S",]$id],pch="*")
sum(day_dtheta[1:35,4])

iht[,PT_id[5]]=integrateHeadTime(day_dh[,PT_id[5]],1)
par(mfrow=c(1,2))
matplot(day_dh[,PT_map[PT_map$depth=="S",]$id],pch='*')#,ylim=c(-200,100))
matplot(day_dh[,PT_map[PT_map$depth=="D",]$id],pch='*')#,ylim=c(-200,100))
matplot(day_mean[30:56,PT_map[PT_map$plot=="5",]$id],pch='*')#,ylim=c(-200,100))
matplot(day_mean[30:56,PT_map[PT_map$plot=="5",]$id],pch='*')#,ylim=c(-200,100))


library(stats)
spectrum(dmf)
par(new=TRUE)
matplot()
legend()
matplot(day_max[,PT_map[PT_map$depth=="S",]$id]-day_min[,PT_map[PT_map$depth=="S",]$id],pch='*',ylim=c(0,1000))
matplot(day_max[,PT_map[PT_map$depth=="D",]$id]-day_min[,PT_map[PT_map$depth=="D",]$id],pch='*',ylim=c(0,1000))


day_min[i:i+3]=aggregate(df_PTn[,i:i+3]~day[,1], data, function(x) min(x))[,2]
day_max[i:i+3]=aggregate(data[,i:i+3]~day[,1], data, function(x) max(x))[,2]
day_mean[i:i+3]=aggregate(data[,i:i+3]~day[,1], data, function(x) mean(x))[,2]
day_var[i:i+3]=(aggregate(data[,i:i+3]~day[,1], data, function(x) sd(x))[,2])^2

test=na.omit(data.frame(cbind(radian_time[1:5357], df_PTn[,3], df_PTn[,22])))
test[,2]=test[,2]-mean(test[,2])
plot(test[,2]~test[,1]+test[,3])


#Data formatting - Temperature
T_idl=factor(T_id %% 3)
T_pos=factor(c(1,1,1,2,2,2,3,3,3))
levels(T_idl)=c("25cm", "5cm", "15cm")
levels(T_pos)=c("Control","Low","High")
T_map=na.omit(data.frame(ID=T_id, depth=T_idl, plot=T_pos))
df_T=data.frame(rbindlist(list(cbind("5cm",TIMESTAMPa,data[,T_map[T_map$depth=="5cm",1]]),cbind("15cm",TIMESTAMPa,data[,T_map[T_map$depth=="15cm",1]]),cbind("25cm",TIMESTAMPa,data[,T_map[T_map$depth=="25cm",1]]))))

#Plot Temperature Data
for(i in 3:5) {
  d=ggplot(df_T, aes(x=df_T[,2],y=df_T[,i], group=df_T[,1], shape=df_T[,1],colour==df_T[,1]))+
    geom_line()+
    geom_line(aes(x=df_T[,2],y=df_T[,i],col=df_T[,1]))+
    labs(x="Time",y="Temperature, C")+
    scale_y_continuous(limits = c(10,30))+
    scale_x_datetime(breaks = major_x, minor_breaks = minor_x, labels=date_format("%d/%m %H:%M"))+
    scale_shape_discrete(name="Depth")+
    scale_colour_discrete(name="Depth")+
    theme_classic()+
    theme(panel.grid.major=element_line(size=.5, color = "dark grey"), panel.grid.minor=element_line(size=.2, color = "light grey"))
  print(d)
}
