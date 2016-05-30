library(lubridate)
library(ggplot2)

## Read in matric potential, temperature and event data
foo=read.csv("C:/Campbellsci/PC200W/CR1000-AB2_Table1.csv")
  # Tensiometer and temperature time series
events=read.csv("ct_alfalfa_events.csv") 
  # Flood event times and control volumes
tensio=read.csv("tensio_events.csv")
  # Mapping of tensiometer ID to plot #, treatment and "active" events
tensio_calib=read.csv("tensiometer_calibration.csv")

## ===SETUP===
PT_id = 2:21                      
  # Range for columns containing pressure transducer data (20 transducers)
T_id = 43:51                      
  # Range for columns containing temperature data (9 thermistors)
major_x = "1 week"                
  # Increment between labeled gridlines
minor_x = "1 day"                 
  # Increment between minor gridlines
origin_time="01/26/2015 10:00"    
  # Origin for time axis

## Formatting
TIMESTAMP = as.POSIXct(strptime(foo[,1],"%m/%d/%Y %H:%M"))   # Make POSIX datetime from time column
event_start = as.POSIXct(strptime(events[,4],"%m/%d/%Y %H:%M"))   # Make POSIX datetime for flood event start and end times
event_end = as.POSIXct(strptime(events[,5],"%m/%d/%Y %H:%M"))
time_test=ts(TIMESTAMP,frequency=length(TIMESTAMP))               # Create a timeseries object for data timestamp
origin=as.POSIXct(strptime(origin_time,"%m/%d/%Y %H:%M"))        # Convert origin into POSIX datetime
index=seq(1,length(TIMESTAMP))
tindex=index*15      # Each record is 15 minutes, multiply index by 15 to yield time in minutes for each record
hour=as.numeric(substr(TIMESTAMP,12,13))      # Parse out hour of day
time_map=cbind(index,tindex,TIMESTAMP) #TODO add day, hour, event timestamps?

# Data transformations
keep=((as.numeric(TIMESTAMP)-as.numeric(origin) > 0))      
TIMESTAMPa=TIMESTAMP[keep]
TIMESTAMPn=seq(0,length(TIMESTAMPa))*15

foo2=foo #COPY loaded data
foo2[,PT_id+1]=(foo[,PT_id+1]-370)/1.742 #convert mV to mbar (cm H2O)
data=foo2[keep,] # Trim the data to start at the time axis origin (defined above)

matric_head=data[,PT_id+1] #make some more convenient frames and column names
names(matric_head)=seq(1,20)
matric_std=data[,PT_id+21]

temp_sensor=data[,T_id] #make some more convenient frames and column names
names(temp_sensor)=paste("T",seq(1,length(T_id),sep=""))
temp_std=data[,T_id+9]
T_id

subsetTimes=function(time_start,time_end,buf=3600) { #Returns data records corresponding to specified time interval
 start=makePOSIXTime(time_start)-2*buf #get data for 2 hours prior to start of event
 end=start+86400
 tend=makePOSIXTime(time_end)
 if(end>tend) end=tend
 gte=(TIMESTAMPa>=start)
 lt=(TIMESTAMPa<end)
 who=which((gte+lt)==2)
 return(who)
}

subsetDate=function(datestart,dateend) {
  gte=(TIMESTAMPa>=datestart)
  lt=(TIMESTAMPa<dateend)
  who=which((gte+lt)==2)
  return(who)
}

makePOSIXTime=function(tee) {
  return(as.POSIXct(strptime(tee,"%m/%d/%Y %H:%M")))
}

#Calculate surface boundary conditions

#Assumed geometric "constants" for all 5 plots - can be treated on an individual basis also
ch_depth=30#mean(c(20,21,25,21,25,23,22,22,20,25,22,22,22,22,19,22,23,21,24,22,22,21,20,21,22,20,22)) #22 cm vertical
ch_width=75#mean(c(2.5,2.5,2.6,2.6,2.5,2.5))*30.48
plot_width=600#mean(c(20,18,19,20,22))*30.48
plot_length=1500#50*30.48
gal_per_cc=0.000264172
ch_stor_gal=((2*ch_width)*plot_length*(ch_depth/2))*gal_per_cc #gallons of storage in both side channels per plot
tensio_clust_l=plot_length/2*30.48 #tensiometers at 1/2 length of plot
tensio_clust_w=plot_width/2*30.48
plot_stor_gal=((plot_length*plot_width)+(plot_length*ch_width*2))*(ch_depth/2)*gal_per_cc
total_stor_gal=ch_stor_gal+plot_stor_gal

ch_fill_time=0
plot_fill_time=0
efficiency=0
for(e in 1:length(events$Date)) {
  flowrate=events$Flow.rate[e]
  ch_fill_time[e]=ch_stor_gal/flowrate
  plot_fill_time[e]=plot_stor_gal/flowrate
  efficiency[e]=plot_fill_time[e]/events$minutes[e]
}
plot(total_stor_gal*(2-efficiency),events$gallons)

t_arrival=ch_fill_time+(plot_fill_time/2)
events2=cbind(events,t_arrival)
par(mfrow=c(1,1))

#Makes campbell tract ET/rainfall/treatments
CT_ET=read.csv("CT_ET_2015.csv")
flood_days=na.omit(yday(as.POSIXct(events$Date,format="%d-%b")))
first_day=min(flood_days)
last_day=max(flood_days)
jan_start=flood_days[1]
jan_stop=flood_days[3]
feb_start=flood_days[4]
feb_stop=flood_days[9]
mar_start=flood_days[11]
mar_stop=flood_days[17]
plot(CT_ET$rain,type="l",col="blue",xlab="Day of year (2015)",ylab="Rainfall or Reference ET, mm",xlim=c(0,100),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
points(CT_ET$eto)
abline(v=first_day,lty=3,lwd=2,col="red")
abline(v=last_day,lty=3,lwd=2,col="red")
 #abline(v=last_day)
 #abline(v=jan_start)
 text(y=12.5,x=(jan_start+(jan_stop-jan_start)/2),"January")
 abline(v=jan_stop,lty=3,lwd=2,col="red")
 abline(v=feb_start-1,lty=3,lwd=2,col="red")
 text(y=12.5,x=(feb_start+(feb_stop-feb_start)/2),"February")
 abline(v=feb_stop+1,lty=3,lwd=2,col="red")
 abline(v=mar_stop,lty=3,lwd=2,col="red")
 text(y=12.5,x=(mar_start+(mar_stop-mar_start)/2),"March")
 abline(v=mar_start,lty=3,lwd=2,col="red")
(sum(CT_ET$eto[first_day:last_day]))
(sum(CT_ET$eto[feb_start:feb_stop]))
(sum(CT_ET$eto[mar_start:mar_stop]))
legend(-2,10,legend=c("Reference Evapotranspiration","24hr Rainfall","Treatment period"), lty = c(0,1,3), lwd=c(0,1,2), pch=c(1,-1,-1), col = c("black","blue","red"))
 
#Cumulative recharge data, March High
crchg=data.frame()
k=te=1
for(te in seq(1,4)) {
  setz=tensio[te,]
  tetreat=paste(setz$time,setz$treat)
  tevents=events[setz$eventstart:setz$eventend,]
  dates=as.POSIXct(strptime(as.character(tevents$Date),"%d-%b"))
  dstart=dates[1]
  dend=dates[length(dates)]
  mhrange=yday(dates)
  etr=CT_ET[mhrange,3]
  etrft=(etr*0.1/2.54)/12
  plot(dates,cumsum((tevents$gallons/7.48)/1000),type="b")
  plot(dates,cumsum((tevents$gallons/7.48)/1000),type="b",col=2)
  k=k+1
}

dfsum=data.frame(plot=numeric(),event=numeric(),tensio=numeric(),startmatr=numeric(),endmatr=numeric(),endsd=numeric(),matmax=numeric())
dfday=data.frame(1:96)
for(p in 1:5) { #loop through 5 monitored plots
  plot=tensio[p,]
  tid=c(plot$s1,plot$s2,plot$d1,plot$d2)
  eid=seq(plot$eventstart,plot$eventend)
  par(mfcol=c(length(tid),length(eid)))
  for(e in eid) {
    start_time=events[e,]$START #get event start time (water on)
    off_time=events[e,]$END     #get water off time
    end_time=events[e+1,]$START #get start of next event (AKA end of current)
    entire=subsetTimes(start_time,end_time)
    minutes=seq(0,length(entire)-1)*15
    irrig=subsetTimes(start_time,off_time,buf=0)
    irr_start=which(entire==irrig[1])
    dpotdt=diff(matric_head[entire,t])
    for(t in tid) {
      #plot(minutes,matric_head[entire,t],main=paste(c(e,t),sep=":"),ylim=c(0,300))
      irr_stop=which(entire==irrig[length(irrig)])
      #abline(v=c(minutes[irr_start],minutes[irr_stop]))
      #abline(v=minutes[irr_start]+events2$t_arrival[e],col="red")
      #abline(v=minutes[Ta],col="red")
      #plot(TIMESTAMPa[entire],matric_std[entire,t],main=paste(c(e,t),sep=":"),ylim=c(0,100))
      irrig_on=rep(0, length(entire))
      irrig_on[seq(irr_start,irr_stop,1)]=1
      fooder=data.frame(TIMESTAMP[entire],minutes,matric_head[entire,t],irrig_on)
      #write.csv(fooder, paste("./tensio_data/tensio_", p, "_",e,"_", t,".csv",sep=""))
      print( paste("./tensio_data/tensio_", p, "_",e,"_", t, " - Irrstart:", minutes[irr_start], " to Irrstop:", minutes[irr_stop], " arriving at ", minutes[irr_start]+events2$t_arrival[e]))
      nighttime=which(minutes > 1200) 
      #plot(matric_head[nighttime,])
      
      fcstart=(mean(fooder[1:4,3]))
      matmax=min(fooder[,3])
      fcend=(mean(fooder[nighttime,3]))
      fcsd=(sd(fooder[nighttime,3]))
      dfsum=rbind(dfsum,data.frame(p,e,t,fcstart,fcend,fcsd,matmax))
      dfday=cbind(dfday,matric_head[entire,t])
    }
  }
}
dfday=dfday[,2:length(names(dfday))]
tensio_calib2=cbind(depth=rep(c("D","S"),5),tensio_calib)
#tc=tensio_calib2[tensio_calib2$depth=="S",]
means=aggregate(tensio_calib2[,3:length(colnames(tensio_calib2))],FUN=mean,by=list(tensio_calib2$depth))
sds=aggregate(tensio_calib2[,3:length(colnames(tensio_calib2))],FUN=sd,by=list(tensio_calib2$depth))
theta_s=means$vgm_ts
theta_r=means$vgm_tr
alpha=means$vgm_alpha
n=means$vgm_n
m=1-(1/n)
K_s=means$vgm_ksat
Se_d=(1+(alpha[1]*seq(0,1000,1))^n[1])^(-m[1])#*(theta_s[1]-theta_r[1])+theta_r[1]
Se_s=(1+(alpha[2]*seq(0,1000,1))^n[2])^(-m[2])#*(theta_s[2]-theta_r[2])+theta_r[2]
Se_s_mat=(1+(alpha[2]*dfsum$fcstart)^n[2])^(-m[2])*(theta_s[2]-theta_r[2])+theta_r[2]
dfsum=cbind(dfsum, vwcstart=Se_s_mat)
write.csv(dfsum,file="tensio_event_fc.csv")

par(mfrow=(c(1,2)))
plot(Se_d*K_s[1],ylim=c(0,150))
plot(Se_s*K_s[2],ylim=c(0,150))

#Plots plotwise retention functions
par(mfrow=c(1,2))
Ses=data.frame(nrow=10001)
for(ee in 1:10){
  cur=tensio_calib[ee,]
  theta_s=cur$vgm_ts
  theta_r=cur$vgm_tr
  alpha=cur$vgm_alpha
  n=cur$vgm_n
  m=1-(1/n)
  K_s=cur$vgm_ksat
  Se=((1+(alpha*seq(0,100000,10))^n)^(-m))#*(theta_s-theta_r)+theta_r)
  hh=(((cur$sat_eff^(1/-m)-1)^(1/n))/alpha)
  Ses=cbind(Ses,Se)
  plot(log10(seq(0,100000,10)),Se)
  abline(v=cur$sat_eff)
  abline(h=log10(hh))
  print(hh)
}

findMatricMaxChange=function(matrx) { #Finds the maximum change in a matric potential dataset
  ddm=diff(c(matrx[1],matrx)) #Adds an identical value to beginning to give a 0 at start of dataset, then calculates deltas
  return (which(ddm==min(ddm))[1])
} 

getTa=function(matrx,thresh) {
  #Start search from max change in matric potentialrge 
  start=findMatricMaxChange(matrx)
  for(i in seq(1,start-1)) {
    ii=start-i #scan back
    if((matrx[ii]-matrx[ii+1]) < thresh)
      return(ii+1)
  }
  return (-1)
}

getLastContinuous=function(seqe,fc,start=0) {
  #print(seqe<fc)
  for(ii in seqe[start:length(seqe)]) {
    if(ii > fc) {
      asdf=which(matr==ii)
      dval=asdf[1]-1
      return(dval)
    }
  }
  return(-1)
}

getTimeDrainage=function(series,start,thresh=0.001) {
  subs=series[start:length(series)]
  last=subs[1]
  for(s in 2:length(subs)) {
    difff=abs(subs[s]-last)
    print(difff)
    if(difff>thresh) {
      return(start+s)
    }
    last=subs[s]
  }
  return(-1)
}

getEndVWC=function(series,dstart) {
  drainage=series[dstart:length(series)]
  return(min(drainage))
}

#Calculation of event water storage and duration of saturation by event
mm=2
Ses2=data.frame(1:96)
par(mfrow=c(1,1))
wcchange=c()
durat=c()
drainvol=c()
for(mm in 2:(length(names(dfday)))) {
  sumarow=dfsum[mm-1,] 
  matr=dfday[,mm-1]
  
  for(m in 2:length(matr)) {
    if(is.na(matr[m]))
      matr[m] = matr[m-1]
    if(matr[m] < 0)
      matr[m] = (matr[m-1]+matr[m+1])/2
  }
  
  initial=mean(matr[1:4])
  final=mean(matr[length(matr)-4:length(matr)])
  sinitial=sd(matr[1:4])
  sfinal=sd(matr[length(matr)-4:length(matr)])  
  
  gfc=which(matr<=sumarow$fcm)
  lfc=which(matr>sumarow$fcm)
  
  cur=tensio_calib[2,]
  #if((t %% 2) == 0) {
  #  cur=tensio_calib[2*p,]
  #} else {
  #  cur=tensio_calib[(2*p),]
  #}
  
  #Get van Genuchten parameters corresponding to current plot
  theta_s=cur$vgm_ts
  theta_r=cur$vgm_tr
  alpha=cur$vgm_alpha
  n=cur$vgm_n
  m=1-(1/n)
  K_s=cur$vgm_ksat
  
  Se=((1+(alpha*matr)^n)^(-m)) #Calculate effective saturation for all matric potentials
  hh=(((cur$sat_eff^(1/-m)-1)^(1/n))/alpha) #Calculate hydraulic head from calibration effective saturation
  
  Sefc=((1+(alpha*sumarow$fcend)^n)^(-m)) #Calculate effective saturation at field capacity (nighttime)
  
  fal=c(initial,final)
  sefl=((1+(alpha*fal)^n)^(-m)) #Calculate initial and final water contents 
  deltawc=sefl[2]-sefl[1]       #Calculate change in water content
  

  #Sethresh=((1+(alpha*thresh)^n)^(-m))

  Ses2=cbind(Ses2,Se)
  #plot(Se,matr,xlim=c(0,1),ylim=c(0,500))
  
  csatch=cumsum(c(0,diff(Se)))
  csamax=which(Se==max(Se))[[1]]
  
  rechargevol=Se[csamax]-getEndVWC(Se,csamax)
  drainvol=c(drainvol,rechargevol)
  Ta=getTa(matr,0.01) 
  drained=which(Se>Sefc)    #Matric potentials with lower magnitude than the final correspond to more saturated conditions.
  width=drained  
  Tfc=getLastContinuous(matr,sumarow$fcend,csamax)

  duration=Tfc-Ta
  filltime=csamax-Ta
  draintime=Tfc-csamax
  
  if(sumarow$p<5) {
    plot(Se*cur$porosity,main=paste(mm," Plot:",sumarow$p,"Event:",sumarow$e,"Tensio:",sumarow$t,"DeltaWC:",deltawc*cur$porosity, "Filled: ",filltime*15,"Drained: ",draintime*15),ylim=c(0.25,0.5))
    abline(h=Sefc*cur$porosity,col="RED")
    #abline(h=Sethresh*cur$porosity,col="RED")
    abline(v=csamax[[1]],col="RED")
    abline(v=Ta,col="RED")
    abline(v=Tfc,col="RED")
  }
  wcchange=c(wcchange,deltawc*cur$porosity)
  durat=c(durat,duration)
}

drainvol*60

dfsum=cbind(dfsum,wcc=c(wcchange,0))
t=15
wabal_all=data.frame(tensio=0,event=0,pool="NA",volume=0)
#par(mfrow=c(2,1))
for(t in levels(factor(dfsum$t))) {
  plotr=floor((as.numeric(t)+3) / 4)
  setz=tensio[plotr,]
  tetreat=paste(setz$time,setz$treat)
  tevents=events[setz$eventstart:setz$eventend,] 
  dates=as.POSIXct(strptime(as.character(tevents$Date),"%d-%b"))
  dstart=dates[1]
  dend=dates[length(dates)]
  mhrange=yday(dates)
  etr=CT_ET[mhrange,3]
  etrft=(etr*0.1)
  allevents=which((dfsum$t)==t)
  applied=(((tevents$gallons/7.48)/1000)*30.48)
  stor=(dfsum[allevents,]$wcc*60)
  len=length(applied)
  for(s in 1:length(stor))
    if(stor[s] < 0) {
      stor[s-1] = stor[s-1]-abs(stor[s])
      stor[s] = 0
    }
  wabal=data.frame(tensio=t,event=dfsum[allevents,]$e,pool=c(rep("Recharge",len),rep("Storage",len),rep("ET",len)),volume=c((applied-stor-etrft)/applied,stor/applied,etrft/applied))
  print(wabal)
  waball_all=rbind(wabal_all,wabal)
  h <- ggplot(wabal, aes(event, volume, fill=pool))
  h=h+geom_bar(stat="identity")
  h=h+geom_ribbon(aes(fill=pool,ymin=-20,ymax=400),position = 'stack')
  print(h)
  #plot(dfsum[allevents,]$e,(cumsum((tevents$gallons/7.48)/1000)*30.48)-cumsum(dfsum[allevents,]$wcc*60),main=paste(t,plotr),ylim=c(0,500),type="b")
  #lines(dfsum[allevents,]$e,cumsum((tevents$gallons/7.48)/1000)*30.48)
}

#Sample event plot
mmm=c(67)
tt=c(8,7) #tensio calibration sample
depthz=c(60,150)
e=events[5,]
Ses2=data.frame(1:96)
par(mfrow=c(1,1))
pchar=c(1,2)
wcchange=c()
durat=c()
par(mfrow=c(1,1))
plot(x=(1:96)*15,type="n",xlim=c(0,1440),ylim=c(0.25,0.35),xlab="Time, minutes",ylab="Volumetric water content, cm/cm",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(v=(44-36)*15,lwd=2)
abline(v=(60-36)*15, lwd=2)
statz=data.frame(depth=numeric(),Ista=numeric(),Isto=numeric(),Tarr=numeric(),Tmax=numeric(),Tfic=numeric())
for(ii in 1:2) {
  mm=mmm[ii]
  tcal=tt[ii]
  sumarow=dfsum[mm-1,]  #Current row with 
  matr=dfday[,mm-1]
  
  for(m in 2:length(matr)) {
    if(is.na(matr[m]))
      matr[m] = matr[m-1]
    if(matr[m] < 0)
      matr[m] = (matr[m-1]+matr[m+1])/2
  }
  
  initial=mean(matr[1:4])
  final=matr[length(matr)]#mean(matr[(length(matr)-4):length(matr)])
  sinitial=sd(matr[1:4])
  sfinal=sd(matr[length(matr)-4:length(matr)])  
  
  gfc=which(matr<=sumarow$fcm)
  lfc=which(matr>sumarow$fcm)
  
  cur=tensio_calib[6,]
  
  #Get van Genuchten parameters corresponding to current plot
  theta_s=cur$vgm_ts
  theta_r=cur$vgm_tr
  alpha=cur$vgm_alpha
  n=cur$vgm_n
  m=1-(1/n)
  K_s=cur$vgm_ksat
  
  Se=((1+(alpha*matr)^n)^(-m)) #Calculate effective saturation for all matric potentials
  hh=(((cur$sat_eff^(1/-m)-1)^(1/n))/alpha) #Calculate hydraulic head from calibration effective saturation
  
  Sefc=((1+(alpha*sumarow$fcend)^n)^(-m)) #Calculate effective saturation at field capacity (nighttime)
  
  fal=c(initial,final)
  sefl=((1+(alpha*fal)^n)^(-m)) #Calculate initial and final water contents 
  deltawc=sefl[2]-sefl[1]       #Calculate change in water content
  
  #Sethresh=((1+(alpha*thresh)^n)^(-m))
  
  Ses2=cbind(Ses2,Se)
  #plot(Se,matr,xlim=c(0,1),ylim=c(0,500))
  
  csatch=cumsum(c(0,diff(Se)))
  csamax=which(Se==max(Se))[[1]]
  
  Ta=getTa(matr,0.01) #Get the value immediately after the last value falling within 2 stdeviations
  drained=which(Se>Sefc)    #Matric potentials with lower magnitude than the final correspond to more saturated conditions.\
  width=drained  
  Tfc=getLastContinuous(matr,sumarow$fcend)
  
  statz=rbind(statz,data.frame(depth=depthz[ii],Ista=9,Isto=24,Tarr=Ta,Tmax=csamax[1],Tfic=Tfc))
  
  if(sumarow$p<5) {
    lines((1:96)*15,Se*cur$porosity,col=mm,pch=pchar[ii],type="b")
    abline(h=sefl[2]*cur$porosity,col=mm)
    abline(h=sefl[1]*cur$porosity,col=mm)
    #abline(v=csamax[[1]],col=mm)
    #abline(v=Ta,col=mm)
    #abline(v=Tfc,col=mm)
  }
  #text(x=1000,y=(sefl[1]+((sefl[2]-sefl[1])/2))*cur$porosity,paste("Change in Soil Storage (cm/cm): ",round(sefl[2]-sefl[1],digits=3)))
  #legend(x=60*15,y=0.375,c("60 cm", "150 cm"),col=mmm,pch=c(1,2),cex=1.5,bty="o")
  write.csv(data.frame(time=(1:96)*15,vwc=Se*cur$porosity),"concept_hydrograph.csv")
  zw=c(0.6,1.5)
  LF=Se[csamax]-sefl[2]
  tdzw=c(23,41)
  c=(tdzw-8)*zw[ii]
  v=c/3
  Ft=sqrt(v/(9.8/(3*(10^-6))))
  w_mod=LF*sqrt((tdzw[ii]-8)/(((tdzw[ii]):96)-8))
  #lines(c(rep(0,tdzw[ii]),w_mod))
}

statz=transform(statz,Idur=Isto-Ista)
statz=transform(statz,Atime=Tarr-Ista)
statz=transform(statz,Mtime=Tmax-Tarr)
statz=transform(statz,Etime=Tfic-Tarr)
statz=transform(statz,DurRat=Etime/Idur)
statz=transform(statz,Velocity=(Tarr-Ista)*15/depth/100/60)
statz=transform(statz,Ksat=Velocity*0.5)

statz
