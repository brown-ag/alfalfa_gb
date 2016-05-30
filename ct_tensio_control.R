#ct_tensio_control


foo=read.csv("C:/Campbellsci/PC200W/CR1000-AB2_Table1.csv")
events=read.csv("ct_alfalfa_events.csv") 
tensio=read.csv("tensio_events.csv")
tensio_calib=read.csv("tensiometer_calibration.csv")


## ===SETUP===
PT_id = ((21-3):21)+1                      
# Range for columns containing pressure transducer data (20 transducers)
#T_id = 43:51                      
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
time_map=cbind(index,tindex,TIMESTAMP)

# Data transformations
keep=((as.numeric(TIMESTAMP)-as.numeric(origin) > 0))      
TIMESTAMPa=TIMESTAMP[keep]
TIMESTAMPn=seq(0,length(TIMESTAMPa))*15

foo2=foo #COPY loaded data
foo2[,(2:21)+1]=(foo[,(2:21)+1]-370)/1.742 #convert mV to mbar (cm H2O)
data=foo2[keep,] # Trim the data to start at the time axis origin (defined above)

for(i in 1:4)
  plot(TIMESTAMPa,foo2[,PT_id[i]],ylim=c(0,1000))

control_tension=(rowMeans(foo2[,PT_id[1:2]]))
mh_tension=(rowMeans(foo2[,(6:7)+1]))
control_tension=mh_tension
cur=tensio_calib[10,] #control, shallow
theta_s=cur$vgm_ts#0.43
theta_r=cur$vgm_tr#0.07
alpha=cur$vgm_alpha#0.0124
n=cur$vgm_n
m=1-(1/n)
K_s=5.3*(10^-5)
Se=(((1+(alpha*control_tension)^n)^(-m)))
hh=(((cur$sat_eff^(1/-m)-1)^(1/n))/alpha)
plot(log10(control_tension),Se)
abline(v=cur$sat_eff)
abline(h=log10(hh))
print(hh)

plot(Se)

dailyFun = function(data,datecol=4,paramcol=3,FUNC=mean) {
  date <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 1)
  time <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 2)
  return(aggregate(data[,paramcol], by=list(date), FUN=FUNC))
}

tensio=data.frame(date=TIMESTAMPa,tension=control_tension,vwc=Se)

daily_control_tension=dailyFun(na.omit(tensio),datecol=1,paramcol=3,FUNC=median)
daily_control_tension=cbind(data.frame(doy=yday(daily_control_tension$Group.1),daily_control_tension))
plot(daily_control_tension[,1],daily_control_tension[,3])

startind=which(control$year==2015 & control$day == daily_control_tension[1,1])
subcon=control[startind:nrow(control),]
pred_vwc=(subcon$stor/soilmax*5/3)
soilmax/5*3
lines(pred_vwc,type="l")
points(121:(121+41),daily_control_tension[,3])

abline(h=0.185490898)
abline(v=42)
RMSE=function(obs,pred) {
  sqrt(mean((obs-pred)^2))
}

test=data.frame(pred=pred_vwc[1:42],obs=daily_control_tension[1:42,3])
RMSE(test$obs,test$pred)
test



