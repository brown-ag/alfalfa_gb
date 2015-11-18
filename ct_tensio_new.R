## Read in matric potential, temperature and event data
foo=read.csv("C:/Campbellsci/PC200W/CR1000-AB2_Table1.csv")
  # Tensiometer and temperature time series
events=read.csv("ct_alfalfa_events.csv") 
  # Flood event times and control volumes
tensio=read.csv("tensio_events.csv")
  # Mapping of tensiometer ID to plot #, treatment and "active" events

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

temp_sensor=data[,T_id+30] #make some more convenient frames and column names
names(temp_sensor)=seq(1,20)
temp_std=data[,T_id+21]

subsetTimes=function(time_start,time_end,buf=3600) { #Returns data records corresponding to specified time interval
 start=as.POSIXct(strptime(time_start,"%m/%d/%Y %H:%M"))-2*buf
 end=as.POSIXct(strptime(time_end,"%m/%d/%Y %H:%M"))-2*buf
 gte=(TIMESTAMPa>=start)
 lt=(TIMESTAMPa<end)
 who=which((gte+lt)==2)
 return(who)
}

makePOSIXTime=function(tee) {
  return(as.POSIXct(strptime(tee,"%m/%d/%Y %H:%M")))
}

# par(mfrow=c(1,2))
# for(e in 5:10){#(length(events[,1])-1)) { #loop through all events
#   start_time=events[e,]$START #get event start time (water on)
#   off_time=events[e,]$END     #get water off time
#   end_time=events[e+1,]$START #get start of next event (AKA end of current)
#   during=subsetTimes(start_time,off_time)
#   after=subsetTimes(off_time,end_time)
#   entire=subsetTimes(start_time,end_time)
#   for(i in seq(1:20)) {
#     plot(TIMESTAMPa[entire],matric_head[entire,i],main=paste(c(e,i),sep=":"),ylim=c(0,500))
#     plot(TIMESTAMPa[entire],matric_std[entire,i],main=paste(c(e,i),sep=":"),ylim=c(0,100))
#   }
# }

for(p in 1:5) { #loop through 5 plots
  plot=tensio[p,]
  tid=c(plot$s1,plot$s2,plot$d1,plot$d2)
  eid=seq(plot$eventstart,plot$eventend)
  par(mfcol=c(length(tid),length(eid)))
  for(e in eid) {
    start_time=events[e,]$START #get event start time (water on)
    off_time=events[e,]$END     #get water off time
    end_time=events[e+1,]$START #get start of next event (AKA end of current)
    during=subsetTimes(start_time,off_time)
    after=subsetTimes(off_time,end_time)
    entire=subsetTimes(start_time,end_time)
    for(t in tid) {
      plot(TIMESTAMPa[entire],matric_head[entire,t],main=paste(c(e,t),sep=":"),ylim=c(0,300))
      abline(v=c(makePOSIXTime(start_time),makePOSIXTime(off_time)))
      #plot(TIMESTAMPa[entire],matric_std[entire,t],main=paste(c(e,t),sep=":"),ylim=c(0,100))
    }
  }
}

