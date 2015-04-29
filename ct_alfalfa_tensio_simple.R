require(ggplot2)
require(reshape)
library(scales)
library(data.table)
#SETTINGS
foo=read.csv("C:/Campbellsci/PC200W/CR1000-AB2_Table1.csv") #path to comma-separated data file (one line header)
bar=read.csv("C:/Campbellsci/PC200W/CR1000-AB2_Table2.dat")
PT_id = 2:21  # Range for columns containing pressure transducer data (20 transducers)
T_id = 43:51  # Range for columns containing temperature data (9 thermistors)
major_x = "24 hours" #Increment between labeled gridlines
minor_x = "2 hour"  #Increment between minor gridlines
origin_time="02/15/2015 15:00"
#origin_time="03/07/2015 15:00" #Time zero for data frames & graphs

#Check Battery
gplot(bar,aes(x=bar[,1], y=bar[,3]))+geom_point()+geom_point(aes(x=bar[,1], y=bar[,3]))

#Construct time series
TIMESTAMP = as.POSIXct(strptime(foo[,1],"%m/%d/%Y %H:%M"))
origin=as.POSIXct(strptime(origin_time,"%m/%d/%Y %H:%M")) #initial time for series
keep = ((as.numeric(TIMESTAMP)-as.numeric(origin) > 0))
#keep = (substr(TIMESTAMP, 12, 16)=="6:00")
TIMESTAMPa=TIMESTAMP[keep]

#Remove convert to mBar, remove NaN values and values prior to specified origin
foo2=foo
foo2[,PT_id+1]=(foo[,PT_id+1]-370)/1.742
data=foo2[keep,]


#Data formatting - Pressure
PT_idl = factor(PT_id %% 4)
levels(PT_idl)=c("D","D","S","S")
PT_loc = vector()
for(i in 1:5) {  PT_loc=c(PT_loc, rep(i,4)) }
#PT_loc
PT_map=data.frame(id=PT_id, depth=PT_idl,plot=PT_loc)
PT_map
df_PT=na.omit(as.data.frame(cbind(TIMESTAMPa,data[,PT_id+1])))
df_PT[,PT_map[PT_map$depth=="S",]$id]=df_PT[ ,PT_map[PT_map$depth=="S",]$id]+(6)
df_PT[,PT_map[PT_map$depth=="D",]$id]=df_PT[ ,PT_map[PT_map$depth=="D",]$id]+(12)
head(df_PT[,PT_map[PT_map$depth=="D",]$id]-df_PT[,PT_map[PT_map$depth=="S",]$id])

cosinor_analyzer(df_PT)

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
      scale_y_continuous(limits = c(0,400))+
      scale_x_datetime(breaks = major_x, minor_breaks = minor_x, labels=date_format("%d/%m %H:%M"))+
      scale_colour_discrete(name="Depth")+
      theme_classic()+
      theme(panel.grid.major=element_line(size=.5, color = "dark grey"),panel.grid.minor=element_line(size=.2, color = "light grey"));
  print(d)
  print(j)
  j=j+1
}

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
      geom_point()+
      geom_point(aes(x=df_T[,2],y=df_T[,i],col=df_T[,1]))+
      labs(x="Time",y="Temperature, C")+
      scale_y_continuous(limits = c(10,30))+
      scale_x_datetime(breaks = major_x, minor_breaks = minor_x, labels=date_format("%d/%m %H:%M"))+
      scale_shape_discrete(name="Depth")+
      scale_colour_discrete(name="Depth")+
      theme_classic()+
      theme(panel.grid.major=element_line(size=.5, color = "dark grey"), panel.grid.minor=element_line(size=.2, color = "light grey"))
  print(d)
}

