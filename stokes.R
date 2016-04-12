
foo1=read.csv("tensio_data\\tensio_4_5_14.csv")
names(foo1)=c("i","ts","minutes","matric","irrig")

irrigwhen=which(foo1$irrig==1)
t_irrig=sum(foo1$irrig[irrigwhen])*15 #Duration of irrigation in minutes
irrig_start=irrigwhen[1]
irrig_stop=irrigwhen[length(irrigwhen)]
ts_irrig=foo1$minutes-foo1$minutes[irrig_start]

irrig_vol=12000 #gallons
irrig_rate=irrig_vol*(1/7.48)/(t_irrig*60) #cubic feet per second
plot_area=1000 #ft squared
irrig_rate_length=irrig_rate/plot_area*.3048 #irrigation rate meters per second
irrig_rate_length_mmhr=irrig_rate_length*1000*3600


tensio_calib=read.csv("tensiometer_calibration.csv")
cur=tensio_calib[8,]
theta_s=mean(cur$vgm_ts)
theta_r=mean(cur$vgm_tr)
alpha=mean(cur$vgm_alpha)
n=mean(cur$vgm_n)
m=1-(1/n)
K_s=mean(cur$vgm_ksat)

Se=((1+(alpha*foo1$matric)^n)^(-m)) #Calculate effective saturation
Se_wilting=((1+(alpha*1500)^n)^(-m)) #plant available water is any water content greater than this value
min(Se)
max(Se)

#Starts dataset from onset of irrigation
data=data.frame(ts=ts_irrig[irrig_start:length(ts_irrig)], 
                VWC=Se[irrig_start:length(ts_irrig)]*cur$vgm_ts)

w_s = max(data$VWC)-min(data$VWC) #Amplitude of water content wave

v = 0.6/(which(data$VWC==max(data$VWC))*15*60) # Velocity of water content wave, m/s

c = 3*v #celerity 'c' is the velocity of any change that rides on the water film

Ft = sqrt(v/9.81*3*(10^(-6))) # Germann et al 2007 film thickness in meters
Ft_um=Ft*1000000 #Film thickness in microns

t_i=1.5*t_irrig
tiwhen=which(data$ts > t_i)[1]
z_i=(Ft^2)*t_irrig*9.81/(2*10^(-6))

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

Tdrainage=getTimeDrainage(data$VWC,which(data$VWC==max(data$VWC)))
#Tdrainage=which(data$VWC==max(data$VWC))
maxwc=max(data$VWC)
endwc=getEndVWC(data$VWC,Tdrainage)
startwc=data$VWC[1]

plot(diff(drainseries))

#stored water
stored=endwc-startwc
leached=maxwc-endwc
peakchange=maxwc-startwc



drainseries=data$VWC[Tdrainage:length(data$VWC)]
draintime=data$ts[Tdrainage:length(data$ts)]-data$ts[Tdrainage]
draintime[1] = 1
drain=data.frame(ts=draintime,VWC=drainseries)
plot(drain$VWC~log(sqrt(drain$ts)))

LF=max(data$VWC)-getEndVWC(data$VWC,Tdrainage)
L=LF/Ft

Reynolds=Ft*v*1
data$VWC[length(data$VWC)]-data$VWC[1]

#Conceptual water balane graph
l2=