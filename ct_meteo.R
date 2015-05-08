#Daily meteorological values
library(lubridate)
day_of_year=yday(daily_windspeed[,1])
#barometric pressure
baro=read.csv("ClimateData/alfRR_Baro.csv")
daily_baro=dailyFun(baro)[,2]*3.386 #kPa

rainfall=read.csv("ClimateData/alfCT_Rain_Tot24.csv")
daily_rainfall=dailyFun(rainfall)

#wind speed
mean_windspeed=read.csv("ClimateData/alfCT_WS2mMn.csv")
max_windspeed=read.csv("ClimateData/alfCT_WS2mMx.csv")
daily_windspeed=dailyFun(mean_windspeed)

#air temperature
atemp_2m=read.csv("ClimateData/alfCT_WS2mMn.csv")
atemp_10m=read.csv("ClimateData/alfCT_WS2mMn.csv")
daily_max_atemp2=dailyFun(atemp_2m,FUNC="max")
daily_min_atemp2=dailyFun(atemp_2m,FUNC="min")
daily_atemp2=dailyFun(atemp_2m)

#dewoint
dewpoint=read.csv("ClimateData/alfRR_DewPt.csv")
daily_dewpoint=dailyFun(dewpoint)
  
#winddir
wind=read.csv("ClimateData/alfCT_WDir.csv")
daily_winddir=dailyFun(wind)

#humidity
rel_hum=read.csv("ClimateData/alfRR_RH.csv")
daily_rh=dailyFun(rel_hum)

#sat vapor pressure (es)
satvp_min=vaporPress(daily_min_atemp2[,2])
satvp_max=vaporPress(daily_max_atemp2[,2])
satvp=(satvp_min+satvp_max)/2

#actual vapor pressure
actvp=vaporPress(daily_dewpoint[,2])
#slope vapor pressure curve
delta=4098*(satvp)/(daily_atemp2[,2]+237.3)^2

#MJ/kg latent heat of vaporization
lambda=2.45 

#gamma, psychrometric constant, kPa/deg C
gamma=0.000665*daily_baro

#radiation
ird=1+0.033*cos(2*pi/365*day_of_year)
d=0.409*sin(2*pi/365*day_of_year-1.39)
omegas=acos(-tan(38.5539)*tan(d))
latitude = 38.5539
Ra=24*60/pi*0.0820*ird*(omegas*sin(latitude)*sin(d)+cos(latitude)*cos(d)*sin(omegas))
Rs=(0.25+0.5*1)*Ra
Rso=(0.75+2*10^-5*60)*Ra
alpha=0 #albedo
Rns=(1-alpha)*Rs
Rnl=4.903*10^-9*((daily_atemp2[,2]+273.16)^4)*(0.34-0.14*sqrt(actvp))*((1.35*Rs/Rso)-0.35)
Rn=Rns-Rnl

ETo=(0.408*delta*Rn+(gamma*900/(daily_atemp2[,2]+273)*daily_windspeed[,2]*(satvp-actvp)))/(delta+(gamma*(1+0.34*daily_windspeed[,2])))

plot(day_of_year,ETo)

dailyFun = function(data, datecol=5,paramcol=4,FUNC=mean) {
  date <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 1)
  time <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 2)
  return(aggregate(data[,paramcol], by=list(date), FUN=FUNC))
}

vaporPress = function(T=20) {
  return(0.6108*exp((17.27*T)/(T+237.3)))
}
