#Daily meteorological values
library(lubridate)
day_of_year=yday(daily_baro[,1])
#barometric pressure
baro=read.csv("ClimateData/alfRR_Baro.csv")
daily_baro=dailyFun(baro)*3.386 #kPa

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
gamma=0.000665*daily_baro[,2] 

#radiation
ird=1+0.033*cos(2*pi/365)
Ra=24*60/pi*0.0820

dailyFun = function(data, datecol=5,paramcol=4,FUNC=mean) {
  date <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 1)
  time <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 2)
  return(aggregate(data[,paramcol], by=list(date), FUN=FUNC))
}

vaporPress = function(T=20) {
  return(0.6108*exp((17.27*T)/(T+237.3)))
}
