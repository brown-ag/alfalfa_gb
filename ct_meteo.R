#Daily meteorological values
library(lubridate)
mean=mean.default

#function definitions
dailyFun = function(data,datecol=4,paramcol=3,FUNC=mean) {
  date <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 1)
  time <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 2)
  return(aggregate(data[,paramcol], by=list(date), FUN=FUNC))
}


monthlyFun = function(data,datecol=4,paramcol=3,FUNC=mean) {
  daily=dailyFun(data,datecol,paramcol,FUNC=max)  
  date <- sapply(strsplit(as.character(daily[,1]), "-"),"[",2)
  return(aggregate(daily[,2], by=list(date), FUN=FUNC))
}


vaporPress = function(T=20) {
  return(0.6108*exp((17.27*T)/(T+237.3)))
}

target_year=2014
origin_time=yday("2015-01-01")
end_time=yday("2015-12-31")

#barometric pressure
baro=read.csv("ClimateData3\\CT_Baro.csv")
baro=baro[year(baro[,4]) == target_year,]
daily_baro=dailyFun(baro)[origin_time:end_time,]
day_of_year=yday(daily_baro[,1])[origin_time:end_time]
daily_baro=daily_baro[,2]*3.386 #kPa

rainfall=read.csv("ClimateData3\\CT_Rain_Tot24.csv")
rainfall2015=rainfall[year(rainfall[,4]) == 2015,]
rainfall2014=rainfall[year(rainfall[,4]) == 2014,]
daily_rainfall=dailyFun(rainfall,datecol=4,paramcol=3)[origin_time:end_time,2]
monthly_rainfall15=monthlyFun(rainfall2015,datecol=4,paramcol=3,FUNC=sum)
monthly_rainfall15[,2]=monthly_rainfall15[,2]
monthly_rainfall14=monthlyFun(rainfall2014,datecol=4,paramcol=3,FUNC=sum)
monthly_rainfall14[,2]=monthly_rainfall14[,2]

monthly_rainfall14[,2]
monthly_rainfall15[,2]

rainfall=read.csv("ClimateData3\\CT_Rain_mm.csv")
monthlyrainfall=data.frame(year=c(rep(2014,12),rep(2015,12)),month=c(monthly_rainfall14[,1],monthly_rainfall15[,1]),rain=c(monthly_rainfall14[,2],monthly_rainfall15[,2])/10*2.54)
aggregate(monthlyrainfall$rain,by=list(monthlyrainfall$year),FUN=sum)


#wind speed
mean_windspeed=read.csv("ClimateData3\\CT_WS2mMn.csv")
max_windspeed=read.csv("ClimateData3\\CT_WS2mMx.csv")
mean_windspeed=mean_windspeed[year(mean_windspeed[,4]) == target_year,]
daily_windspeed=dailyFun(mean_windspeed,datecol=4,paramcol=3)[origin_time:end_time,2]

#air temperature
atemp_2m=read.csv("ClimateData3\\CT_Ta2m.csv")
atemp_2m=atemp_2m[year(atemp_2m[,4])==target_year,]
atemp_10m=read.csv("ClimateData3\\CT_Ta10m.csv")
daily_max_atemp2=dailyFun(atemp_2m,FUNC="max")[origin_time:end_time,2]
daily_min_atemp2=dailyFun(atemp_2m,FUNC="min")[origin_time:end_time,2]
daily_atemp2=dailyFun(atemp_2m,datecol=4,paramcol=3)[origin_time:end_time,2]

#dewpoint
dewpoint=read.csv("ClimateData3\\CT_DewPt.csv")
dewpoint=dewpoint[year(dewpoint[,4]) == target_year,]
daily_dewpoint=dailyFun(dewpoint)[origin_time:end_time,2]
  
#winddir
wind=read.csv("ClimateData3\\CT_WDir.csv")
wind=wind[year(wind[,4]) == target_year,]
daily_winddir=dailyFun(wind)[origin_time:end_time,2]

#humidity
rel_hum=read.csv("ClimateData3\\CT_RH.csv")
rel_hum=rel_hum[year(rel_hum[,4]) == target_year,]
daily_rh=dailyFun(rel_hum)[origin_time:end_time,2]

#sat vapor pressure (es)
satvp_min=vaporPress(daily_min_atemp2)
satvp_max=vaporPress(daily_max_atemp2)
satvp=(satvp_min+satvp_max)/2

#actual vapor pressure
actvp=vaporPress(daily_dewpoint)
#slope vapor pressure curve
delta=4098*(satvp)/(daily_atemp2+237.3)^2

#MJ/kg latent heat of vaporization
lambda=2.45 

#gamma, psychrometric constant, kPa/deg C
gamma=0.000665*daily_baro

#radiation
ird=1+0.033*cos(2*pi/365*day_of_year)
d=0.409*sin(2*pi/365*day_of_year-1.39)
latitude = 38.5539
omegas=acos(-tan(latitude*pi/180)*tan(d))

Ra=24*60/pi*0.0820*ird*(omegas*sin(latitude*pi/180)*sin(d)+cos(latitude*pi/180)*cos(d)*sin(omegas))
Rs=(0.25+0.5*1)*Ra
Rso=(0.75+2*10^-5*60)*Ra
alpha=0.3 #albedo
Rns=(1-alpha)*Rs
Rnl=4.903*10^-9*((daily_atemp2+273.16)^4)*(0.34-0.14*sqrt(actvp))*((1.35*Rs/Rso)-0.35)
Rn=Rns-Rnl

ETo=(0.408*delta*Rn+(gamma*900/(daily_atemp2+273)*daily_windspeed*(satvp-actvp)))/(delta+(gamma*(1+0.34*daily_windspeed)))

plot(day_of_year,daily_rainfall,type="l")
points(day_of_year,ETo)

df_eto=data.frame(doy=day_of_year,eto=ETo,rain=daily_rainfall)
write.csv(file=paste("CT_ET_",target_year,".csv",sep=""),df_eto)
sum(df_eto$eto)
#monthly
months=as.numeric(format(strptime(day_of_year, format="%j"), format="%m"))
m_rain=aggregate(daily_rainfall,by=list(months),FUN=mean)
m_et=aggregate(ETo,by=list(months),FUN=mean)

m_rain
m_et

target_year
