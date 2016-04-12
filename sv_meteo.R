#sv_meteo.R
#Utilizes Hargreaves & Samani (1985) to calculate reference ET using Tmax and Tmin

library(lubridate)
mean=mean.default

#function definitions
dailyFun = function(data,datecol=4,paramcol=3,FUNC=mean) {
  date <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 1)
  time <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 2)
  return(aggregate(data[,paramcol], by=list(date), FUN=FUNC))
}

sv_meteo=read.csv("sv_meteo.csv")
#sv_meteo=read.csv("ClimateData/alfCT_Ta2m.csv")
#sv_meteo=sv_meteo[,c(5,4)]
a=0 #Site specific calibration coefficients (default a=0; b=1)
b=1
#MJ/kg latent heat of vaporization
lambda=2.45 
#radiation
day_of_year=as.numeric(levels(factor(yday(as.POSIXct(sv_meteo[,1],"%m/%d/%Y %H:%M",tz="GMT")))))
#day_of_year=as.numeric(levels(factor(yday(as.POSIXct(sv_meteo[,1],"%Y-%m-%d %H:%M:%S",tz="GMT")))))
ird=1+0.033*cos(2*pi/365*day_of_year)
d=0.409*sin(2*pi/365*day_of_year-1.39)
latitude = 41.499536#
omegas=acos(-tan(latitude*pi/180)*tan(d))
Ra=24*60/pi*0.0820*ird*(omegas*sin(latitude*pi/180)*sin(d)+cos(latitude*pi/180)*cos(d)*sin(omegas))

Tmax=dailyFun(sv_meteo,datecol=1,paramcol=2,FUNC=max)
Tmin=dailyFun(sv_meteo,datecol=1,paramcol=2,FUNC=min)
ETo=(1/lambda)*0.0023*(((Tmax[,2]-Tmin[,2])/2)+17.8)*sqrt(Tmax[,2]-Tmin[,2])*Ra
sum(ETo)
plot(ETo)
day_of_year
