#Water balance plots

y15=read.csv("CT_ET_2015.csv")
y14=read.csv("CT_ET_2014.csv") 
y13=read.csv("CT_ET_2013.csv")
y12=read.csv("CT_ET_2012.csv")

day_of_year=1:365
yr=c(rep(2014,365),rep(2015,365))
yet=c(y15$eto)
yra=c(y15$rain)
months=as.numeric(format(strptime(day_of_year, format="%j"), format="%m"))

df_month=na.omit(data.frame(months,yet,yra))

month_len=aggregate(rep(1,length(months)),by=list(months),FUN=sum)

monthly_et=aggregate(df_month$yet,by=list(df_month$months),FUN=mean)
monthly_rain=aggregate(df_month$yra,by=list(df_month$months),FUN=mean)
(prod=data.frame(mon=1:12,len=month_len[,2],eto=monthly_et[,2],rain=monthly_rain[,2]))
prod=transform(prod,etol=len*eto,rainl=len*rain)

plot(day_of_year,y15$rain,col="blue",lwd=2,type="l", axes=FALSE,xlim=c(0,365))
lines(y15$eto)
axis(1)
axis(2, las=1)

plot(day_of_year,cumsum(y15$rain-y15$eto),col="blue",lwd=2,type="l", axes=FALSE,xlim=c(0,365))
#lines(cumsum(y15$eto))
axis(1)
axis(2, las=1)

sum(y15$eto)

sum(y15$rain[day_of_year<80])
