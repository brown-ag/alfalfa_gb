#sv_waterbalance
library(lubridate)
#x11()
setwd("C:\\Users\\agbrown\\workspace\\alfalfa_gb\\")
svwb=read.csv("scottvalley\\sv_control_volume.csv")
svwb=na.omit(svwb) #remove NA
#plot((1:length(svwb$Date)),svwb[,3],type="l")
#locator(1)

plotareas=c(0.8,1.1,1.2,1.2,1.3,1.4,1.4,1.5,1.5,1.9,1.5,1.4)

tymes=as.POSIXct(strptime(svwb[,1],format="%m/%d/%Y %H:%M:%S"))
timeFun=function(times, values, interval, fun) {
	#intervals: second", "minute", "hour", "day", "week", "month","year", "quarter"
	aggr=round_date(tymes,interval)
	return(aggregate(values, by=list(aggr), FUN=fun))
}
dailyflows=numeric(54)
for(nn in 2:12) {	
	daysum=timeFun(tymes,svwb[,2],interval='day',fun=sum)
	dailyflows=cbind(dailyflows,daysum$x)
	if(nn == 2) {
		dailyflows[,1]=daysum[,1]
	}
}
colnames(dailyflows)=colnames(svwb)

on=which(daysum$x > 0)
#plot(daysum)
print(length(on))
cumsum(daysum$x)

tymes[length(tymes)]-tymes[1]

treatsums=apply(svwb[,2:12],MARGIN=2,FUN=sum)
treatmeans=c(mean(treatsums[1:3]),mean(treatsums[4:6]),mean(treatsums[7:9]))

totalwater=numeric(11)
for(ii in 1:11) {
	totalwater[ii]=treatsums[ii]*plotareas[ii]
}
totalwater
sum(totalwater)
	