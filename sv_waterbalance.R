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
	daysum=timeFun(tymes,svwb[,nn],interval='day',fun=sum)
	dailyflows=cbind(dailyflows,daysum$x)
	if(nn == 2) {
		dailyflows[,1]=daysum[,1]
	}
}
colnames(dailyflows)=colnames(svwb)

on=which(daysum$x > 0)
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

#ET data
svet=read.csv("SV_ET_2015.csv")
svpr=read.csv("SV_PR_2015.csv")
svpr
for(ch in 2:length(colnames(dailyflows))) {
  #irrigation data
  irrigday=as.numeric(levels(factor(yday(tymes))))
  irrigqty=dailyflows[,ch]*30.48 #cm of water
  
  #matrix
  lastday=svet$day_of_year[length(svet$day_of_year)]
  watmat=data.frame(day=1:lastday,et=rep(0,lastday),precip=rep(0,lastday),irrig=rep(0,lastday),aw=rep(0,lastday),rech=rep(0,lastday))
  watmat$et[svet$day_of_year] = svet$et
  watmat$irrig[irrigday]=irrigqty
  watmat$precip = svpr$PrecipitationSumIn[1:svet$day_of_year[length(svet$day_of_year)]]*2.54
  #assume no runoff
  
  #soil storage
    #assumed AWC (SSURGO)
    awc=12.46 #cm in 150cm
    #initial available water
    aw0=0
  
  for(d in svet$day_of_year) {
    watmat$aw[d] = watmat$aw[d-1] + watmat$precip[d] + watmat$irrig[d] - watmat$et[d]/10 #inputs (cm of water)
    if(watmat$aw[d] > awc) { #remove excess
      excess=watmat$aw[d]-awc
      watmat$aw[d]=awc
      watmat$rech[d] = watmat$rech[d]+excess
    }
  }
  
  print(sum(watmat$rech)/sum(watmat$irrig+watmat$precip))
  
  write.csv(watmat,file=paste("SV_check_",colnames(dailyflows)[ch],"_balance.csv",sep=""))
}
sum(diff(watmat$aw))

bar_w = 0.5 # width of bars
offset = c(0,0,0,0) # offset to avoid overlapping
cols = grey.colors(4) # colors for different types

watmat2=watmat[0,]
checks=c("C1","H1","L1","N1")
for(c in 1:length(checks)) {#colnames(dailyflows)[ch]) {
  fr=read.csv(paste("SV_check_",checks[c],"_balance.csv",sep=""))
  watmat2=rbind(watmat2,cbind(fr,rep(c,nrow(fr))))
}
names(watmat2)=c("X", "day", "et", "precip", "irrig", "aw", "rech", "type")
library(plotrix)
# set up empty plot with sensible x and y lims
par(mfrow=c(4,1))
for (i in unique(watmat2$type)){
    dd = watmat2[watmat2$type==i, ]
    plot(dd$day, dd$irrig, type='n',ylim=c(0,100),xlim=c(0,125))
    #axis.break(axis=2, breakpos = 50, brw = 0.02,  style = "zigzag")
    x = dd$X
    y = dd$irrig
    #plot(watmat2$irrig)
    # rectangles
    print(x-bar_w)
    rect(xleft=x-bar_w+offset[i], ybottom=0, xright=x+bar_w+offset[i], ytop=y, col=cols[i])  
    #lines(dd$precip,col="blue")
    #lines(dd$et,col="red")
    lines(dd$aw,col="green")
    #lines(dd$rech,col="yellow")
    # errors bars
    # arrows(x0=x+offset[i], y0=y-0.5*dd$SE, x1=x+offset[i], y1=y+0.5*dd$SE, col=1, angle=90, code=3, length = 0.1)
}

watmat3=watmat[0,]
checks=colnames(dailyflows)[2:12]
for(c in 1:length(checks)) {
  fr=read.csv(paste("SV_check_",checks[c],"_balance.csv",sep=""))
  watmat3=rbind(watmat3,cbind(fr,rep(c,nrow(fr))))
}
names(watmat3)=c("X", "day", "et", "precip", "irrig", "aw", "rech", "type")
summary(watmat3)

#contmat=watmat3[which(watmat3$type==1 | watmat3$type==2 | watmat3$type==3),]
contmat=aggregate(watmat3,by=list(watmat3$type),FUN=sum)
contmat=cbind(check=contmat$Group.1,et=contmat$et/10,precip=contmat$precip,irrig=contmat$irrig)
contmat=cbind(treat=c(rep("C",3),rep("H",3),rep("L",3),rep("N",2)),transform(contmat,rech=precip+irrig-et))

contmat2=aggregate(contmat[,3:6],by=list(contmat$treat),FUN=mean)
contmat2
contmat2=transform(contmat2,refrac=rech/(irrig+precip))
write.csv(contmat2,"sv_balance_avg.csv")

plot(which(watmat3$irrig>0))
