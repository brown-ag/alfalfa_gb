library(raster)
library(plot3D)
library(rgl)
source("ct_methods.R")
dimension=11
simdir="C:\\Users\\agbrown\\workspace\\alfalfa_gb\\Simulations_Durner1\\"
nsim=15
kekeke=read.csv(paste(simdir,"errormap.csv",sep=""))
sest=read.csv(paste(simdir,"estimap.csv",sep=""))
mastermap=kekeke[,1:(length(names(kekeke))-1)]
serr=kekeke[,length(names(kekeke))]
tp=which(serr<(1000)) #take top percentile of model runs
lala=matrix(unlist(lapply(2:length(names(mastermap)), function(i) { return(list(mean(mastermap[tp,i]), sd(mastermap[tp,i]))) })),nrow=2)
#makeLimit(lala)

fit=readFitIn(1,1,simdir)
min(serr)
which(serr==min(serr))
lines(unlist(sest[,which(serr<=1000)]))
plot(fit[,2],ylim=c(-250,0))
eeek=which(serr<10000)
for(e in eeek) {
  lines((1:length(fit[,2])),unlist(sest[1:length(sest[,e]),e]))
}
# par(mfrow=c(3,5))
require(gridExtra)
require(raster)
plotz=c()
last=1
cur=dimension^2
library(plot3D)
nom=params[2:length(params)]
par(mfrow=c(1,1))
colorz=rainbow(dimension)

for(ff in 1:ncol(serr)) {
  axes=axis_map[ff,]
  targ=kekeke[last:cur,]
  #print(contourplot(targ[,13]~targ[,axes[1]]*targ[,axes[2]],xlab=nom[axes[1]-2],ylab=nom[axes[2]-2]))
  serr2=serr
  serr2[serr2>20000]=20000
  ras=raster(nrows=dimension,ncols=dimension,vals=matrix(serr2[last:cur]),xmn=min(mastermap[last:cur,axis_map[ff,1]]),xmx=max(mastermap[last:cur,axis_map[ff,1]]),ymn=min(mastermap[last:cur,axis_map[ff,2]]),ymx=max(mastermap[last:cur,axis_map[ff,2]]))
#vals=unlist(targ[,13]),xmn=min(targ[,axes[1]]),xmx=max(targ[,axes[1]]),ymn=min(targ[,axes[2]]),ymx=max(targ[,axes[2]]))
   #filled.contour(x=targ[,axes[1]],y=targ[,axes[2]],z=targ[,13])
   f <- function(X) min(X, na.rm=TRUE)

   rf=focal(ras,w=matrix(1,3,3))
   iseven=!((dimension^2)%%2)
   localmin <- focal(ras,fun=f,pad=TRUE,padValue=NA,w=matrix(1,(dimension^2)+iseven,(dimension^2)+iseven))
   ras2=ras==localmin
   minXY=xyFromCell(ras2,Which(ras2==1,cells=TRUE))
   minx=matrix(minXY)
   #filledContour(ras,col=terrain.colors(log10(err[,3])*5))
   filledContour(ras,col=colorz,plot.axes={axis(1);axis(2);points(minx[1,1],minx[2,1]);},xlab=nom[axes[1]-2],ylab=nom[axes[2]-2])
   #contour(ras,col=terrain.colors(log10(matrix(serr[last:cur]))),nlevels=100)#,nlevels=100,xlab=(names(map)[,axes[1]]),ylab=(names(map)[,axes[1]]))
#   points(minXY,pch=3,col=1)
  last=cur+1
  cur=cur+(dimension^2)
}
