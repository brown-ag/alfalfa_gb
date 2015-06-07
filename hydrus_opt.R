#hydrus-1d multiparameter optimization script
#MODELID = 5
source("ct_methods.R")
params_MIM=c("modelid","thetar","thetas","alpha","n","ksat","l","thrim","thsim","omega","pulse")
# params_DUR=c("MODELID","THETAR","THETAS","ALPHA1","ALPHA2","N1","N2","KSAT","L","W2"     ,"PULSE")
params=params_MIM

dimension=26
simdir="S:\\Andrew\\Code\\alfalfa_gb_git\\Simulations_MIM1"

library(plot3D)

nsim=1
serr=c()
sest=list()
mastermap=c()
axis_map=c()
for(s in 1:nsim) {
  simpath=paste(simdir,s,"\\",sep="")
  errz=c()
  map_fname=paste(simpath,"MAP.MAP",sep="")
  map=read.csv(map_fname)
  names(map)=c("id1","id2",params[2:length(params)])
  mastermap=rbind(mastermap,map)
  map_axes=(sapply(3:length(map[1,]),function(i) { if(sum(map[,i])!=(map[1,i]*length(map[,i]))) return(TRUE); return(FALSE) }))
  map_axes=which(map_axes==TRUE)+2
  
  axis_map=rbind(axis_map,map_axes)
 #map_axes=c(7,10)
  fit=readFitIn(1,s)
  node=readNodeFile(1,s)
  fp=array(getFitPoints(node[,1],fit[,1]))
  plot(fit[,2])
  est=matrix(nrow=65,ncol=(dimension^2))
  
  for(i in 1:(dimension^2)) {
    node=readNodeFile(i,s)
    fp=array(getFitPoints(node[,1],fit[,1]))
    if(max(fp)<length(node[,2])) {
      valz=node[1:length(fp),2]
      valz[is.na(valz)] = 1000
      noder=aggregate(array(valz)~fp,FUN=mean)
      est[,i]=noder[,2]
      errz=c(errz,sum((noder[,2]-fit[,2])^2))      
    } else {
      est[,i]=noder[,2]
      errz=c(errz,sum((noder[,2]-fit[,2])^2))
    }
  }
 
  sest=cbind(sest,est)
  serr=c(serr,errz)
}
serr=matrix(serr,ncol=nsim)
tp=which(serr<(1000)) #take top percentile of model runs
lala=matrix(unlist(lapply(2:length(names(mastermap)), function(i) { return(list(mean(mastermap[tp,i]), sd(mastermap[tp,i]))) })),nrow=2)
makeLimit(lala)

min(serr)
which(serr==min(serr))
lines(unlist(sest[,which(serr=<1000))
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
kekeke=cbind(mastermap, array(serr))
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
   localmin <- focal(ras,fun=f,pad=TRUE,padValue=NA,w=matrix(1,(dimension^2)+1,(dimension^2)+1))
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
write.csv(kekeke,"errormap.csv")

library(lattice)
contourplot
errz=c(errz,sum((noder-fit[,2])^2))
map=na.omit(map)
err=cbind(map[,map_axes],errz[2:((dimension^2)+1)])
xx=unique(err[,1])#err[order(err[,3])[1:11],1]
yy=unique(err[,2])#erer[order(err[,1])[1:11],2]
zz=matrix(err[,3],ncol=dimension)

plot3d(x=err[,1],y=err[,2],z=err[,3])
dis=dist(err[,1:4],method="euclidean")
tree=hclust(dis,method="ward")
clust=as.factor((cutree(tree,k=3)-2) %% 3 + 1)
plot(tree)
rect.hclust(tree,k=3,border="red")

library(soma)
par(mfrow=c(1,1))
err1=(t(aggregate(err[,3],by=list(err[,1]),FUN=matrix)))[2:(dimension+1),]
err2=(t(aggregate(err[,3],by=list(err[,2]),FUN=matrix)))[2:(dimension+1),]
matplot(err1,x=yy,type="l",xlab="Ksat",ylab="Objective",lty=floor(100*xx),col=floor(100*xx))
legend(x="topleft", as.character(xx),ncol=7,lty=floor(100*xx),col=floor(100*xx),cex=0.9)
matplot(err2,x=xx,type="l",xlab="w",ylab="Objective",lty=floor(100*yy),col=floor(100*yy))
legend(x="topleft", as.character(yy),ncol=7,lty=floor(100*yy),col=floor(100*yy),cex=0.9)
plot

flah=which(((mins<0) +(mins>-40))==2)
flah2=cbind(map[flah,c(9,11)],errz[flah])
which(errz==flah2[which(flah2[,3]==min(flah2[,3])),][,3])
gee=order(errz)[1:10]
plot(fit[,2],ylim=c(-250,0))
for(g in gee){
  lines(est[,g])
}
bmap=cbind(map[gee,],errz[gee])
cor(bmap[,9],bmap[,11])
contour(y=yy,x=xx,z=zz/mean(zz),nlevels=10000)
maa=matrix(xx,yy,zz/mean(zz))
library(misc3d)
library(plot3D)
library(rgl)

contour3d(x=xx,y=yy)
persp3d(xx,yy,z=zz-(mean(zz))/mean(zz))

library(raster)
ras=raster(nrows=26,ncols=26,vals=log10(tar[,13]),xmn=min(err[,1]),xmx=max(err[,1]),ymn=min(err[,2]),ymx=max(err[,2]))
filledContour(ras)
f <- function(X) min(X, na.rm=TRUE)
localmin <- focal(ras,fun=f,pad=TRUE,padValue=NA,w=matrix(1,121,121))
ras2=ras==localmin
minXY=xyFromCell(ras2,Which(ras2==1,cells=TRUE))
minx=matrix(minXY)
filledContour(ras,col=terrain.colors(log10(err[,3])*5))
filledContour(ras,col=terrain.colors(log10(err[,3])*5),plot.axes={axis(1);axis(2);points(minx[1,1],minx[3,1]);points(minx[2,1],minx[4,1                                                                                                                     ])})
contour(ras,col=terrain.colors(log10(err[,3])*5),nlevels=100,xlab=names(map[,map_axes])[1],ylab=names(map[,map_axes])[2])
points(minXY,pch=3,col=1)

plot(map[,10],map[,11])
points(bmap[,10],bmap[,11],pch="*",col="blue")
