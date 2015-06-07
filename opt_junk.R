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