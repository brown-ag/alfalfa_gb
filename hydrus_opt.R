#hydrus-1d multiparameter optimization script
#MODELID = 5
library(plot3D)
readNodeFile <- function(id,sim) {
  fn="OBS_NODE.out"
  fl="Node"
  return(readH1DFile(id,sim,fn,4,list(a="1.234",b="1.234",c="1.234",d="1.234"),12,1))
}

readFitIn <- function(id,sim) {
  fn="FIT.IN"
  return(readH1DFile(id,sim,fn,5,list(a="1.234",b="1.234",c="1.234",d="1.234",e="1.234"),16,3))
}
fee=""
#H1D FILE reader
readH1DFile <- function(id,sim,fname,cols,whatt,skipp,trimm) {
  obs_node_fname=paste("S:\\Andrew\\Code\\alfalfa_gb_git\\Simulations\\",sim,"\\",id,"\\",fname,sep="")
  #print(c(sim,id,fname))
  con=file(obs_node_fname, open="r")
  flag=FALSE
  iyx=suppressWarnings(scan(con,what=whatt,skip=skipp,fill=TRUE))
  close(con)
  mat=matrix(unlist(iyx),ncol=cols)
  mat=mat[1:(length(mat[,1])-trimm),]
  class(mat)="numeric"
  return(mat)
}

getFitPoints <- function(x,fit,int=15) {
  x=array(x)
  mmin=0
  mmax=0
  buf=c()
  for(i in 1:length(fit)) {
    mmax=int*i
    interval=(x[((x<=mmax) + (x>mmin))-1])
    buf=c(buf,rep(i,length(interval)))
    mmin=mmax
  }
  return(buf)
}
nsim=15
serr=c()
sest=list()
mastermap=c()
for(s in 1:nsim) {
  simpath=paste(".\\Simulations\\",s,"\\",sep="")
  errz=c()
  dimension=11
  map_fname=paste(simpath,"MAP.MAP",sep="")
  map=read.csv(map_fname)
  names(map)=c("id1","id2",params[2:length(params)])
  mastermap=rbind(mastermap,map)
  map_axes=(sapply(3:length(map[1,]),function(i) { if(sum(map[,i])!=(map[1,i]*length(map[,i]))) return(TRUE); return(FALSE) }))
  map_axes=which(map_axes==TRUE)+2
 #map_axes=c(7,10)
  fit=readFitIn(1,s)
  node=readNodeFile(1,s)
  fp=array(getFitPoints(node[,1],fit[,1]))
  plot(fit[,2])
  est=matrix(nrow=66,ncol=(dimension^2))
  
  for(i in 1:(dimension^2)) {
    node=readNodeFile(i,s)
    fp=array(getFitPoints(node[,1],fit[,1]))
    if(max(fp)<length(node[,2])) {
      noder=aggregate(array(node[1:length(fp),2])~fp,FUN=mean)
      length(fp)#print(max(node[,1]))
      length(node[,2])
      est[,i]=noder[,2]
      #lines(noder[,2],col=i)
      errz=c(errz,sum((noder[,2]-fit[,2])^2))      
    } else {
      length(node[,2])
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
makeLimit <- function(tplist) {
  xbar=tplist[1,1:length(tplist[1,])]
  inter=2*tplist[2,1:length(tplist[1,])]
  buf=lapply(1:length(xbar),function(i) return(c(xbar[i]-inter[i],xbar[i]+inter[i])))
  #shell.exec(paste("cp .\\LIMITS.IN .\\LIMITS",strftime(Sys.time(),format="%d%m%Y:%H%M%S"),".IN",sep=""))
  buf2=list(params,xbar)
  names(buf)=params
  write.csv(buf,"LIMITS.IN")
  write.csv(buf2,"CENTERS.IN")
}


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
ras=raster(nrows=11,ncols=11,vals=log10(err[,3]),xmn=min(err[,1]),xmx=max(err[,1]),ymn=min(err[,2]),ymx=max(err[,2]))
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
