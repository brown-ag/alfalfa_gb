library(raster)
library(plot3D)
library(rgl)
library(gridExtra)

source("ct_methods.R")
dimension=11
simdir=paste("C:\\Users\\agbrown\\workspace\\alfalfa_gb\\",c("Simulations_vGM1\\","Simulations_Durner1\\","Simulations_MIM1\\"),sep="")
params_MIM=c("modelid","thetar","thetas","alpha","n","ksat","l","thrim","thsim","omega","pulse")
params_DUR=c("MODELID","THETAR","THETAS","ALPHA1","ALPHA2","N1","N2","KSAT","L","W2","PULSE")
params=rbind(params_DUR,params_DUR,params_MIM)
nsims=c(15,15,21)

for(i in 1:length(nsims)) {
	kekeke=read.csv(paste(simdir[i],"errormap.csv",sep=""))
	sest=read.csv(paste(simdir[i],"estimap.csv",sep=""))
	axis_map=read.csv(paste(simdir[i],"axismap.csv",sep=""))
	axis_map=matrix(unlist(axis_map[,2:3]),nrow=nsims[i])
	mastermap=kekeke[,1:(length(names(kekeke))-1)]
	serr=kekeke[,length(names(kekeke))]
	serr=matrix(serr,ncol=nsims[i])
	fit=readFitIn(1,1,simdir[i])
	serr[is.na(serr)]=20000
	serr[which(serr>20000)]=20000
	print(paste("Lowest SSQ:",min(serr)))
	print(paste("Model ID lowest SSQ:",which(serr==min(serr))))
	plot(fit[,2],ylim=c(-250,0))
	ran=range(serr)
	lines(unlist(sest[,which(serr<=0.05*(ran[2]-ran[1])]))
	#eeek=which(serr<10000)
	#for(e in eeek) {
	#  lines((1:length(fit[,2])),unlist(sest[1:length(sest[,e]),e]))
	#}

	plotz=c()
	last=1
	cur=dimension^2
	library(plot3D)
	nom=params[i,]
	par(mfrow=c(1,1))
	colorz=rainbow(dimension)

	for(ff in 1:nsims[i]) {
	  axes=axis_map[ff,]
	  targ=kekeke[last:cur,]
	  xmin=min(mastermap[last:cur,axis_map[ff,1]+1])
	  xmax=max(mastermap[last:cur,axis_map[ff,1]+1])
	  ymin=min(mastermap[last:cur,axis_map[ff,2]+1])
	  ymax=max(mastermap[last:cur,axis_map[ff,2]+1])
	  print(paste("Parameters:", names(mastermap)[axis_map[ff,1]+1],"&",names(mastermap)[axis_map[ff,2]+1], "--","Range:",paste("[",xmin,",",xmax,"] & [",ymin,",",ymax,"]",sep="")))
	  #print(contourplot(targ[,13]~targ[,axes[1]]*targ[,axes[2]],xlab=nom[axes[1]-2],ylab=nom[axes[2]-2]))
	  ras=raster(nrows=dimension,ncols=dimension,vals=matrix(serr[last:cur]),xmn=xmin,xmx=xmax,ymn=ymin,ymx=ymax)
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
}