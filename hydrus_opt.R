#hydrus-1d multiparameter optimization script
#MODELID = 5
source("ct_methods.R")
params_MIM=c("modelid","thetar","thetas","alpha","n","ksat","l","thrim","thsim","omega","pulse")
params_DUR=c("MODELID","THETAR","THETAS","ALPHA1","ALPHA2","N1","N2","KSAT","L","W2","PULSE")
params=params_MIM

dimension=11
simdir="C:\\Users\\agbrown\\workspace\\alfalfa_gb\\Simulations_MIM1\\"

nsim=21
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
  fit=readFitIn(1,s,simdir)
  node=readNodeFile(1,s,simdir)
  fp=array(getFitPoints(node[,1],fit[,1]))
  plot(fit[,2]) 
  est=matrix(nrow=65,ncol=(dimension^2))
  
  for(i in 1:(dimension^2)) {
    node=tryCatch(readNodeFile(i,s,simdir),error=function(e) { return(cbind(1:length(fit[,1]),rep(1000,10000))) })
    fp=array(getFitPoints(node[,1],fit[1:length(fit[,1]),1]))
    test1=((max(fp[which(fp<=length(est[,i]))]))==length(est[,i])) #makes sure the dimensions of the matrices line up by trimming fitpoints
	test2=!(length(fp) > length(node[,1]))
	if(test1 & test2) {
	#if(max(fp)==length(est[,i])) {
	  print(length(fp))
	  print(length(node[,1]))
      valz=node[1:length(fp),2]
      valz[is.na(valz)] = 1000
      noder=aggregate(array(valz)~fp,FUN=mean)
      est[,i]=noder[1:length(est[,i]),2]
      errz=c(errz,sum((noder[,2]-fit[,2])^2))
	  print(errz)
    } else {
		errz=c(errz,NA)
	}
  }
 
  sest=cbind(sest,est)
  serr=c(serr,errz)
}
serr=matrix(serr,ncol=nsim)
kekeke=cbind(mastermap, array(serr))
write.csv(sest,paste(simdir,"estimap.csv",sep=""))
write.csv(kekeke,paste(simdir,"errormap.csv",sep=""))
write.csv(axis_map,paste(simdir,"axismap.csv",sep=""))