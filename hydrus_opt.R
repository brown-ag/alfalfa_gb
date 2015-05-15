#hydrus-1d multiparameter optimization script
#MODELID = 5
library(plot3D)
readNodeFile <- function(id) {
  fn="OBS_NODE.out"
  fl="Node"
  return(readH1DFile(id,fn,fl,9,4))
}

readFitIn <- function(id) {
  fn="FIT.IN"
  fl="FOS"
  return(readH1DFile(id,fn,fl,7,2))
}

#H1D FILE reader
readH1DFile <- function(id,fname,strFlag,cols,start) {
  obs_node_fname=paste("S:\\Andrew\\Code\\alfalfa_gb_git\\Simulations\\",id,"\\",fname,sep="")
  con=file(obs_node_fname, open="r")
  buf=matrix(ncol=cols)
  flag=FALSE
  while (length(li <- readLines(con, n=1,warn=FALSE))>0) {
   if(flag) {
     baz=gsub("\\s+",",",li,perl=TRUE)
     qux=as.numeric(unlist(strsplit(baz[2:length(baz)],split=",")))
     #print(length(qux))
     if(length(qux)==cols)
       buf=rbind(buf,qux)
   }
   if(grepl(strFlag,li,fixed=TRUE)) {
     flag=TRUE
   }
  }
  close(con)
  return(buf[start:length(buf[,3]),3:cols])
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
errz=c()
map_fname=".\\MAP.MAP"
map=read.csv(map_fname)
fit=readFitIn(1)
node=readNodeFile(1)
fp=array(getFitPoints(node[,1],fit[,1]))
for(i in 1:121) {
  node=readNodeFile(i)
  fp=array(getFitPoints(node[,1],fit[,1]))
  noder=aggregate(array(node[1:length(fp),2])~fp,FUN=mean)
  errz=c(errz,sum((noder-fit[,2])^2))
}

sseer <- function(i,fitp) {
  node=readNodeFile(i)
  noder=aggregate(array(node[1:length(fitp),2])~fitp,FUN=mean)
  errz=c(errz,sum((noder-fitp[,2])^2))
}
node=readNodeFile(1)
fp=array(getFitPoints(node[,1],fit[,1]))
noder=aggregate(array(node[1:length(fp),2])~fp,FUN=mean)
errz=c(errz,sum((noder-fit[,2])^2))
map=na.omit(map)
err=cbind(map[,8],map[,11],errz)
scatter2D(z=err[,3],y=err[,2],x=err[,1])
