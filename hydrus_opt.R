#hydrus-1d multiparameter optimization script
#MODELID = 5
library(plot3d)
errz=c()
for(i in 1:36) {
  node=readNodeFile(i)
  fit=readFitIn(i)
  errz=c(errz,sqrt(mean((node[,2]-fit[,2])^2)))
}
err=matrix(errz,nrow=6,ncol=36)
contour(err[,1:6])


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
