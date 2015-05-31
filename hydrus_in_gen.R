#hydrus-1d input generator

selector_fname=".\\Templates\\SELECTOR_TEMPLATE.IN"
fit_fname=".\\Templates\\FIT_TEMPLATE.IN"
atmo_fname=".\\Templates\\ATMOSPH_TEMPLATE.IN"
prof_fname=".\\Templates\\PROFILE_TEMPLATE.DAT"
H1D_fname=".\\Templates\\HYDRUS1D_TEMPLATE.DAT"
selector_template=readChar(selector_fname,file.info(selector_fname)$size)
fit_template=readChar(fit_fname,file.info(fit_fname)$size)
atmo_template=readChar(atmo_fname,file.info(atmo_fname)$size)
prof_template=readChar(prof_fname,file.info(prof_fname)$size)
h1d_template=readChar(H1D_fname,file.info(H1D_fname)$size)

disc=10 #'disc' defines the discretization within the specified min and maximum

makeSelectorFit <- function(n,pval,params,outpath) {
  buffer=c(selector_template,fit_template,atmo_template,prof_template,h1d_template)
  fnames=c("SELECTOR.IN","FIT.IN","ATMOSPH.IN","PROFILE.DAT","HYDRUS1D.DAT")
  values=c(pval)
  for(p in 1:length(params)) {
    for(qq in 1:length(buffer))
      buffer[qq]=gsub(paste("%",toupper(params[p]),"%",sep=""),values[p],buffer[qq])
  }
  for(qq in 1:length(buffer)) {
    writeChar(buffer[qq],paste(outpath,"\\",n,"\\",fnames[qq],sep=""))
  }
}

makeMasterBatch <- function(nsim) {
  buffer="@echo off\n"
  for(kk in 1:(nsim)) {
    buffer=paste(buffer,"cd .\\",kk,"\n",sep="")  
    buffer=paste(buffer,"call sim.bat\n", sep="")  
    #buffer=paste(buffer,"cd ..\n", sep="")  
  }
  write(buffer,paste("S:\\Andrew\\Code\\alfalfa_gb_git\\Simulations\\master.bat",sep=""))
}

makeBatch <- function(last_index,n,outpath) {
  buffer="@echo off\n" 
  wd=gsub("/","\\",getwd(),fixed=TRUE)
  for(kk in 1:(last_index)) {
    buffer=paste(buffer,"@echo ", wd,"\\Simulations\\",n,"\\",kk," > C:\\hydrus1d\\Level_01.dir\nC:\ncd C:\\hydrus1d\\\nH1D_clci<return.txt\nS:\ncd ",wd,"\\Simulations\n", sep="") 
    #@echo S:\Andrew\Code\alfalfa_gb_git\Simulations\2\1 > "C:\hydrus1d\Level_01.dir"
    #C:
    #cd C:\hydrus1d\
    #H1D_clci<return.txt
    #buffer=paste(buffer,"\"C:\\hydrus1d\\H1D_clci.exe\" .\\",kk,"\n", sep="")  
  }                   
  write(buffer,paste(outpath,"\\sim.bat",sep=""))
}

getSeq <- function(range, disc){
  return(seq(range[1],range[2],(range[2]-range[1])/disc))
}

getParams <- function(qq) {
  glib=(gsub("range_","",rownames(qq)))
  return(glib)
}

makeGrid <- function(qq,d) {
  gnames=gsub("range_","d_",rownames(qq))
  buf=list()
  for(p in 1:length(rownames(qq))) {
    lux=qq[p,2:3]
    buf[[p]]=getSeq(as.matrix(lux),d)
  }
  return(expand.grid(buf))
}

# limits=list(modelid=c(5,5),
#          thetar=c(0,0),
#          thetas=c(0.44,0.44),
#          alpha1=c(0.005,0.03),
#          alpha2=c(0.01,0.05),
#          n1=c(1.1,3),
#          n2=c(2,4),
#          ksat=c(0.01,0.5),
#          l=c(0.5,0.5),
#          w2=c(0.01,1),
#          pulse=c(16,16))
nanana=list(modelid=c(5,5),
             thetar=c(0,0),
             thetas=c(0.44,0.44),
             alpha1=c(0.01,0.01),
             alpha2=c(0.03,0.03),
             n1=c(1.5,1.5),
             n2=c(3,3),
             ksat=c(0.03,0.03),
             l=c(0.5,0.5),
             w2=c(0.5,0.5),
             pulse=c(16,16))
params=names(nanana)
#write.csv(limits,"LIMITS.IN")
limits=read.csv("LIMITS.IN")
limits=limits[,2:length(limits[1,])]
centers=read.csv("CENTERS.IN")
centers= cbind(centers[,3],centers[,3])
names(centers)=params
names(limits)=params
limits=do.call(rbind,limits)
#centers=do.call(rbind,centers)
vparams=sapply(X = 1:length(limits[,1]), function(i) {limits[i,1]!=limits[i,2]})
simz=combn(rownames(limits[vparams,]),2)
nsim=(length(simz)/2)

for(n in 1:nsim) {
  simpath=paste(".\\Simulations\\",n,sep="")
  if(!dir.exists(simpath)) {
    dir.create(simpath,recursive=TRUE)
  }
  quid=centers
  what=unlist(sapply(X = 1:length(vparams), function(i) { for(j in simz[,n]) if(names(vparams)[i]==j) return(TRUE); return(FALSE)}))
  quid[what,]=limits[what,]
  rownames(quid)=params
  write.csv(quid,paste(simpath,"\\INIT.IN",sep=""))
}

print(paste("Making input files for ",nsim," 2-variable (disc=",disc,") simulations...",sep=""))
for(n in 1:nsim) {
  simpath=paste(".\\Simulations\\",n,sep="")
  map_fname=paste(simpath,"\\MAP.MAP",sep="")
  init_fname=paste(simpath,"\\INIT.IN",sep="")
  
  quid=read.csv(init_fname)
  #params=getParams(quid)
  gridd=makeGrid(quid,disc)
  
  for(i in 1:length(gridd[,1])) {
    dd=paste(simpath,"\\",i,sep="")
    if(!dir.exists(dd)) {
      dir.create(dd,recursive=TRUE)
    }
    makeSelectorFit(i,gridd[i,1:length(params)],params,simpath)
  }
  makeBatch(i,n,simpath)
  write.csv(gridd,map_fname)
  print(paste("     Simulation #",n," input created",sep=""))
}
makeMasterBatch(nsim)
print("DONE!")