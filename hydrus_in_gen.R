#hydrus-1d input generator
source("ct_methods.R")

template_set="vGM"
simdir=paste("C:\\Users\\agbrown\\workspace\\alfalfa_gb\\Simulations_",template_set,"3\\",sep="")
iteration=3
disc=25 #'disc' defines the discretization within the specified min and maximum


selector_fname=paste(".\\Templates\\",template_set,"\\SELECTOR_TEMPLATE.IN",sep="")
fit_fname=paste(".\\Templates\\",template_set,"\\FIT_TEMPLATE.IN",sep="")
atmo_fname=paste(".\\Templates\\",template_set,"\\ATMOSPH_TEMPLATE.IN",sep="")
prof_fname=paste(".\\Templates\\",template_set,"\\PROFILE_TEMPLATE.DAT",sep="")
H1D_fname=paste(".\\Templates\\",template_set,"\\HYDRUS1D_TEMPLATE.DAT",sep="")
LIMITS_fname=paste(".\\Templates\\",template_set,"\\LIMITS",iteration,".IN",sep="")
selector_template=readChar(selector_fname,file.info(selector_fname)$size)
fit_template=readChar(fit_fname,file.info(fit_fname)$size)
atmo_template=readChar(atmo_fname,file.info(atmo_fname)$size)
prof_template=readChar(prof_fname,file.info(prof_fname)$size)
h1d_template=readChar(H1D_fname,file.info(H1D_fname)$size)

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
# nanana=list(modelid=c(5,5),
#              thetar=c(0,0),
#              thetas=c(0.44,0.44),
#              alpha1=c(0.01,0.01),
#              alpha2=c(0.03,0.03),
#              n1=c(1.5,1.5),
#              n2=c(3,3),
#              ksat=c(0.03,0.03),
#              l=c(0.5,0.5),
#              w2=c(0.5,0.5),
#              pulse=c(16,16))
#mim-in
# nanana=list(modelid=c(6,6),
#             thetar=c(0,0),
#             thetas=c(0.44,0.44),
#             alpha=c(0.01,0.03),
#             n=c(1.1,2),
#             ksat=c(0.1,1),
#             l=c(0.1,1),
#             thrim=c(0,0.2),
#             thsim=c(0,0.4),
#             omega=c(0.1,1),
#             pulse=c(16,16))
#params=names(nanana)
#write.csv(limits,"LIMITS.IN")
limits=read.csv(LIMITS_fname)
limits=limits[,2:length(limits[1,])]
params=names(limits)
centers=(limits[2,1:length(limits[1,])]+limits[1,1:length(limits[1,])])/2
centers=rbind(centers,centers)
# names(centers)=params
# names(limits)=params
limits=do.call(rbind,limits)
centers=do.call(rbind,centers)
vparams=sapply(X = 1:length(limits[,1]), function(i) {limits[i,1]!=limits[i,2]})
simz=combn(rownames(limits[vparams,]),2)
nsim=(length(simz)/2)
print(simdir)
print(paste("Making initial parameter maps for",nsim,"simulations..."))
for(n in 1:nsim) {
  simpath=paste(simdir,n,sep="")
  if(!dir.exists(simpath)) {
    dir.create(simpath,recursive=TRUE)
  }
  quid=centers
  what=unlist(sapply(X = 1:length(vparams), function(i) { for(j in simz[,n]) if(names(vparams)[i]==j) return(TRUE); return(FALSE)}))
  quid[what,]=limits[what,]
  rownames(quid)=params
  write.csv(quid,paste(simpath,"\\INIT.IN",sep=""))
}

print(paste("Making HYDRUS input files for ",nsim," 2-variable (disc=",disc,") simulations...",sep=""))
for(n in 1:nsim) {
  simpath=paste(simdir,n,sep="")
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
  makeBatch(i,n,simpath,simdir)
  write.csv(gridd,map_fname)
  print(paste("     Simulation #",n," input created",sep=""))
}
makeMasterBatch(nsim,simdir)
print("DONE!")