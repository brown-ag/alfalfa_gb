#hydrus-1d input generator
selector_fname=".\\Templates\\SELECTOR_TEMPLATE.IN"
fit_fname=".\\Templates\\FIT_TEMPLATE.IN"
atmo_fname=".\\Templates\\ATMOSPH_TEMPLATE.IN"
map_fname=".\\MAP.MAP"
selector_template=readChar(selector_fname,file.info(selector_fname)$size)
fit_template=readChar(fit_fname,file.info(fit_fname)$size)
atmo_template=readChar(atmo_fname,file.info(atmo_fname)$size)
params=t(c("MODELID", "THETAR","THETAS","ALPHA1","ALPHA2","N1","N2","KSAT","L","W2"))
bcs=t(c("PULSE"))
mat_selector=matrix(params)
mat_atmo=matrix(bcs)

#Simulation directory path
simpath="Simulations"

#Define the range of parameters to use for inital guesses.
#Setting min and max equal holds that parameter constant
disc=10 #'disc' defines the discretization within the specified min and maximum
range_thetar=c(0,0)#c(0,0.2)
range_thetas=c(0.44,0.44)#c(0.3,0.5)
range_alpha1=c(0.01,0.01)#c(0,0.1)
range_alpha2=c(0.026,0.026)
range_n1=c(4,6)#c(1,4)
range_n2=c(3,3)#c(1,4)
range_ksat=c(0.02,0.02)#c(0.01, 1)
range_l=c(0.01,2)#c(0,1)
range_w2=c(0,0)
range_pulse=c(16,16)

makeSelectorFit <- function(n,id,pval,bval) {
  buffer=selector_template
  buffer2=fit_template
  buffer3=atmo_template
  values=c(id,pval)
  avalues=c(bval)
  for(p in 1:length(params)) {
    buffer=gsub(paste("%",params[p],"%",sep=""),values[p],buffer)
    buffer2=gsub(paste("%",params[p],"%",sep=""),values[p],buffer2)
  }
  for(av in 1:length(bcs)) {
    buffer3=gsub(paste("%",bcs[av],"%",sep=""),avalues[av],buffer3)
  }
  writeChar(buffer,paste(".\\Simulations\\",n,"\\SELECTOR.IN",sep=""))
  writeChar(buffer2,paste(".\\Simulations\\",n,"\\FIT.IN",sep=""))
  writeChar(buffer3,paste(".\\Simulations\\",n,"\\ATMOSPH.IN",sep=""))
}

makeBatch <- function(last_index) {
  buffer="@echo off\n"
  for(kk in 1:(last_index-1)) {
    buffer=paste(buffer,"cp ","PROFILE.DAT ", kk, "\n", sep="")
    buffer=paste(buffer,"cp ","HYDRUS1D.DAT ", kk, "\n", sep="")
    #buffer=paste(buffer,"cp ","ATMOSPH.IN Simulations\\", kk, "\n", sep="")
    buffer=paste(buffer,"\"C:\\hydrus1d\\H1D_calc.exe\" S:\\Andrew\\Code\\alfalfa_gb_git\\Simulations\\",kk,"\n", sep="")  
  }
  write(buffer,".\\Simulations\\sim.bat")
}

getSeq <- function(range, disc){
  return(seq(range[1],range[2],(range[2]-range[1])/disc))
}

d_thetar=getSeq(range_thetar,disc)
d_thetas=getSeq(range_thetas,disc)
d_alpha1=getSeq(range_alpha1,disc)
d_alpha2=getSeq(range_alpha2,disc)
d_n1=getSeq(range_n1,disc)
d_n2=getSeq(range_n2,disc)
d_ksat=getSeq(range_ksat,disc)
d_l=getSeq(range_l,disc)
d_w2=getSeq(range_w2,disc)
d_pulse=getSeq(range_pulse,disc)
map=matrix(ncol=11)

gridd=expand.grid(d_thetar,d_thetas,d_alpha1,d_alpha2,d_n1,d_n2,d_ksat,d_l,d_w2,d_pulse)
for(i in 1:length(gridd[,1])) {
  dd=paste(".\\Simulations\\",i,sep="")
  if(!dir.exists(dd)) {
    dir.create(dd)
  }
  makeSelectorFit(i,5,gridd[i,1:9],gridd[,10])
  map=rbind(map,c(i,gridd[i,1:9],gridd[,10]))
}
makeBatch(i)
write.csv(map,map_fname)
shell('S:\\Andrew\\Code\\alfalfa_Gb_git\\Simulations\\sim.bat')
