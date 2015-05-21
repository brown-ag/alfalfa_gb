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
range_n1=c(1.5,1.5)#c(1,4)
range_n2=c(3,3)#c(1,4)
range_ksat=c(0.6,0.8)#c(0.01, 1)
range_l=c(0.5,0.5)#c(0,1)
range_w2=c(0.60,0.70)
range_pulse=c(16,16)

makeSelectorFit <- function(n,id,a,b,c,d,e,f,g,h,i,j) {
  buffer=selector_template
  buffer2=fit_template
  buffer3=atmo_template
  values=c(id,a,b,c,d,e,f,g,h,i)
  avalues=c(j)
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

d_thetar=seq(range_thetar[1], range_thetar[2], (range_thetar[2]-range_thetar[1])/disc)
d_thetas=seq(range_thetas[1], range_thetas[2], (range_thetas[2]-range_thetas[1])/disc)
d_alpha1=seq(range_alpha1[1], range_alpha1[2], (range_alpha1[2]-range_alpha1[1])/disc)
d_alpha2=seq(range_alpha2[1], range_alpha2[2], (range_alpha2[2]-range_alpha2[1])/disc)
d_n1=seq(range_n1[1], range_n1[2], (range_n1[2]-range_n1[1])/disc)
d_n2=seq(range_n2[1], range_n2[2], (range_n2[2]-range_n2[1])/disc)
d_ksat=seq(range_ksat[1], range_ksat[2], (range_ksat[2]-range_ksat[1])/disc)
d_l=seq(range_l[1], range_l[2], (range_l[2]-range_l[1])/disc)
d_w2=seq(range_w2[1], range_w2[2], (range_w2[2]-range_w2[1])/disc)
d_pulse=seq(range_pulse[1], range_pulse[2], (range_pulse[2]-range_pulse[1])/disc)
map=matrix(ncol=11)
n=1
for(a in d_thetar) {
  for(b in d_thetas) {
    for(c in d_alpha1) {
      for(d in d_alpha2) {
        for(e in d_n1) {
          for(f in d_n2) {
            for(g in d_ksat) {
              for(h in d_l) {
                for(i in d_w2) {
                  for(j in d_pulse) {
                    dd=paste(".\\Simulations\\",n,sep="")
                    if(!dir.exists(dd)) {
                     dir.create(dd)
                    }
                    makeSelectorFit(n,5,a,b,c,d,e,f,g,h,i,j)
                    map=rbind(map,c(n,a,b,c,d,e,f,g,h,i,j))
                    n=n+1
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
makeBatch(n)
write.csv(map,map_fname)
shell('S:\\Andrew\\Code\\alfalfa_Gb_git\\Simulations\\sim.bat')
