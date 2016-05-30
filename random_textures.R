nsim=100000
sand=runif(nsim,min=0,max=100)
clay=numeric()
for(s in 1:length(sand)) {
  cmax=60
  if(sand[s] > 40) cmax=100-sand[s]
  clay[s] = runif(1, min=0,max=cmax)
}
silt=100-sand-clay
plot(sand,clay,pch=".")
write.csv(file="runif_textures.csv",data.frame(sand,silt,clay))

mean(sand)
mean(silt)
mean(clay)
