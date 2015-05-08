library(soiltexture)
textures=cbind(tense$CLAY,tense$SILT,tense$SAND)
colnames(textures)= c("CLAY","SILT","SAND")
geo=TT.plot(class.sys="USDA.TT")
TT.points(textures,geo,pch=as.numeric(tense$depth=="D")+1, col=rep(c(rep(4,3),rep(3,3))))
legend(
  x = 80,
  y = 90,
  legend = c("Shallow = 60cm", "Deep = 150cm"), 
  pt.lwd = 4,
  pch = c(1,2),
  col = c(3,4)
)