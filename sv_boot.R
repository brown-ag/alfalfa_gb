#sv bootstrap
library(boot)

bfun=function(data,indices) {
  m1=lm(data$bio[indices]~data$twater[indices])
  return(coef(m1)[2])
}


svdf=read.csv("2015_sv_biomass.csv",stringsAsFactors=FALSE)
tmap=c("C"=0,"L"=6,"M"=10,"H"=28)
twater=as.numeric(tmap[svdf$treat])
svdf=cbind(svdf,twater)

m0=lm(data=svdf,bio~twater)
res1=boot(data=svdf[which(svdf$cut==1),],statistic=bfun,R=10000)
res1
res2=boot(data=svdf[which(svdf$cut==2),],statistic=bfun,R=10000)
res2
plot(res1)
plot(res2)
ci1=boot.ci(res1,conf=0.95,type="basic")
ci1
ci2=boot.ci(res2,conf=0.95,type="basic")
ci2

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

lmp(m0)
dolmp=function(data,indices) {
  return(lmp(lm(data$bio[indices]~data$twater[indices])))
}
res3=boot(data=svdf[which(svdf$cut==1),],statistic=dolmp,R=10000)
res3
sum(res3$t<0.05)/length(res3$t) #calculates power
