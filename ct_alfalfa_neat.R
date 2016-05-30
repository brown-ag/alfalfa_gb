foo=read.csv("310315_plant-counts.csv")

#normalize to control then aggregate by plot
control=which(foo$treat=="C")
cnor=foo[control,]
cnorm=colMeans(cnor[,7:10])

fxo=foo[-control,]
fxo$initialP=fxo$initialP/cnor$initialP
fxo$finalP=fxo$finalP/cnor$finalP
fxo$finalS=fxo$finalS/cnor$finalS
fxo$bio=fxo$bio/cnor$bio
nrow(fxo)
fxo21=aggregate(fxo[,1:6],by=list(fxo$plot),FUN=function(x) x[1])[,2:6]
fxo2=aggregate(fxo[,7:10],by=list(fxo$plot),FUN=mean)[,2:5]
fxo2=cbind(fxo21,fxo2)
fxoa=(aov(data=fxo,bio~initialP+day*water+block))
etaSquared(fxoa,anova=TRUE)
plot(fxo2$bio~fxo2$treat)

#head(foo)
asdf=aggregate(foo$bio,by=list(foo$plot),FUN=sum)
trts=aggregate(foo$treat,by=list(foo$plot),FUN=function(x) x[1])
trtz=aggregate(foo$block,by=list(foo$plot),FUN=function(x) x[1])
plot(asdf[,2]~trts[,2]+trtz[,2])


c2=aov(data=foo, bio~initialP+water*day+block+Error(plot/subplot))
summary(c2)
etaSquared(c2)

c0=aov(foo$bio~foo$water*foo$day+foo$block)
summary(c0)
c1=aov(foo$bio~foo$initialP+foo$water*foo$day+foo$block)
summary(c1)
library(lsr)
etaSquared(c1,anova=TRUE,type=3)
c2=aov(foo$bio~foo$finalP+foo$water*foo$day+foo$block+Error(foo$subplot/(foo$water*foo$day)))
summary(c2)
c3=aov(foo$bio~foo$initialP+foo$water*foo$day+foo$block+Error(foo$subplot/(foo$water*foo$day)))
summary(c3)

anova(c2,c3)
library(lme4)
fmn=lmer(bio~(1|block),data=foo,REML=FALSE)
a0=lmer(data=foo,bio~water*day+(1|block),REML=FALSE)
a1=lmer(data=foo,bio~water*day+(1|block)+(1+(water*day|subplot)),REML=FALSE)
anova(fmn,a0,a1)

fmn=lmer(bio~(0),data=foo)
fmn=lmer(bio~(1|block),data=foo)

fm0=lmer(bio~water*day+(1|block),data=foo)
fm1=lmer(bio~initialP+water*day+(1|block),data=foo)
fm2=lmer(bio~initialP+water*day+(1|block)+(1|subplot),data=foo)
fm3=lmer(bio~initialP+water*day+(1|block)+(1+(water*day)|subplot),data=foo)
anova(fmn,fm0,fm1,fm2,fm3)

summary(c3)
anova(c0,c2)

plot(bio~water*day,data=foo)

 library(car)
water=factor(foo$water)
day=factor(foo$day)
block=factor(foo$block)
subplot=factor(foo$subplot)
a1=aov(foo$bio~water+day+block)
a2=aov(foo$bio~water+day+block+subplot)
a3=aov(foo$bio~water)
summary(a1)
summary(a2)
summary(a3)
Anova(a3)
summary(manova(cbind(foo$bio,foo$finalP,foo$finalS)~foo$initialP+foo$water*foo$day+foo$block))

dumb=function(x) {
  return(x[1])
}
#aggregate subplots to get biomass on 3m^2 area
newfoo=aggregate(foo$bio*2, by=list(foo$plot), FUN=sum)
newipl=aggregate(foo$initialP*2,by=list(foo$plot), FUN=sum)[,2]
newpl=aggregate(foo$finalP*4, by=list(foo$plot), FUN=sum)[,2]
newps=aggregate(foo$finalS*2, by=list(foo$plot), FUN=sum)[,2]
colnames(newfoo)=c("plot", "bio")
newwater=aggregate(foo$water,by=list(foo$plot), FUN=dumb)[,2]
newday=aggregate(foo$day,by=list(foo$plot), FUN=dumb)[,2]
newfoo=cbind(newfoo,iplants=newipl,plants=newpl,stems=newps,block=c(rep(1,7),rep(2,7),rep(3,7)),water=newwater,day=newday)
summary(manova(cbind(newfoo$bio,newfoo$plants,newfoo$stems)~newfoo$iplants+newfoo$water*newfoo$day+newfoo$block))
##
aovagg=aov(newfoo$bio~newfoo$iplants+factor(newfoo$water)*factor(newfoo$day)+factor(newfoo$block))
etaSquared(aovagg,anova=TRUE)
#aggregate subplots to get biomass on 3m^2 area
newfoo=aggregate(foo$bio*2, by=list(foo$plot), FUN=mean)
newipl=aggregate(foo$initialP*2,by=list(foo$plot), FUN=mean)[,2]
newpl=aggregate(foo$finalP*4, by=list(foo$plot), FUN=mean)[,2]
newps=aggregate(foo$finalS*2, by=list(foo$plot), FUN=mean)[,2]
colnames(newfoo)=c("plot", "bio")
newwater=aggregate(foo$water,by=list(foo$plot), FUN=dumb)[,2]
newday=aggregate(foo$day,by=list(foo$plot), FUN=dumb)[,2]
newfoo=cbind(newfoo,iplants=newipl,plants=newpl,stems=newps,block=c(rep(1,7),rep(2,7),rep(3,7)),water=newwater,day=newday)
coef(manova(cbind(newfoo$bio,newfoo$plants,newfoo$stems)~newfoo$iplants+newfoo$water*newfoo$day+newfoo$block))

summary(aov(newfoo$bio~newfoo$iplants+newfoo$water+newfoo$day+newfoo$block))
summary(aov(newfoo$plants~newfoo$iplants+newfoo$water+newfoo$day+newfoo$block))
summary(aov(newfoo$stems~newfoo$iplants+newfoo$water+newfoo$day+newfoo$block))
stpp=newfoo$stems/newfoo$plants
summary(aov(stpp~newfoo$iplants+newfoo$water+newfoo$day+newfoo$block))
