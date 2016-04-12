foo=read.csv("310315_plant-counts.csv")
#head(foo)

c0=aov(foo$bio~foo$water*foo$day+foo$block)
summary(c0)
c1=aov(foo$bio~foo$initialP+foo$water*foo$day+foo$block)
summary(c1)
c2=aov(foo$bio~foo$finalP+foo$water*foo$day+foo$block+Error(foo$subplot/(foo$water*foo$day)))
c3=aov(foo$bio~foo$initialP+foo$water*foo$day+foo$block+Error(foo$subplot/(foo$water*foo$day)))

fm0=lmer(bio~water*day+block,data=foo)
fm1=lmer(bio~initialP+water*day+block)
fm2=lmer(bio~finalP+water*day+block)
fm01=lmer(bio~water*day+(1|block),data=foo)
library(lme4)

summary(c3)
anova(c0,c2)


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
summary(aov(newfoo$bio~newfoo$iplants+newfoo$water+newfoo$day+newfoo$block))
