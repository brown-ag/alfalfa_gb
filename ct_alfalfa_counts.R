foo = read.csv("S:\\Andrew\\CampbellAlfalfa\\310315_plant-counts.csv")

foo=foo[!(foo$water==0),]

block = factor(foo$block)
plot=factor(foo$plot)
subplot=factor(foo$subplot)
water=factor(foo$water)
time=factor(foo$day)

spp=foo$finalS/foo$finalP
bio=foo$bio
a= aov(bio~time*water+block)
anova(a)
plot(a)
plot(TukeyHSD(a,conf.level=0.95))

mv1 <- manova(cbind(bio,foo$finalP,foo$finalS) ~ foo$initialP+time*water+block, data=foo)
summary(mv1,test="W")
summary(mv1,test="H")
summary(mv1,test="P")
spip=foo$finalS/foo$initialP
summary(lm(bio~foo$initial))

library(nlme)
m=lme(spp~time*water,random=~1|block,data=foo) 
summary(m)

library(lme4)
m2=lmer(bio~time*water + (1|block) + (1|plot),data=foo)
m1=lm(spp~time*water+block*plot,data=foo)
m1grid=ref.grid(m1,data=foo)
m2grid=ref.grid(m2)
plot(Anova(m1, type="II"))
lsmeans(m2)
summary(m2)
coef(m2)

lmer(bio~time*water+(1|block))

m3=lmer(spp~time*water+(1|block)+(1|plot))
m4=lmer(spp~water*time+(1|block)+(1|plot))
ref.grid(m3,data=foo)
Anova(m3, type=3)
Anova(m4, type=3)
coef(m3)
plot(m3)
coef(m4)
plot(m4)

aaa=aov(bio~foo$initialP+time*water+Error(plot/subplot))
lmer(bio~time*water+(1|block))

with(foo, tapply(spp, list(foo$treat,subplot), mean))

library(rgl)
lines3d(x=foo$subplot,y=foo$plot,z=spp)

foo$P=foo$initialP-foo$finalP

library(ggplot2)
t = topo.colors(1)
asdf = (ggplot(foo, aes(x=foo$plot,y=bio))+
  geom_line()+
  geom_line(aes(x=foo$plot[foo$subplot==1],y=foo$bio[foo$subplot==1],foo$bio[foo$subplot==2],foo$bio[foo$subplot==3])),group=t[2],colour=t[2]))+
+theme_classic())
print(asdf)

aa=lm(spp~block+time*water)
predict(aa)
Anova(aa,type=2)
plot(spp~water:time)
plot(spp~time*water+block)
plot(deltaP~time*water*block)
plot(TukeyHSD(aov(foo$bio~time*water+block),conf.level=0.95))

anova(m3)