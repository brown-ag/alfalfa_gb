#SV final analysis

cvol=read.csv("sv_control_volume.csv")
foob=read.csv("27-05-15_SV_bio.csv")
foob2=read.csv("2015_sv_biomass.csv")
water_ctoc=rev(cvol$water)
foob2$water=water_ctoc[foob2$check]

foob2$bio=foob2$bio*2*0.0040468564

cut1=foob2[which(foob2$cut==1),]
cut2=foob2[which(foob2$cut==2),]

modcut1=lm(data=cut1,bio~water)
modcut2=lm(data=cut2,bio~water)

summary(modcut1)

plot(data=cut1,bio~rev(water))
abline(lm(data=cut1,bio~rev(water)))
plot(data=cut2,bio~water)

library(lattice)
xyplot(bio~water|cut,data=foob2)


funner=function(data, indices, ninds) {
  d=data[indices,]
  m=lm(d$bio~d$water)
  co=coef(m)
  return(co)
}
bo=boot(data=cut1,R=9999,statistic=funner,ninds=length(cut1$cut))
bo2=boot(data=cut2,R=9999,statistic=funner,ninds=length(cut2$cut))
plot(bo,index=1)
plot(bo,index=2)

plot(bo2,index=1)
plot(bo2,index=2)
boot.ci(bo,conf=0.95,type="bca",index=1)
boot.ci(bo,conf=0.95,type="bca",index=2)
boot.ci(bo2,conf=0.95,type="bca",index=1)
boot.ci(bo2,conf=0.95,type="bca",index=2)
?boot.ci

predict(cut1,newdata=(data.frame(water=c(0,6,10,28))))


bo
bo2

mc1=aggregate(cut1$bio,by=list(cut1$treat),FUN=mean)
sc1=aggregate(cut1$bio,by=list(cut1$treat),FUN=sd)
mc2=aggregate(cut2$bio,by=list(cut1$treat),FUN=mean)
sc2=aggregate(cut2$bio,by=list(cut1$treat),FUN=sd)

cbind(mc1,sc1[,2]/sqrt(8))
cbind(mc2,sc2[,2]/sqrt(8))
cut1no=cut1[which(cut1$treat != "H"),]
cut1modno=lm(data=cut1no,bio~water)
cut2no=cut2[which(cut2$treat != "H"),]
cut2modno=lm(data=cut2no,bio~water)

library(multcomp)
tuk <- glht(aov(cut2,bio~treat), linfct = mcp(tx = "Tukey"))


TukeyHSD(aov(cut1$bio~cut1$treat))
TukeyHSD(aov(cut2$bio~cut2$treat))
TukeyHSD(aov(cut1no$bio~cut1no$treat))
TukeyHSD(aov(cut2no$bio~cut2no$treat))

library(agricolae)
print(HSD.test(aov(data=cut2,bio~treat), "tx", group=TRUE))


cut1predict=predict(modcut1,newdata=data.frame(water=c(0,6,10,28)),interval="predict")
cut2predict=predict(modcut2,newdata=data.frame(water=c(0,6,10,28)),interval="predict")
transform(cut1predict,diff=(lwr-upr)/2)
transform(cut2predict,diff=(lwr-upr)/2)

cut1nopredict=predict(cut1modno,newdata=data.frame(water=c(0,6,10,28)),interval="predict")
cut2nopredict=predict(cut2modno,newdata=data.frame(water=c(0,6,10,28)),interval="predict")
transform(cut1nopredict,diff=(lwr-upr)/2)
transform(cut2nopredict,diff=(lwr-upr)/2)

bono=boot(data=cut1no,R=9999,statistic=funner,ninds=length(cut1$cut))
bono2=boot(data=cut2no,R=9999,statistic=funner,ninds=length(cut2$cut))
plot(bono,index=1)
plot(bono,index=2)
plot(bono2,index=1)
plot(bono2,index=2)
boot.ci(bono,conf=0.95,type="bca",index=1)
boot.ci(bono,conf=0.95,type="bca",index=2)
boot.ci(bono2,conf=0.95,type="bca",index=1)
boot.ci(bono2,conf=0.95,type="bca",index=2)

