foo = read.csv("27-05-15_SV_bio.csv")
head(foo)

#Alfalfa biomass, using plant count and % cover as covariate
a=(lm(log10(foo$AlfBag)~foo$Treat+foo$Plant.C+foo$X.Cover))
summary(a)
plot(foo$AlfBag~foo$Plant.C)
#plot(TukeyHSD(a,conf.level=0.95))

#Weed biomass using alfalfa plant count and % cover  as covariate
b=(lm(log10(foo$WeedBag)~foo$Treat+foo$Plant.C+foo$X.Cover))
summary(b)
plot(foo$WeedBag~foo$Plant.C)
#plot(TukeyHSD(b,conf.level=0.95))

#parse out weed variables
weeds=foo$Weeds
weed_s=strsplit(weeds,";")



#MANOVA
mv1 <- manova(cbind(foo$AlfBag,foo$WeedBag) ~ foo$Treat+foo$Plant.C+foo$X.Cover, data=foo)
summary(mv1,test="W")
summary(mv1,test="H")
summary(mv1,test="P")
