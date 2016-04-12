cvol=read.csv("sv_control_volume.csv")
foob=read.csv("27-05-15_SV_bio.csv")
foob2=read.csv("2015_sv_biomass.csv")
water=cvol$water[foob$check]

tmap=c(H=28,M=10,L=6,C=0)
twater=as.numeric(tmap[foob$Treat])

t0=lm(foob$AlfBag~twater)
summary(t0)

t1=lm(foob$AlfBag~water)
plot(foob$AlfBag~water)
summary(t1) #no significant differences across field (p=0.527)

t2=lm(foob$WeedBag~water)
plot(foob$WeedBag~water)
summary(t2) #no significant differences across field (p=0.884)

t3=lm((foob$AlfBag+foob$WeedBag)~water)
plot((foob$AlfBag+foob$WeedBag)~water)
summary(t3) #no significant differences across field (p=0.593)

delP=foob$ipc-foob$ppc
plot(delP~water)
t4=lm(delP~water) #mean loss of 3.65 plants per square meter across whole field. no sigificant trent (p=0.298)
summary(t4)

t6=lm(foob$WeedBag~foob$ppc)
summary(t6) #Significant decrease in weedbiomass for increasing alfalfa plant count (p=0.00253)
#Suggestive of weed biomass opportunistic growth in low plant density areas.

#summary stats
#Intercept
bio=mean(foob$AlfBag+foob$WeedBag)*0.02 #3.64 Mg/ha total yield
biosd=sd(foob$AlfBag+foob$WeedBag)*0.02 
bio
biosd/sqrt(length(foob$AlfBag))

alfrat=mean(foob$AlfBag)*0.02
alfsd=sd(foob$AlfBag)*0.02
alfrat
alfrat/sqrt(length(foob$AlfBag)) #2.77 Mg/ha just alfalfa

plot(foob$AlfBag~foob$check)

#Using distance as component of regression
jmdist=read.csv('jm_veg_dist.csv')
distr=c()
#Get sets for each plot
for(p in 1:length(foob$Plot)) {
  who=which(foob$Plot[p]==jmdist$qid)
  distr=c(distr,min(jmdist[who,]$dist))
}
t7=lm(foob$AlfBag~water+distr)
summary(t7)
t8=lm(foob$WeedBag~water+distr)
summary(t8)
anova(t1,t7)


#ANCOVA
c0=lm(foob$AlfBag~foob$ipc+water)
summary(c0)


#using location as component of regression
library(lattice)
bioloc=read.csv("sv_bio_locations.csv")
locs=bioloc[foob$Plot,2:3]

require(akima)
gps.interp <- with( locs, interp(x=locs$lat, y=locs$lng, z=foob$AlfBag))
contour(gps.interp)



cut2=foob2[which(foob2$cut==2),]
plot(as.numeric(tmap[cut2$treat]),cut2$bio)

#aggregating by location

locbio=aggregate(foob2$bio*2,by=list(foob2$treat),FUN=sum)
countz=aggregate(foob2$bio,by=list(foob2$treat),FUN=length)
plot(locbio[,1],locbio[,2]/countz[,2])
