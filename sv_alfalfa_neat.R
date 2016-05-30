cvol=read.csv("sv_control_volume.csv")
foob=read.csv("27-05-15_SV_bio.csv")
foob2=read.csv("2015_sv_biomass.csv")
water=cvol$water[foob$check]

tmap=c(H=28,M=10,L=6,C=0)
twater=as.numeric(tmap[foob$Treat])

days_inundated=rev(c(37,37,37,11,10,13,8,5,5,0,0))
dmap=days_inundated[foob2$check]


t0=lm(foob$AlfBag~twater)
summary(t0)
foob2=cbind(foob2,data.frame(water=c(water,water)))
foob2=transform(foob2,bio=bio*2*0.0040468564)

cmod=lm(bio~water,data=foob2)

plot(bio~dmap,data=foob2,pch=cut)

plot(bio~water,pch=cut,col=cut,data=foob2)
abline(cmod)
pcmod=predict(cmod, newdata=data.frame(water),interval="conf")
lines(lty=3,pcmod[,3])
lines(lty=3,pcmod[,2])
pcmod

foob

pcmod=predict(cmod,newdata=twater)
t1=lm(foob$AlfBag~water)
plot(foob$AlfBag~water)
summary(t1) #no significant differences across field (p=0.527)
t2=lm(foob$WeedBag~water)
plot(foob$WeedBag~water)
summary(t2) #no significant differences across field (p=0.884)
foob
t3=lm(((foob$AlfBag+foob$WeedBag)*2*0.0040468564)~foob$ipc+twater)
plot((foob$AlfBag+foob$WeedBag)~water)
summary(t3) #no significant differences across field (p=0.593)

plot((foob2$bio[which(foob2$cut==2)]*2*0.0040468564)~rev(water),ylim=c(0,3))
lmcut2=lm((foob2$bio[which(foob2$cut==2)]*2*0.0040468564)~rev(water))

foob2

lmcut2d=lm((foob2$bio[which(foob2$cut==2)]*2*0.0040468564)~dmap[which(foob2$cut==2)])
summary(lmcut2d)
abline(lmcut2)
predcut2=predict(lmcut2,data.frame(water=seq(0,30,1)),interval="predict")
head(predcut2)
lines(seq(0,30,1),rev(predcut2[,2]))
lines(seq(0,30,1),rev(predcut2[,3]))
summary(lmcut2)

library(boot)
library(dismo)
df1=data.frame(bio=(foob2$bio[which(foob2$cut==2)]*2*0.0040468564),water=rev(water))
set.seed(5132015)
kf <- kfold(nrow(df1),k = 8)
RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}
rmse <- rep(NA, 5)
slopeR <- rep(NA, 5)
for (k in 1:5) {
  test <- df1[kf == k, ]
  train <- df1[kf != k, ]
  modk=lm(data=train, bio~water)
  trpred=predict(modk,data.frame(water=test$water))
  rmse[k] <- RMSE(trpred, test$bio)
  slopeR[k] = coef(modk)[2]
}
nullm=lm(data=df1,bio~1)
null=RMSE(df1$bio,mean(bio))
rmse
slopeR
mean(rmse)
1 - (mean(rmse) / null)

water2=c(water,water)
ipc2=c(foob$ipc,foob$ipc)
dftmod=data.frame(ipc=ipc2,water=water2,cut=foob2$cut)
TMOD=lm(foob2$bio~ipc2+water2+foob2$cut+ipc2*water2)
library(MASS)
stepAIC(TMOD)
dftmod$ipc=rep(10,length(dftmod$ipc))
dftmod$cut=rep(2,length(dftmod$ipc))
p1=predict(TMOD,newdata=dftmod[rep(1:32,2),],interval="predict")
dftmod=cbind(dftmod,p1)
plot(dftmod$water,dftmod$fit)

install.packages("vegan")
library(vegan)
dat1=data.frame(bio=foob2$bio[which(foob2$cut==2)])
grp1=factor(twater)
mrpp(dat1,grp1)


library(car)
library(lmtest)
#install.packages("lmtest")
bio=round(foob$AlfBag+foob$WeedBag,digits=0)
g0=glm(bio~1,family="poisson")
g1=glm(bio~water,family="poisson")
g2=glm(bio~foob$ipc+water,family="poisson")
g3=glm(bio~foob$ipc+water+foob$X.Cover,family="poisson")

lrtest(g0,g1,g2,g3)

pc=foob$ppc
ic=foob$ipc
co=foob$X.Cover
aa=foob$D
gp0=glm(pc~1,family="poisson")
gp1=glm(pc~water,family="poisson")
gp2=glm(pc~ic+water,family="poisson")
gp3=glm(pc~ic+water+co,family="poisson")
gp4=glm(pc~ic+water+co+aa,family="poisson")
lrtest(gp0,gp1,gp2,gp3,gp4)
vif(gp3)
confint(gp3)


vif(gp3)
preddat <- predict(g3, newdata=data.frame(water=1:32,foob$ipc=rep(20,32),foob$X.Cover=rep(50,32)), se.fit=TRUE)
preddat
plot(preddat$fit)

vif(g2)
vif(g3)
delP=foob$ipc-foob$ppc
plot(delP~water)
t4=lm(delP~water) #mean loss of 3.65 plants per square meter across whole field. no sigificant trent (p=0.298)
summary(t4)

t6=lm(foob$WeedBag~foob$ppc+water)
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
t7=lm(foob$AlfBag~foob$ipc+water+distr)
summary(t7)
t8=lm(foob$WeedBag~water+distr)
summary(t8)
anova(t1,t7)


#ANCOVA
c0=lm(foob$AlfBag~foob$ipc+water)
summary(c0)


foobsum=aggregate(foob[,c(4,5,9,10)],by=list(foob$Treat),FUN=sum)
foobsum=transform(foobsum,bio=AlfBag+WeedBag)
plot(foobsum$bio~foobsum$Group.1)

#using location as component of regression
library(lattice)
bioloc=read.csv("sv_bio_locations.csv")
locs=bioloc[foob$Plot,2:3]

require(akima)
gps.interp <- with( locs, interp(x=locs$lng, y=locs$lat, z=foob$AlfBag))
contour(gps.interp)
points(x=locs$lng, y=locs$lat)

cut2=foob2[which(foob2$cut==2),]
plot(as.numeric(tmap[cut2$treat]),cut2$bio)

#aggregating by location

locbio=aggregate(foob2$bio*2,by=list(foob2$treat),FUN=sum)
countz=aggregate(foob2$bio,by=list(foob2$treat),FUN=length)
plot(locbio[,1],locbio[,2]/countz[,2])


#LIKELIHOOD TEST
m0=lm(foob2$bio[which(foob2$cut==1)]~1)
m1=lm(foob2$bio[which(foob2$cut==1)]~twater[which(foob2$cut==1)])
anova(m0,m1)

m0=lm(foob2$bio[which(foob2$cut==2)]~1)
m1=lm(foob2$bio[which(foob2$cut==2)]~twater)
anova(m0,m1)
twater
pee=predict(m1,data.frame(twater=seq(0,28,1)),interval="predict")
plot(seq(0,28,1),pee[,1],ylim=c(0,400))
lines(seq(0,28,1),pee[,2])
lines(seq(0,28,1),pee[,3])
plot(foob2$bio[which(foob2$cut==2)],pch="*")
plot(foob2$bio[which(foob2$cut==1)]~,pch="*")

#MIXED Effects
library(lme4)
check=c(rep(1,4),rep(2,4),rep(3,3),rep(4,2),rep(5,3),rep(6,3),rep(7,2),rep(8,3),rep(9,3),rep(10,2),rep(11,3))
lme1=lmer(foob2$bio[which(foob2$cut==2)]~twater+(1|check))
summary(lme1)
stepAIC(lm(foob2$bio[which(foob2$cut==2)]~twater+check+foob$ipc))
stepAIC(lm(foob$ipc~check+foob$X.Cover))
mean(foob$ipc)
sd(foob$ipc)
gt100=which(foob2$bio > 100)
foob3=foob2[gt100,]
twater3=twater[gt100]
summary(lm(foob3$bio~twater3))

#check aggregation
foob2
cbio=aggregate(foob2$bio[which(foob2$cut==2)],by=list(check),FUN=function(x) { sum(x)/0.5/length(x) })
water
cwater=aggregate(water,by=list(check), FUN=function(x) x[1])
cbio
summary(lm(cbio[,2]~sort(cwater[,2])))
plot(cbio[,2]~sort(cwater[,2]))

plot(foob2$bio[which(foob2$cut==2)]~foob2$treat[which(foob2$cut==2)])

#piecewise
breaks <- twater[which(twater >= 9)]
mse <- numeric(length(breaks))
for(i in 1:length(breaks)){
  piecewise1 <- lm(data=foob2,bio ~ twater*(twater < breaks[i]) + twater*(twater>=breaks[i]))
  mse[i] <- summary(piecewise1)[6]
}
mse <- as.numeric(mse)
brea=breaks[which(mse==min(mse))[[1]]]
foob3=foob2[which(foob2$cut==2),]
foob3=cbind(foob3,twater=as.numeric(tmap[as.character(foob3$treat)]))
piecewise2 <- lm(data=foob3,bio ~ twater*(twater < 10) + twater*(twater > 10))
summary(piecewise2)

#autocorrelation
twater=c(rep(0,8),rep(6,8),rep(10,8),rep(28,8))
cut1=lm(foob2$bio[which(foob2$cut==1)]~twater)
cut2=lm(foob2$bio[which(foob2$cut==2)]~twater)
summary(cut2)
cut12=lm(foob2$bio[which(foob2$cut==1)]~foob$ipc[which(foob2$cut==1)]+twater)
summary(cut12)
plot(cut12)


plot(foob2$bio[which(foob2$cut==1)]~jitter(twater,0.5))
plot(foob2$bio[which(foob2$cut==2)]~jitter(twater,0.5))
plot(resid(cut1))
plot(resid(cut2))

bartlett.test(foob2$bio[which(foob2$cut==2)]~twater)

library(car)
dwt(cut1,simulate=TRUE,reps=9999)
dwt(cut2,simulate=TRUE,reps=9999)

plot(cut2)

summary(cut1)
summary(cut2)

#segmented
library(segmented)
lf1=lm(data=foob3,bio~twater)
segmented.lm(lf1,seg.Z=~twater)
library(spdep)
?moran.mc

moran.mc(cut2$residuals)
