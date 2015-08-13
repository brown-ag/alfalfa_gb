#install.packages("stringr",repos="http://cran.us.r-project.org")
library(stringr)
foo = read.csv("27-05-15_SV_bio.csv", stringsAsFactor=FALSE)
bar = read.csv("27-05-15_weedsout.csv", stringsAsFactor=FALSE)
head(foo)
foo=(cbind(foo,bar))
#Alfalfa biomass, using plant count and % cover as covariate
a=(lm(log10(foo$AlfBag)~foo$Treat+foo$Plant.C+foo$X.Cover))
summary(a)
summary(lm(foo$AlfBag~foo$Plant.C))
#plot(TukeyHSD(a,conf.level=0.95))

#Weed biomass using alfalfa plant count and % cover  as covariate
b=(lm(log10(foo$WeedBag)~foo$Treat+foo$Plant.C+foo$X.Cover))
summary(b)
summary(lm(foo$WeedBag~foo$Plant.C))
#plot(TukeyHSD(b,conf.level=0.95))

#parse out weed variables
weeds=foo$Weeds

#print((weeds))
xx<-function(x) {
	#print(weeds[x])
	return(strsplit(weeds[x],";"))
}
yy<-function(y) {
	weeder=unlist(weed_s[y])
#	print(length(weeder))
	return(lapply(1:length(weeder),function(yyy) str_match(weeder[yyy],"([1-9]+)([a-z]{2})")))
}
weed_s=lapply(1:length(weeds),FUN=xx)
#print(weed_s)
weed_r=lapply(1:length(weeds),FUN=yy)
#print(length(weed_r))
matout=c()
specieslist=c()
countlist=c()
print(weed_r)
for(i in 1:length(weed_r))  {
	w=matrix(weed_r[i][[1]])
	#:print(w[1,1])
	print(length(w))
	for(ww in w)
		countlist=c(countlist,ww[,2])
		specieslist=c(specieslist,ww[,3])
#num_species=(length((w[,1])) / 3)	
	
	#for(ww in w) {
	#	#print ww
	#}
	#print(w[[1]])
	#specieslist=rbind(specieslist[i,],w[[1]][,3])
	#tout=rbind(matout,c(i,num_species))
	#int(num_species)
	#or(ww in 1:length(w)) {
		
}
df=data.frame(names(specieslist))
#warnings()
#MANOVA
mv1 <- manova(cbind(foo$AlfBag,foo$WeedBag) ~ foo$Treat+foo$Plant.C+foo$X.Cover, data=foo)
mv2 <- manova(cbind(foo$AlfBag,foo$WeedBag) ~ foo$Treat+foo$Plant.C+foo$X.Cover+foo$dl+foo$ma+foo$gr+foo$hs+foo$ca+foo$mu+foo$lq+foo$hb+foo$pw+foo$cw+foo$pl+foo$lc)
lm21 <- lm(foo$AlfBag~foo$dl+foo$ma+foo$gr+foo$hs+foo$ca+foo$mu+foo$lq+foo$hb+foo$pw+foo$cw+foo$pl+foo$lc)
lm22 <- lm(foo$WeedBag~foo$dl+foo$ma+foo$gr+foo$hs+foo$ca+foo$mu+foo$lq+foo$hb+foo$pw+foo$cw+foo$pl+foo$lc)
mv3 <- manova(cbind(foo$dl,foo$ma,foo$gr,foo$hs,foo$ca,foo$mu,foo$lq,foo$hb,foo$pw,foo$cw,foo$pl,foo$lc)~foo$Treat+foo$WeedBag+foo$AlfBag+foo$Plant.C)



#PCA

cols=cbind(foo$Plant.C,foo$X.Cover,foo$AlfBag,foo$WeedBag,foo$dl, foo$ma,foo$gr,foo$hs,foo$ca,foo$mu,foo$lq,foo$hb,foo$pw,foo$cw,foo$pl,foo$lc)
pcweeds=data.frame(cols)
names(pcweeds)=c("plantcount","xcover","alfbag","weedbag","dl","ma","gr","hs","ca","mu","lq","hb","pw","cw","pl","lc")
library(mvnormtest)
mat=na.omit(t(pcweeds))
mshapiro.test(mat)
#install.packages(c("corrplot","psych", "caret"))
library(psych)
library(corrplot)
library(caret)
round(cor(pcweeds),2) # correlation matrix
pairs.panels(pcweeds, lm=TRUE, rug=FALSE, method="pearson")
correlations<-cor(pcweeds)
corrplot(correlations,type = "upper")
findCorrelation(correlations, cutof=.6) 


sp.pca<-princomp(~pcweeds$plantcount+pcweeds$xcover+pcweeds$dl+pcweeds$ma+pcweeds$gr+pcweeds$hs+pcweeds$ca+pcweeds$mu+pcweeds$lq+pcweeds$hb+pcweeds$pw+pcweeds$cw+pcweeds$pl+pcweeds$lc, data=pcweeds, cor=TRUE, scores=TRUE)
sp.pca$scores # view all PC scores

cormat<-cor(cbind(sp.pca$scores,pcweeds)) #view the loadings used to make the biplot
cormat

summary(sp.pca)
plot(sp.pca,type="lines")
biplot(sp.pca)
library(rgl)
foo$colz[foo$Treat=="C"]="red"
foo$colz[foo$Treat=="L"]="orange"
foo$colz[foo$Treat=="M"]="yellow"
foo$colz[foo$Treat=="H"]="blue"
plot3d(sp.pca$scores[,1:3],col=foo$colz)
text3d(sp.pca$scores[,1:3],texts=rownames(pcweeds))
text3d(sp.pca$loadings[,1:3]*5.1,texts=rowames(sp.pca$loadings), col="red")
coords <- NULL
for (i in 1:nrow(sp.pca$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),sp.pca$loadings[i,1:3]))
}
lines3d(coords*5, col="red", lwd=4)

summary(lm(foo$AlfBag~sp.pca$scores[,1:3]))
summary(lm(sp.pca$scores[,3]~foo$Treat))
