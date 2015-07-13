#install.packages("stringr",repos="http://cran.us.r-project.org")
library(stringr)
foo = read.csv("27-05-15_SV_bio.csv", stringsAsFactor=FALSE)
head(foo)

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
#summary(mv1,test="W")
#summary(mv1,test="H")
#summary(mv1,test="P")
