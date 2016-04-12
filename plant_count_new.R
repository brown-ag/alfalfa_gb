foo = read.csv("S:\\Andrew.Brown\\CampbellAlfalfa\\310315_plant-counts.csv")
foo$bio=foo$bio*2
foo=data.frame(foo)

subse=which(foo$day>0)

controls=which(foo$day==0)
bb=1
asdf=function(blk) {
  controls=which(foo$day==0)
  cc=controls[which(foo$block[controls]==bb)]
  
}
newbio=c()
for(bb in foo$block) {
  cc=controls[which(foo$block[controls]==bb)]
  meanbio=mean(foo$bio[cc])
  sdbio=sd(foo$bio[cc])
  newbio
}

summary(aov(foo$bio~foo$treat+foo$block))

boxplot(log(foo$bio[subse])~foo$day[subse])
summary(lm(foo$bio[subse]~foo$day[subse]))
bartlett.test(foo$bio[subse]~foo$day[subse])
boxplot(log(foo$bio[subse])~foo$water[subse])

summary(lm(foo$bio~foo$day*foo$water+foo$block))

delP = foo$initialP - foo$finalP
boxplot(delP~foo$water*foo$day)
anova(lm(delP~foo$water+foo$block))
bartlett.test(delP~foo$treat)
library(mvoutlier)
outliers <- aq.plot(foo$bio)
outliers # show list of outliers