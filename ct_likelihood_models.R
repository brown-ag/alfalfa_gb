ctdf=read.csv("310315_plant-counts.csv")
ctdf[,7:10]=2*ctdf[,7:10] #per square meter

cor(ctdf[,7:10])
install.packages("lmtest")
library(lmtest)
library(nlme)
library(lme4)
lm0=lm(data=ctdf,bio~1)
lmb=lm(data=ctdf,bio~block)
lm1=lm(data=ctdf,bio~day*water+block)
lm2=lm(data=ctdf,bio~initialP+day*water+block)
lm3=lm(data=ctdf,bio~initialP+finalP+day*water+block)
lrtest(lm0,lmb,lm1,lm2,lm3)
anova(lm0,lmb,lm1,lm2,lm3)

plot(data=ctdf,bio~finalP)

fp0=lm(data=ctdf,finalP~1)
fp11=lm(data=ctdf,finalP~initialP)
fp2=lm(data=ctdf,finalP~initialP+block)
fp3=lm(data=ctdf,finalP~initialP+day*water+block)
lrtest(fp0,fp1,fp2,fp3)

plot(data=ctdf,finalP~initialP)

fs0=lm(data=ctdf,finalS~1)
fs1=lm(data=ctdf,finalS~initialP)
fs2=lm(data=ctdf,finalS~initialP+block)
fs3=lm(data=ctdf,finalS~initialP+day+water+block)
fs4=lm(data=ctdf,finalS~finalP+day+water+block)
lrtest(fs0,fs1,fs2,fs4)

library(MASS)
library(car)
vif(fs2)
plot(data=ctdf,finalS~finalP,ylim=c(0,150),xlim=c(0,25))
abline(lm(data=ctdf,finalS~finalP))

summary(fs4)
plot(fp3)

summary(fp3)

m0=lmer(data=ctdf,bio~initialP+(1|plot)+(1|block),REML=F)
m1=lmer(data=ctdf,bio~initialP+day*water+(1|plot)+(1|block),REML=F)
m2=lmer(data=ctdf,bio~initialP+finalP+day*water+(1|plot)+(1|block),REML=F)
(AIC()-AIC(m0))*log2(exp(1))
anova(m2,m1,m0)
