#bootstrap linear model power test
foo = read.csv("S:\\Andrew.Brown\\CampbellAlfalfa\\310315_plant-counts.csv")
foo$bio=foo$bio*2
foo=data.frame(foo)
bio=foo$bio
treat=foo$treat
water=foo$water
timer=foo$day
block=foo$block
mt=lm(bio ~ block)
m1=lm(bio ~ water*timer + block)
model=anova(m1)
pwr.f2.test(7,55,f2 = 13870/(127802+13870))
#test just timing effect
subse=which(foo$day>0)
jt=aov(bio[subse]~factor(timer[subse]))
plot(TukeyHSD(x = jt))
anova(lm(foo$finalP[subse]~foo$initialP[subse]))


deltaP=foo$initialP-(2*foo$finalP)
subse=which(foo$water>0)
anova(lm(foo$bio[subse]~foo$day[subse]*foo$water[subse]+foo$initialP[subse]+foo$block[subse]))
subse=1:length(foo$initialP)
anova(lm(deltaP[subse]~foo$day[subse]*foo$water[subse]+foo$initialP[subse]+foo$block[subse]))

damaged=which(deltaP<=-5)
anova(lm(foo$bio[damaged]~foo$day[damaged]*foo$water[damaged]+foo$initialP[damaged]+foo$block[damaged]))

stpp=foo$finalS/foo$finalP
anova(lm(stpp[subse]~foo$day[subse]*foo$water[subse]+foo$initialP[subse]+foo$block[subse]))
boxplot(stpp[subse]~foo$day[subse])
subse3=which(foo$block==1)
boxplot(foo$bio[subse3]~foo$day[subse3])

#effect size of RCB
library(pwr)
PVb=(eta2)
eff_CRD=eta2
eff_RCB=eff_CRD/sqrt(1-PVb)

Nj     <- c(9,9,9,9,9,9,9)               # group sizes for 7 groups
mu     <- aggregate(foo$bio,list(foo$treat),mean)$x           # expected values in groups
sigma  <- aggregate(foo$bio,list(foo$treat),sd)$x                    # true standard deviations in groups
mus    <- rep(mu, times=Nj[1])             # for use in rnorm(): vector of mus
sigmas <- rep(sigma, times=Nj[1])          # for use in rnorm(): vector of true sds
IV     <- factor(c(rep(1, 7*3),rep(2,7*3),rep(3,7*3)))   # factor for ANOVA
nsims  <- 1000                          # number of simulations

# reference: correct power
power.anova.test(groups=7, n=Nj[1], between.var=var(mu), within.var=sigma[1]^2)$power

doSim <- function() {                   # function to run one ANOVA on simulated data
  DV <- rnorm(sum(Nj), mus, sigmas)   # data from all three groups
  anova(lm(DV ~ IV))["IV", "Pr(>F)"]  # p-value from ANOVA
}

pVals  <- replicate(nsims, doSim())     # run the simulation nsims times
(power <- sum(pVals < 0.05) / nsims)    # fraction of significant ANOVAs


cohen.ES("anov",size="large")

pwr.anova.test(k=7,n=3,f=0.4)

library(boot)

interaction.plot(foo$water,foo$day,foo$bio)

fitter=""
rsq <- function(dep, ind1, ind2, blk, ninds, data, indices) {
  d <- data[indices[1:ninds],]
  #fit <- lm(dep[indices[1:ninds]]~ind[indices[1:ninds]]+blk[indices[1:ninds]], data=d)
  #fitter <<-cbind(fitter,fit$coef)
  fit <<- lm(dep[indices[1:ninds]]~ind1[indices[1:ninds]]+ind2[indices[1:ninds]]+blk[indices[1:ninds]], data=d)
  #return(summary(fit)$r.square)
  #r2=(coef(fit)[[2]])
  return(c(coef(fit)[[2]],coef(fit)[[3]]))
  #return(r2)
  #return(pwr.f2.test(8,54,r2/(1-r2))$power)
  #return(pwr.f2.test(6,56,r2/(1-r2))$power)  
} 
# bootstrapping with 1000 replications 
#results <- boot(data=foo, statistic=rsq,R=1000, dep=foo$bio, ind=foo$treat, blk=foo$block, ninds=63)
results <- boot(data=foo, statistic=rsq,R=1000, dep=foo$bio, ind1=foo$day, ind2=foo$water, blk=foo$block, ninds=63)
# view results
results 
plot(results)
# get 95% confidence interval 
boot.ci(results, type="bca")
min(results$t)
max(results$t)
sum(results$t>=0.5)/sum(results$t>-1)



bio=foo$bio
treat=foo$treat
water=foo$water
timer=foo$day
block=foo$block

mydata=scale(foo$bio)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
flit=kmeans(mydata, 3)
aggregate(mydata,by=list(flit$cluster),FUN=mean)
mydata <- data.frame(mydata, flit$cluster)

# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model
clazz=fit$classification

pa=pam(dist(data.frame(scale(cbind(foo$initialP,foo$subplot,foo$block)))),k=3)
plot(pa$clustering)

coef(lm(foo$bio~foo$water))

m=lm(bio~water*timer+pa$clustering)
m1=lme(bio~water*timer,foo,~1|block)
m3=lme(bio ~ water*day, foo, ~ 1|subplot/plot)

bar=list()
baz=list()
for(i in 1:21) {
  nix=c()
  for(s in 1:3) {
    b=foo[((foo$plot==i)+(foo$subplot==s))==2,]$bio
    bar=c(bar,b)
    nix=c(nix,b)
  }
  baz=c(baz,mean(nix))
}

