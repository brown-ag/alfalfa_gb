#bootstrap linear model power test
foo = read.csv("S:\\Andrew\\CampbellAlfalfa\\310315_plant-counts.csv")
foo$bio=foo$bio*2
foo=data.frame(foo)
library(boot)
library(pwr)
fitter=""
rsq <- function(dep, ind, blk, ninds, data, indices) {
  d <- data[indices[1:ninds],]
  fit <- lm(dep[indices[1:ninds]]~ind[indices[1:ninds]]+blk[indices[1:ninds]], data=d)
  fitter<<-cbind(fitter,fit$coef)
  #fit <<- lm(dep[indices[1:ninds]]~ind[indices[1:ninds]], data=d)
  #return(summary(fit)$r.square)
  #r2=(mean(dep[ind!='C'])-mean(dep[ind=='C']))/sd(dep[ind=='C'])
  #(summary(fit)$r.square)
  return(r2)
  #return(pwr.f2.test(8,54,r2/(1-r2))$power)
  #return(pwr.f2.test(6,56,r2/(1-r2))$power)  
} 
# bootstrapping with 1000 replications 
#results <- boot(data=foo, statistic=rsq,R=1000, dep=foo$bio, ind=foo$treat, blk=foo$block, ninds=63)
results <- boot(data=foo, statistic=rsq,R=1000, dep=foo$bio, ind=foo$treat, blk=foo$block,ninds=63)
# view results
results 
plot(results)
# get 95% confidence interval 
boot.ci(results, type="bca")
min(results$t)
max(results$t)
sum(results$t>=0.5)/sum(results$t>-1)

library(nlme)
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

m=lm(bio~water*timer+pa$clustering)
m1=lme(bio~water*day,foo,~1|block)
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

