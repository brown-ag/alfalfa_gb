#bootstrap linear model power test
foo = read.csv("S:\\Andrew\\CampbellAlfalfa\\310315_plant-counts.csv")
foo$bio=foo$bio*2
foo=data.frame(foo)
library(boot)
library(pwr)
rsq <- function(dep, ind, blk, ninds, data, indices) {
  d <- data[indices[1:ninds],]
  fit <- lm(dep[indices[1:ninds]]~ind[indices[1:ninds]]+blk[indices[1:ninds]], data=d)
  #fit <<- lm(dep[indices[1:ninds]]~ind[indices[1:ninds]], data=d)
  return(summary(fit)$r.square)
  #r2=(summary(fit)$r.square)
  #return(pwr.f2.test(8,54,r2/(1-r2))$power)
  #return(pwr.f2.test(6,56,r2/(1-r2))$power)  
} 
# bootstrapping with 1000 replications 
results <- boot(data=foo, statistic=rsq,R=1000, dep=foo$bio, ind=foo$treat, blk=foo$block, ninds=63)
# view results
results 
plot(results)
# get 95% confidence interval 
boot.ci(results, type="bca")
min(results$t)
max(results$t)
sum(results$t>=0.9)/sum(results$t>-1)

library(nlme)
bio=foo$bio
treat=foo$treat
water=foo$water
timer=foo$day
block=foo$block
m1=lme(bio~water*day,foo,~1|block)
m3=lme(bio ~ water*day, foo, ~ 1|subplot/plot)
