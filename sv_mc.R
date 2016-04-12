svdata=read.csv("2015_sv_biomass.csv")

cut1=which(svdata$cut==1)
cut2=which(svdata$cut==2)

library(MASS)
treatmap=data.frame(C=0,L=6,M=10,H=28)
svdata2=svdata
cut22=which(svdata$cut==2)
svdata3=na.omit(svdata2[cut22,])
summary(rlm(bio ~ 1, data=svdata3))


anBase <- anova(lm(bio ~ treat, data=svdata[cut1,]))
Fbase  <- anBase["treat", "F value"]
(pBase <- anBase["treat", "Pr(>F)"])

fit0 <- lm(svdata$bio[cut1] ~ 1, data=svdata)        ## fit 0-model
E    <- residuals(fit0)               ## residuals
Er   <- E / sqrt(1-hatvalues(fit0))   ## rescaled residuals
Yhat <- fitted(fit0)                  ## prediction

getAnova <- function(dat, idx) {
  Ystar <- Yhat + Er[idx]
  anBS  <- anova(lm(Ystar ~ treat, data=dat))
  anBS["treat", "F value"]
}

library(boot)
nR       <- 999
(bsAnova <- boot(svdata[cut1,], statistic=getAnova, R=nR))
Fstar    <- bsAnova$t
tol     <- .Machine$double.eps^0.5
FsIsGEQ <- (Fstar > Fbase) | (abs(Fstar-Fbase) < tol)
(pValBS <- (sum(FsIsGEQ) + 1) / (length(Fstar) + 1))

#cut2
anBase <- anova(lm(bio ~ treat, data=svdata[cut2,]))
Fbase  <- anBase["treat", "F value"]
(pBase <- anBase["treat", "Pr(>F)"])

fit0 <- lm(svdata$bio[cut2] ~ 1, data=svdata)        ## fit 0-model
E    <- residuals(fit0)               ## residuals
Er   <- E / sqrt(1-hatvalues(fit0))   ## rescaled residuals
Yhat <- fitted(fit0)                  ## prediction

fit1 <- lm(svdata$bio[cut2] ~ treat[cut2], data=svdata)        ## fit real measurements

getAnova <- function(dat, idx) {
  Ystar <- Yhat + Er[idx]
  anBS  <- (lm(Ystar ~ treat, data=dat))
  return(as.numeric(coef(anBS)[[2]]))
  #return(c(cc[[1]],cc[[2]]))
  #anBS["treat", "F value"]
}

nR       <- 999
(bsAnova <- boot(svdata[cut2,], statistic=getAnova, R=nR))
Fstar    <- bsAnova$t
tol     <- .Machine$double.eps^0.5
FsIsGEQ <- (Fstar > Fbase) | (abs(Fstar-Fbase) < tol)
(pValBS <- (sum(FsIsGEQ) + 1) / (length(Fstar) + 1))