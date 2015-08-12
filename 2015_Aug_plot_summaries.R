#Alfalfa flooding tolerance summary plots for Helen; August 2015
#@author: andrew brown

#dependencies
library(ggplot2)
library(plyr)
library(reshape2)
library(outliers)
#----------------------------------------------------------------------------------
# Biomass bargraphs
  #SV - 2 cuttings
sv_bio1=read.csv("27-05-15_SV_justbio.csv", stringsAsFactors=FALSE)
sv_bio2=read.csv("15-7-15_SV_bio_Rinput.csv",stringsAsFactors=FALSE)

getbarplot <- function(df, expfactor, factorlevels, yymin=0, yymax=275) {
  melt1=melt(df, id.vars=c(expfactor))
  means1=ddply(melt1,c(expfactor,"variable"),summarise,mean=mean(value))
  means1$treat = factor(means1$treat,levels=factorlevels)
  means1.barplot=qplot(x=treat, y=mean,fill=variable,data=means1,stat="identity",position="dodge",ymax=yymax,ymin=yymin)
  means1.sem <- ddply(melt1, c("treat", "variable"), summarise, mean=mean(value), sem=sd(value)/sqrt(length(value)))
  means1.sem <- transform(means1.sem, lower=mean-sem, upper=mean+sem)
  means1.barplot + geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=means1.sem)
}

#test for outliers
grubbs.test(sv_bio1$bio)
grubbs.test(sv_bio2$bio)

#individual barplots
getbarplot(sv_bio1,"treat",c("C","L","M","H"))
a1=aov(sv_bio1$bio ~ sv_bio1$treat)
plot(a1)
summary(a1)
plot(TukeyHSD(a1,conf.level=0.95)) #no means separation required

getbarplot(sv_bio2,"treat",c("C","L","M","H"))
a2=aov(sv_bio2$bio ~ sv_bio2$treat)
plot(a2)
summary(a2)
plot(TukeyHSD(a2,conf.level=0.95)) # means separation: C == L > M == H

#combined first and 2nd cutting

svbio=cbind(rep(1,length(sv_bio1$bio)),sv_bio1)
names(svbio) = c("cut","treat","bio")
cut2=cbind(rep(2,length(sv_bio2$bio)),sv_bio2)
names(cut2) = c("cut","treat","bio")
svbio=rbind(svbio,cut2)
svbio$treat=factor(svbio$treat,levels=c("C","L","M","H"))
svbio$bio=svbio$bio*2 #biomass per square meter conversion

#write.csv(svbio,file='2015_sv_biomass.csv')
#getbarplot(svbio,"treat",c("C","L","M","H"))

melt1=melt(svbio, id.vars=c("cut","treat"))
means1.sem <- ddply(melt1, c("cut","treat","variable"), summarise, mean=mean(value), sem=sd(value)/sqrt(length(value)))
means1.sem <- transform(means1.sem, lower=mean-sem, upper=mean+sem)
names(means1.sem) = c("cut","treat","variable","bio","sem","lower","upper")

cbar=ggplot(means1.sem,aes(treat,bio,fill=factor(cut)))+geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Cutting",breaks=c(1, 2),labels=c("1st", "2nd"))+
  xlab("Flooding intensity")+ylab("Mean alfalfa biomass, g/0.5m2")+geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=means1.sem)
cbar 

  #CT - 1 cutting
ct_bio=read.csv("310315_plant-counts.csv",stringsAsFactors=FALSE)
ct_bio$bio=ct_bio$bio*2 #biomass per square meter conversion
ct_bio$treat=factor(ct_bio$treat,levels=c("C","JL","JH","FL","FH","ML","MH"))
melt2=melt(ct_bio, id.vars=c("treat"))
means2.sem <- ddply(melt2, c("treat","variable"), summarise, mean=mean(value), sem=sd(value)/sqrt(length(value)))
means2.sem <- transform(means2.sem, lower=mean-sem, upper=mean+sem)
names(means1.sem) = c("treat","variable","mean","sem","lower","upper")
means22.sem=means2.sem[which(means2.sem$variable == "bio"),] 
ctbar=ggplot(means22.sem,aes(treat,mean))+geom_bar(stat="identity",position="dodge")+
  xlab("Flooding intensity")+ylab("Mean alfalfa biomass, g/m2")+geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=means22.sem)
ctbar

  #SV - First cutting weeds
sv_weed=read.csv("27-05-15_SV_bio.csv", stringsAsFactors=FALSE)
sv_weed_spp = read.csv("27-05-15_weedsout.csv", stringsAsFactors=FALSE)
sv_weedc=cbind(sv_weed['Treat'],sv_weed['Plant.C'],sv_weed['WeedBag'],sv_weed_spp)
sv_weedc$Plant.C=sv_weedc$Plant.C*2
sv_weedc$WeedBag=sv_weedc$WeedBag*2
sv_weedc$Treat=factor(sv_weedc$Treat,levels=c("C","L","M","H"))
melt3=melt(sv_weedc,id.vars=c("Treat"))
means3.sem=ddply(melt3,c("Treat","variable"),summarise,mean=mean(value),sem=sd(value)/sqrt(length(value)))
means3.sem <- transform(means3.sem, lower=mean-sem, upper=mean+sem)
names(means3.sem) = c("Treat","variable","mean","sem","lower","upper")
means3_plantc=means3.sem[which(means3.sem$variable == "Plant.C"),] 
svpcbar=ggplot(means3_plantc,aes(Treat,mean))+geom_bar(stat="identity",position="dodge")+
  xlab("Flooding intensity")+ylab("Mean plant count, crowns/m2")+geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=means3_plantc)
svpcbar

means3_weedbio=means3.sem[which(means3.sem$variable == "WeedBag"),] 
svwbbar=ggplot(means3_weedbio,aes(Treat,mean))+geom_bar(stat="identity",position="dodge")+
  xlab("Flooding intensity")+ylab("Mean weed biomass, g/m2")+geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=means3_weedbio)
svwbbar
#----------------------------------------------------------------------------------

# Nitrate bargraphs
  #SV - Before, middle, after

  #CT - Before, After
#----------------------------------------------------------------------------------
#EC
  #SV - before

  #CT - before