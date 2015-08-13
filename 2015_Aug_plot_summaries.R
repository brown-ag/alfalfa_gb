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

#test for outliers
grubbs.test(sv_bio1$bio)
grubbs.test(sv_bio2$bio)

#individual barplots
getbarplot(sv_bio1,"treat",c("Control","Low","Medium","High"))
a1=aov(sv_bio1$bio ~ sv_bio1$treat)
plot(a1)
summary(a1)
plot(TukeyHSD(a1,conf.level=0.95)) #no means separation required

getbarplot(sv_bio2,"treat",c("Control","Low","Medium","High"))
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
svbio$treat=factor(svbio$treat,levels=c("Control","Low","Medium","High"))
svbio$bio=svbio$bio*2 #biomass per square meter conversion

#write.csv(svbio,file='2015_sv_biomass.csv')
#getbarplot(svbio,"treat",c("Control","Low","Medium","High"))

melt1=melt(svbio, id.vars=c("cut","treat"))
means1.sem <- ddply(melt1, c("cut","treat","variable"), summarise, mean=mean(value), sem=sd(value)/sqrt(length(value)))
means1.sem <- transform(means1.sem, lower=mean-sem, upper=mean+sem)
names(means1.sem) = c("cut","treat","variable","bio","sem","lower","upper")

cbar=ggplot(means1.sem,aes(treat,bio,fill=factor(cut)))+geom_bar(stat="identity",position="dodge")+scale_fill_discrete(name="Cutting",breaks=c(1, 2),labels=c("1st", "2nd"))+xlab("Flooding intensity")+ylab("Mean alfalfa biomass, g/0.5m2")+geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=means1.sem)+theme(text = element_text(size=24))
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
ctbar=ggplot(means22.sem,aes(treat,mean))+geom_bar(stat="identity",position="dodge")+xlab("Flooding intensity")+ylab(bquote('Mean alfalfa biomass,'~g-m^-2~''))+geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=means22.sem)+theme(text = element_text(size=24))
ctbar

  #SV - First cutting weeds
sv_weed=read.csv("27-05-15_SV_bio.csv", stringsAsFactors=FALSE)
sv_weed_spp = read.csv("27-05-15_weedsout.csv", stringsAsFactors=FALSE)
totalbio=sv_weed$AlfBag+sv_weed$WeedBag
plot(aov(totalbio ~ sv_weed$Treat))

sv_weedc=cbind(sv_weed['Treat'],sv_weed['Plant.C'],sv_weed['WeedBag'],sv_weed_spp)
sv_weedc$Plant.C=sv_weedc$Plant.C*2
sv_weedc$WeedBag=sv_weedc$WeedBag*2
sv_weedc$Treat=factor(sv_weedc$Treat,levels=c("Control","Low","Medium","High"))
melt3=melt(sv_weedc,id.vars=c("Treat"))
means3.sem=ddply(melt3,c("Treat","variable"),summarise,mean=mean(value),sem=sd(value)/sqrt(length(value)))
means3.sem <- transform(means3.sem, lower=mean-sem, upper=mean+sem)
names(means3.sem) = c("Treat","variable","mean","sem","lower","upper")
means3_plantc=means3.sem[which(means3.sem$variable == "Plant.C"),] 
svpcbar=ggplot(means3_plantc,aes(Treat,mean))+geom_bar(stat="identity",position="dodge")+xlab("Flooding intensity")+ylab(bquote('Mean plant count,' ~plants-m^-2 ~''))+geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=means3_plantc)+theme(text = element_text(size=24))
svpcbar

means3_weedbio=means3.sem[which(means3.sem$variable == "WeedBag"),] 
svwbbar=ggplot(means3_weedbio,aes(Treat,mean))+geom_bar(stat="identity",position="dodge")+scale_fill_discrete(name="Cutting",breaks=c(1, 2),labels=c("1st", "2nd"))+xlab("Flooding intensity")+ylab(bquote('Mean weed biomass,'~g-m^-2~''))+geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=means3_weedbio)+theme(text = element_text(size=24))
svwbbar
#----------------------------------------------------------------------------------

# Nitrate bargraphs
  #SV - Before, middle, after
sv_no3=read.csv("2015_sv_no3_final.csv")
sv_no3$treat=factor(sv_no3$treat,labels=c("Control","Low","Medium","High"))
melt4=melt(sv_no3,id.vars=c("treat","rdepth"))
means4.sem=ddply(melt4, c("treat","rdepth","variable"),summarise,mean=mean(value),sem=sd(value)/sqrt(length(value)))
means4.sem=transform(means4.sem,lower=mean-sem,upper=mean+sem)
sv_july_no3=means4.sem[which(means4.sem$variable=="no3"),]
#sv_july_no3=sv_july_no3[which(sv_july_no3$rdepth!=37.5),]
svjnbar=ggplot(sv_july_no3,aes(treat,mean,fill=factor(rdepth)))+geom_bar(stat="identity",position="dodge")+scale_fill_discrete(name="Depth",breaks=c(7.5, 22.5,37.5),labels=c("0-15 cm", "15-30 cm","30-45 cm"))+xlab("Flooding intensity")+ylab(bquote(~NO[3]~'-N concentration, '~mg-kg^-1~' soil'))+geom_errorbar(aes(ymax=upper,ymin=lower),position=position_dodge(0.9),data=sv_july_no3)+theme(text = element_text(size=24))
svjnbar
#----------------------------------------------------------------------------------
#EC
  #SV - before

  #CT - before