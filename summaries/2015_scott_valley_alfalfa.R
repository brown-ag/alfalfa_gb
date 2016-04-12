sv_bio1=read.csv("27-05-15_SV_justbio_new.csv", stringsAsFactors=FALSE)
sv_bio1=read.csv("27-05-15_SV_bio.csv", stringsAsFactors=FALSE)
sv_bio2=read.csv("15-7-15_SV_bio_Rinput.csv",stringsAsFactors=FALSE)

loc=factor(rep(c(rep(1,2),rep(2,2),rep(3,2),rep(4,2)),4))

subse=which(sv_bio1$Treat != 'H')
cut1=(aov((sv_bio1$AlfBag[subse]+sv_bio1$WeedBag[subse])~sv_bio1$Treat[subse]))
summary(cut1)
TukeyHSD(cut1)

subse=which(sv_bio2$treat != 'H')
cut2=(aov(sv_bio2$bio[subse]~sv_bio2$treat[subse]))
summary(cut2)
TukeyHSD(cut2)

mapp=c(rep(0,8),rep(6,8),rep(10,8),rep(28,8))

bionew=(sv_bio2$bio)

cut11=(lm(sv_bio1$bio~mapp))
summary(cut11)

mapp2=mapp^2
mapp3=mapp^3

subse=which(mapp>=0)
cut21=(lm(sv_bio2$bio[subse]~mapp[subse]))
cut22=(lm(sv_bio2$bio[subse]~mapp[subse]+mapp2[subse]))
cut23=(lm(sv_bio2$bio[subse]~mapp[subse]+mapp2[subse]+mapp3[subse]))
anova(cut21,cut22,cut23)
summary(cut22)

test=seq(0,30)

yy=-18.9942*test+0.4635*(test^2)
boxplot(sv_bio2$bio[subse]~mapp[subse])
abline(cut22)
#H != C
#L == C
#M != C
#L != H
#M == H
#M == L

=2*2.668