sv_bio1=read.csv("27-05-15_SV_justbio.csv", stringsAsFactors=FALSE)
sv_bio2=read.csv("15-7-15_SV_bio_Rinput.csv",stringsAsFactors=FALSE)

cut1=(aov(sv_bio1$bio~sv_bio1$treat))
summary(cut1)
TukeyHSD(cut1)

cut2=(aov(sv_bio2$bio~sv_bio2$treat))
summary(cut2)
TukeyHSD(cut2)
