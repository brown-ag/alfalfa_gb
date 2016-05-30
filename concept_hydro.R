#Concept hydrograph ggplot
library(ggplot2)

df=read.csv("concept_hydrograph.csv")[,2:3]
df[21,2] = df[length(df[,2]),2]
substor=df[,2]
subrech=df[,2]
subinit=c(df[1:17,2],rep(df[17,2],97-17))

yloc1=((df[length(df[,2]),2]-df[17,2])/2)+df[17,2]
yloc2=((df[17,2]-0.275)/2)+0.275
gt=which(df[,2] > df[length(df[,2]),2])
lt=which(df[,2] < df[length(df[,2]),2])

substor[gt] = NA
subrech[lt] = NA

df=cbind(df,substor,subrech)

dfline=ggplot(df,aes(time,vwc))+
  geom_area(aes(y=subrech,fill=4,ymin=0.25,ymax=0.35))+
  geom_area(aes(y=substor,fill=3,ymin=0.25,ymax=0.35))+
  geom_area(aes(y=subinit,fill=2,ymin=0.25,ymax=0.35))+
  geom_point()+
  geom_line()+
  geom_text(aes(x=600,y=0.3125,label="Recharge",size=20),colour=rgb(1,1,1),fontface = "bold",,size=12)+
  geom_text(aes(x=600,y=yloc1,label="Soil water stored",size=20),colour=rgb(1,1,1),fontface = "bold",size=12)+
  geom_text(aes(x=600,y=yloc2,label="Initial water content",size=20),colour=rgb(1,1,1),fontface = "bold",size=12)+
  coord_cartesian(ylim = c(0.275,0.35),expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  xlab("Time, minutes")+
  ylab('Volumetric water content (-)')+
  theme_classic()+
  theme(legend.position="none",panel.border = element_rect(colour = "black", fill=NA, size=0.5),axis.title.y=element_text(margin=margin(0,20,0,0)), axis.title.x=element_text(margin=margin(20,0,0,0)), text = element_text(size=28), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),plot.margin = unit(c(1,1,1,1), "cm"))
dfline
ggsave(dfline,filename="conceptual_vwc_hydrograph.pdf",units="cm",width=20,height=20)
