#THORNTHWAITE-MATHER WATER BALANCE
#SCOTT VALLEY
library(lubridate)

ctract14=read.csv("SV_ET_2014.csv")
ctract14=cbind(data.frame(year=rep(2014,nrow(ctract14)),irrig=rep(0,nrow(ctract14))),ctract14)
ctract15=read.csv("SV_ET_2015.csv")
ctract15=cbind(data.frame(year=rep(2015,nrow(ctract15)),irrig=rep(0,nrow(ctract15))),ctract15)

#get irrigation data
events=read.csv("sv_waterapplied.csv")
#events_edit=edit(events)
idayz=events[,1]

apply(events,MARGIN=2,FUN=function(x) sum(x > 0))
days_inundated=c(37,37,37,11,10,13,8,5,5,0,0)

plot(apply(events[,2:12],MARGIN=2,FUN=sum)) #total applied water (mm) by treatment

start=yday("1-10-2014") #october 1st 2014 to october 1st 2015

ctract=rbind(ctract14[start:nrow(ctract14),],ctract15[1:start,])

ct_budget_all=data.frame(year=numeric(),day=numeric(), et=numeric(), stor=numeric(), rech=numeric(), irri=numeric())
for(i in 1:11) {
  #clear irrigation data buffer and then populate with treatment info
  irrig=rep(0,366) #create buffer for irrigation quantities
  irrig[(365-start)+idayz] = events[,i+1] #insert irrigation into buffer
  
  cptract=ctract #copy sv control data into buffer
  cptract$rain=cptract$rain+irrig #add irrigation to rain
  
  ct_exc=cptract$rain > cptract$et #ET = PET when rainfall exceeds et
  
  soilmax=5*30.48*0.5*10*0.25 #water content at field capacity is 0.25cm/cm, in a 5ft (1500mm) rootzone
  soilc=numeric()
  eta=numeric()
  deep=numeric()
  eterm=numeric()
  eterm=c(eterm,1)
  eta=c(eta,cptract$et[1])
  soilc=c(soilc,0.5*soilmax) #starting in october with half of field capacity
  deep=c(deep,0)
  for(m in 2:length(ct_exc)) {
    eterm=c(eterm,exp(-(cptract$et[m]-cptract$rain[m])/soilmax))
    soilc=c(soilc,soilc[m-1]*eterm[m])
    if(soilc[m] > soilmax) {
      deep=c(deep,soilc[m]-soilmax)
      soilc[m]=soilmax
    } else {
      deep=c(deep,0)
    }
    deltaS=soilc[m]-soilc[m-1]
    if(ct_exc[m]) {
      eta=c(eta,cptract$et[m])
    } else {
      eta=c(eta,cptract$et[m]-deltaS)
    }
  }
  ct_budget=data.frame(exp=rep(i,366),year=ctract$year,day=ctract$day_of_year,et=eta,stor=soilc,rech=deep,irri=cptract$rain)
  ct_budget_all=rbind(ct_budget_all,ct_budget)
}

#soil storage plot by treatment
control1=ct_budget_all[which(ct_budget_all$exp==11),]
continued1=ct_budget_all[which(ct_budget_all$exp==3),]
high1=ct_budget_all[which(ct_budget_all$exp==6),]
low1=ct_budget_all[which(ct_budget_all$exp==9),]

plot(control1$stor/soilmax,type="l",lty=1,ylim=c(0,1))
lines(continued1$stor/soilmax,type="l",lty=1,col="red")
lines(high1$stor/soilmax,type="l",lty=1,col="blue")
lines(low1$stor/soilmax,type="l",lty=1,col="green")


plot(cumsum(control1$rech)/1000,type="l",lty=1,ylim=c(0,150))
lines(cumsum(continued1$rech)/1000,type="l",lty=1,col="red")
lines(cumsum(high1$rech)/1000,type="l",lty=1,col="blue")
lines(cumsum(low1$rech)/1000,type="l",lty=1,col="green")


rechfrac=data.frame(day=1:366,control=0,continued=0,high=0,low=0)
deltas=c(0,diff(control1$stor))
rechfrac[,2]=(control1$rech)/(control1$rech+deltas+control1$et)
plot(rechfrac[,2],type="l",lty=1,ylim=c(0,1))
deltas=c(0,diff(continued1$stor))
rechfrac[,3]=(continued1$rech)/(continued1$rech+deltas+continued1$et)
lines(rechfrac[,3],type="l",lty=1,col="red")
deltas=c(0,diff(high1$stor))
rechfrac[,4]=(high1$rech)/(high1$rech+deltas+high1$et)
lines(rechfrac[,4],type="l",lty=1,col="blue")
deltas=c(0,diff(low1$stor))
rechfrac[,5]=(low1$rech)/(low1$rech+deltas+low1$et)
lines(rechfrac[,5],type="l",lty=1,col="green")

abovezero=apply(rechfrac[,2:5] > 0,MARGIN=2,FUN=which)
mean(rechfrac[abovezero$continued,3])
mean(rechfrac[abovezero$high,4])
mean(rechfrac[abovezero$low,5])

eend=yday("01-04-2015")
estart=1
estart:eend
