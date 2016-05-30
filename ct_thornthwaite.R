#THORNTHWAITE-MATHER WATER BALANCE
library(lubridate)

ctract14=read.csv("CT_ET_2014.csv")
ctract14=cbind(data.frame(year=rep(2014,nrow(ctract14)),irrig=rep(0,nrow(ctract14))),ctract14)
ctract15=read.csv("CT_ET_2015.csv")
ctract15=cbind(data.frame(year=rep(2015,nrow(ctract15)),irrig=rep(0,nrow(ctract15))),ctract15)

#get irrigation data
events=read.csv("ct_appliedwater.csv")
idayz=yday(as.POSIXct(events$Date,format="%d-%m-%y"))
events[,2:8]=events[,2:8]*10 #convert to mm

plot(apply(events[,2:8],MARGIN=2,FUN=sum)) #total applied water (mm) by treatment

start=yday("1-10-2014") #october 1st 2014 to october 1st 2015

ctract=rbind(ctract14[start:nrow(ctract14),],ctract15[1:start,])

ct_budget_all=data.frame(year=numeric(),day=numeric(), et=numeric(), stor=numeric(), rech=numeric(), irri=numeric())
for(i in 1:7) {
  #clear irrigation data buffer and then populate with treatment info
  irrig=rep(0,366) #create buffer for irrigation quantities
  irrig[(365-start)+idayz] = events[,i+1] #insert irrigation into buffer
  
  cptract=ctract #copy ctract control data into buffer
  cptract$rain=cptract$rain+irrig #add irrigation to rain
  
  ct_exc=cptract$rain > cptract$eto #ET = PET when rainfall exceeds et
  
  soilmax=5*30.48*10*0.25 #water content at field capacity is 0.25cm/cm, in a 5ft (1500mm) rootzone
  soilc=numeric()
  eta=numeric()
  deep=numeric()
  eterm=numeric()
  eterm=c(eterm,1)
  eta=c(eta,cptract$eto[1])
  soilc=c(soilc,0.5*soilmax) #starting in october with half of field capacity
  deep=c(deep,0)
  for(m in 2:length(ct_exc)) {
    eterm=c(eterm,exp(-(cptract$eto[m]-cptract$rain[m])/soilmax))
    soilc=c(soilc,soilc[m-1]*eterm[m])
    if(soilc[m] > soilmax) {
      deep=c(deep,soilc[m]-soilmax)
      soilc[m]=soilmax
    } else {
      deep=c(deep,0)
    }
    deltaS=soilc[m]-soilc[m-1]
    if(ct_exc[m]) {
      eta=c(eta,cptract$eto[m])
    } else {
      eta=c(eta,cptract$eto[m]-deltaS)
    }
  }
  ct_budget=data.frame(exp=i,year=ctract$year,day=ctract$doy,et=eta,stor=soilc,rech=deep,irri=cptract$rain)
  ct_budget_all=rbind(ct_budget_all,ct_budget)
}

#soil storage plot by treatment
control=ct_budget_all[which(ct_budget_all$exp==7),]
janhi=ct_budget_all[which(ct_budget_all$exp==2),]
febhi=ct_budget_all[which(ct_budget_all$exp==4),]
marhi=ct_budget_all[which(ct_budget_all$exp==6),]
janlo=ct_budget_all[which(ct_budget_all$exp==1),]
feblo=ct_budget_all[which(ct_budget_all$exp==3),]
marlo=ct_budget_all[which(ct_budget_all$exp==5),]

par(mfrow=c(1,1))
plot(control$stor/soilmax,type="l",lty=1,ylim=c(0,1))
lines(janhi$stor/soilmax,type="l",lty=1,col="red")
lines(febhi$stor/soilmax,type="l",lty=1,col="blue")
lines(marhi$stor/soilmax,type="l",lty=1,col="green")
lines(janlo$stor/soilmax,type="l",lty=2,col="red")
lines(feblo$stor/soilmax,type="l",lty=2,col="blue")
lines(marlo$stor/soilmax,type="l",lty=2,col="green")

rechfrac=data.frame(day=1:366,control=0,janhi=0,febhi=0,marhi=0,janlo=0,feblo=0,marlo=0)

deltas=c(0,diff(control$stor))
rechfrac[,2]=(control$rech)/(control$rech+control$stor+control$et)
plot(rechfrac[,2],type="l",lty=1,ylim=c(0,1))

deltas=c(0,diff(janhi$stor))
rechfrac[,3]=(janhi$rech)/(janhi$rech+deltas+janhi$et)
lines(rechfrac[,3],type="l",lty=1,col="red")

deltas=c(0,diff(febhi$stor))
rechfrac[,4]=(febhi$rech)/(febhi$rech+deltas+febhi$et)
lines(rechfrac[,4],type="l",lty=1,col="blue")

deltas=c(0,diff(marhi$stor))
rechfrac[,5]=(marhi$rech)/(marhi$rech+deltas+marhi$et)
lines(rechfrac[,5],type="l",lty=1,col="green")

deltas=c(0,diff(janlo$stor))
rechfrac[,6]=(janlo$rech)/(janlo$rech+deltas+janlo$et)
lines(rechfrac[,3],type="l",lty=1,col="red")

deltas=c(0,diff(feblo$stor))
rechfrac[,7]=(feblo$rech)/(feblo$rech+deltas+feblo$et)
lines(rechfrac[,4],type="l",lty=1,col="blue")

deltas=c(0,diff(marlo$stor))
rechfrac[,8]=(marlo$rech)/(marlo$rech+deltas+marlo$et)
lines(rechfrac[,5],type="l",lty=1,col="green")


abovezero=apply(rechfrac[,2:8] > 0,MARGIN=2,FUN=which)
mean(rechfrac[abovezero$control,2])
mean(rechfrac[abovezero$janhi,3])
mean(rechfrac[abovezero$febhi,4])
mean(rechfrac[abovezero$marhi,5])
mean(rechfrac[abovezero$janlo,6])
mean(rechfrac[abovezero$feblo,7])
mean(rechfrac[abovezero$marlo,8])

par(mfrow=(c(6,1)))
plot(rechfrac[abovezero$janhi,3])
plot(rechfrac[abovezero$febhi,4])
plot(rechfrac[abovezero$marhi,5])
plot(rechfrac[abovezero$janlo,6])
plot(rechfrac[abovezero$feblo,7])
plot(rechfrac[abovezero$marlo,8])


mean(c(rechfrac[abovezero$janhi[1],3],rechfrac[abovezero$febhi[1],4],rechfrac[abovezero$marhi[1],5],rechfrac[abovezero$janlo[1],6],rechfrac[abovezero$feblo[1],7],rechfrac[abovezero$marlo[1],8]))

