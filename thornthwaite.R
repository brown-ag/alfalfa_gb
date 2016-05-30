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

ct_budget_all=data.frame(year=numeric(),day=numeric(), et=numeric(), stor=numeric(), rech=numeric(), irrig=numeric())
for(i in 1:7) {
  #clear irrigation data buffer and then populate with treatment info
  irrig=rep(0,366) #create buffer for irrigation quantities
  irrig[idayz] = events[,i+1] #insert irrigation into buffer
  
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
  ct_budget=data.frame(exp=i,year=ctract$year,day=ctract$doy,et=eta,stor=soilc,rech=deep)
  ct_budget_all=rbind(ct_budget_all,ct_budget)
}

#soil storage plot by treatment
control=ct_budget_all[which(ct_budget_all$exp==7),]
plot(control$stor,type="l",lty=1,ylim=c(0,400))
janhi=ct_budget_all[which(ct_budget_all$exp==2),]
lines(janhi$stor,type="l",lty=1,col="red")
febhi=ct_budget_all[which(ct_budget_all$exp==4),]
lines(febhi$stor,type="l",lty=1,col="blue")
marhi=ct_budget_all[which(ct_budget_all$exp==6),]
lines(marhi$stor,type="l",lty=1,col="green")
janlo=ct_budget_all[which(ct_budget_all$exp==1),]
lines(janlo$stor,type="l",lty=2,col="red")
feblo=ct_budget_all[which(ct_budget_all$exp==3),]
lines(feblo$stor,type="l",lty=2,col="blue")
marlo=ct_budget_all[which(ct_budget_all$exp==5),]
lines(marlo$stor,type="l",lty=2,col="green")

plot(cumsum(control$rech),type="l",lty=1,ylim=c(0,3000))
lines(cumsum(janhi$rech),type="l",lty=1,col="red")
lines(cumsum(febhi$rech),type="l",lty=1,col="blue")
lines(cumsum(marhi$rech),type="l",lty=1,col="green")
lines(cumsum(janlo$rech),type="l",lty=2,col="red")
lines(cumsum(feblo$rech),type="l",lty=2,col="blue")
lines(cumsum(marlo$rech),type="l",lty=2,col="green")
