#water balance data processing
wabala=read.csv("water_balance.csv")

#just shallow tensiometers

foo=wabala[(wabala$tensio %in% c(1,2,5,6,9,10,13,14)),]

events=read.csv("ct_alfalfa_events.csv") 

foo_stor=which(foo$pool == "Storage")
foo_et=which(foo$pool == "ET")
foo_re=which(foo$pool == "Recharge")

plot(yday(strptime(events$Date,format="%d-%b")),foo$fraction[foo$stor])

sum(c(mean(foo$fraction[foo_stor]),mean(foo$fraction[foo_et]),mean(foo$fraction[foo_re])))

plot(foo$fraction[foo_stor]~foo$tensio[foo_stor])
plot(foo$fraction[foo_et]~foo$tensio[foo_et])
plot(foo$fraction[foo_re]~foo$tensio[foo_re])

applied_volume=which(events$gallons > 0)

applied_gallons=events$gallons[applied_volume]

plot(foo$fraction[foo_stor]~foo$event[foo_stor])
plot(foo$fraction[foo_et]~foo$event[foo_et])
plot(foo$fraction[foo_re]~foo$event[foo_re])

vol_map=data.frame(eid=1:length(events$gallons),vol=events$gallons)
volumes_recharged=vol_map[foo$event[foo_re],]$vol*foo$fraction[foo_re]/7.48/1000
volumes_stored=vol_map[foo$event[foo_stor],]$vol*foo$fraction[foo_stor]/7.48/1000
plot(volumes_recharged~foo$event[foo_re], ylab="Feet of water")
plot(volumes_stored~foo$event[foo_stor], ylab="Feet of water")

len_applied=vol_map[foo$event[foo_re],]$vol/7.48/1000
plot(volumes_recharged~)

foo=wabala[!(wabala$tensio %in% c(1,2,5,6,9,10,13,14)),]

events=read.csv("ct_alfalfa_events.csv") 

foo_stor=which(foo$pool == "Storage")
foo_et=which(foo$pool == "ET")
foo_re=which(foo$pool == "Recharge")

sum(c(mean(foo$fraction[foo_stor]),mean(foo$fraction[foo_et]),mean(foo$fraction[foo_re])))

plot(foo$fraction[foo_stor]~foo$tensio[foo_stor])
plot(foo$fraction[foo_et]~foo$tensio[foo_et])
plot(foo$fraction[foo_re]~foo$tensio[foo_re])

applied_volume=which(events$gallons > 0)

applied_gallons=events$gallons[applied_volume]

plot(foo$fraction[foo_stor]~foo$event[foo_stor])
plot(foo$fraction[foo_et]~foo$event[foo_et])
plot(foo$fraction[foo_re]~foo$event[foo_re])

vol_map=data.frame(eid=1:length(events$gallons),vol=events$gallons)
volumes_recharged=vol_map[foo$event[foo_re],]$vol*foo$fraction[foo_re]/7.48/1000
volumes_stored2=vol_map[foo$event[foo_stor],]$vol*foo$fraction[foo_stor]/7.48/1000
plot(volumes_recharged~foo$event[foo_re], ylab="Feet of water")
plot(volumes_stored2~foo$event[foo_stor], ylab="Feet of water")


depth_ratio=volumes_stored/volumes_stored2
depth_ratio[depth_ratio<0]=NA
plot(depth_ratio~foo$event[foo_stor])
mean(na.omit(depth_ratio))

