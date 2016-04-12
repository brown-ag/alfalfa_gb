#downloads campbell tract climate data

library(XML)
library(utils)
base='http://apps.atm.ucdavis.edu/wxdata/data/'
doc.html = htmlTreeParse(base,useInternal = TRUE)

doc.text = unlist(xpathApply(doc.html, '//a', xmlValue))
zipfiles=grepl("zip",doc.text)

dir.create("ClimateData")

for(u in doc.text[zipfiles]) {
  url=paste(base,u,sep="")
  local=paste("ClimateData\\",u,sep="")
  download.file(url, local)
  unzip(local)
}


origin_time="2015-01-1 0:00"
origin=as.POSIXct(strptime(origin_time,"%Y-%m-%d %H:%M")) #initial time for series

for(i in gsub("zip","csv",doc.text[zipfiles])) {
  foo=read.csv(i,stringsAsFactors=FALSE)
  TIMESTAMP = as.POSIXct(strptime(foo[,4],"%Y-%m-%d %H:%M"))
  keep = ((as.numeric(TIMESTAMP)-as.numeric(origin) > 0))
  write.csv(na.omit(foo[keep,]),file=paste("ClimateData\\alf",i,sep=""))
}

