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
