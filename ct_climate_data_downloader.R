#downloads campbell tract climate data

library(XML)
library(utils)
base='http://apps.atm.ucdavis.edu/wxdata/data/'
doc.html = htmlTreeParse(base,useInternal = TRUE)

doc.text = unlist(xpathApply(doc.html, '//a', xmlValue))
zipfiles=grepl("zip",doc.text)

dir.create("CT_data")

for(u in doc.text[zipfiles]) {
  url=paste(base,u,sep="")
  local=paste("CT_Data\\",u,sep="")
  download.file(url, local)
  unzip(local)
}
