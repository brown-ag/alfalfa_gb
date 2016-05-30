#temperature grownig degree days scott valley
library(lubridate)
df=read.csv("SV_TEMP_ALL.csv")

fives=((1:ncol(df))[((1:ncol(df) %% 4)==2)]) #gets all columns with data for 5cm soil depth
#fives=((1:ncol(df))[((1:ncol(df) %% 4)==0)])#25cm

dailyFun = function(data,datecol=4,paramcol=3,FUNC=mean) {
  date <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 1)
  time <- sapply(strsplit(as.character(data[,datecol]), " "), "[", 2)
  return(aggregate(data[,paramcol], by=list(date), FUN=FUNC))
}

monthlyFun = function(data,datecol=4,paramcol=3,FUNC=mean) {
  daily=dailyFun(data,datecol,paramcol,FUNC=max)  
  date <- sapply(strsplit(as.character(daily[,1]), "-"),"[",2)
  return(aggregate(daily[,2], by=list(date), FUN=FUNC))
}

dfday=data.frame(loc=numeric(),day=numeric(),meen=numeric(),maxx=numeric(),minn=numeric())
dfgdd=data.frame(loc=numeric(),gdd=numeric())
dfgdda=data.frame(loc=numeric(),day=numeric(),gdd=numeric())
for(fi in 1:length(fives)) {
  
  dfs=(df[,c(1,fives[fi])])
  dfs[is.na(dfs[,2]),2]=0
  mx=dailyFun(data=dfs,datecol=1,paramcol=2,FUNC=max)
  mn=dailyFun(data=dfs,datecol=1,paramcol=2,FUNC=min)
  me=dailyFun(data=dfs,datecol=1,paramcol=2)
  subr=data.frame(loc=fi,day=me[,1],meen=me,maxx=mx,minn=mn)
  dfday=rbind(dfday,subr)
  #me
  #me[,2]=(mx[,2]+mn[,2])/2
  #me=dfs
  gddme=sum((me[me[,2]>5,2]-5))
  
  gdda=(me[,2]-5)
  gdda[gdda<0]=0
  
  dfgdda=rbind(dfgdda,data.frame(loc=rep(fi,length(me[,1])),day=me[,1],gdd=gdda))
  
  dfgdd=rbind(dfgdd,data.frame(loc=fi,gdd=gddme))
}
gddme

loc1=(dfgdda[dfgdda$loc==1,])
loc1
plot(loc1[,2],cumsum(loc1[,3]),type="l",ylim=c(0,300),col="blue")
for(ii in 2:11) {
  loci=(dfgdda[dfgdda$loc==ii,])
  lines(loci[,2],cumsum(loci[,3]),col=ii)
}


dfgdd=dfgdd[1:11,]
dfgdd

library(sp)
library(maptools)

#geoloc=readShapePoints("gis\\jm_geoprobes.shp",proj4string = CRS("+proj=longlat +datum=WGS84"))
geoloc=readShapePoints("gis\\jm_geoprobes.shp",proj4string = CRS("+proj=utm +zone=10N +ellps=WGS84")
#foo=data.frame(lat=geoloc$Latitude,lng=geoloc$Longitude,ID=as.numeric(t((data.frame(strsplit(as.character(geoloc$Comment),"JM")))[2,])[,1]))[2:13,]
#coordinates(foo) = ~lng+lat
#proj4string(foo) = CRS("+proj=longlat +datum=WGS84")
#geoloc <- spTransform(foo, CRS("+proj=utm +zone=10N +ellps=WGS84"))
#writePointsShape(geoloc,"gis\\jm_geoprobes")
geoloc=geoloc[1:11,]

locs=read.csv("sv_bio_locations.csv")
bios=read.csv("27-05-15_SV_bio.csv")
bio2=read.csv("15-7-15_SV_bio_Rinput.csv")

data.shape<-readShapePoly("gis\\jm_field.shp",IDvar="Check_Num",CRS("+proj=utm +zone=10N +ellps=WGS84"))
r=raster(data.shape)
res(r)=10

bioloc=merge(locs,bios,by.x="subplot",by.y = "Plot" )
df=SpatialPointsDataFrame(bioloc[,2:3],bioloc,proj4string=CRS("+proj=longlat +datum=WGS84"))
df <- spTransform(df, CRS("+proj=utm +zone=10N +ellps=WGS84"))
df$water=rep(c(0,6,10,29),each=8)
df$bio=df$AlfBag+df$WeedBag
df$bio2=bio2$bio*2*0.0040468564
df$bio=df$bio*2*0.0040468564

geoloc$gdd[c(dfgdd$loc)]=c(dfgdd$gdd)

m=Tps(coordinates(geoloc),geoloc$gdd)
tps=interpolate(r,m)
gddloc=extract(tps,df)

tpst=extract(tps,geoloc)


spplot(tps, sp.layout = list("sp.points", df, pch = 16, cex = 0.3, col = "white"))

gs=gstat(formula=gdd~1,locations=geoloc)
g <- as(r, 'SpatialGrid')
kp=predict(gs,g)
spplot(kp,"var1.pred", sp.layout = list("sp.points", df, pch = 16, cex = 0.1, col = "black"))

head(df)
#df[32,]=NA
#df=na.omit(df)
gddloc=predict(gs,df)
gddloc=gddloc$var1.pred
m0=(lm(data=df,bio2~lat+lng+ipc+ppc+X.Cover+WeedBag+bio+gddloc+water))
om=stepAIC(m0)
om2=lm(data=df,bio2~water)
plot(om)
vif(om)

summary(om)

plot(data=df,gddloc~water)
plot(data=df,lat~water)
plot(data=df,gddloc~lat)

plot(df$bio2,predict(om2,df),xlab="Observed",ylab="Predicted")
abline(0,1)
library(spdep)
nb_3nn=knn2nb(knearneigh(cbind(df$lng,df$lat),k=3))
moran.mc(resid(m0),listw=nb2listw(nb_3nn),nsim=9999)

#try to predict distance from valve using first cutting data
m0=lm(data=df,bio~lng)

summary(m0)
m1=stepAIC(m0)

plot(df$bio~predict(m0,df),ylim=c(0,3),xlim=c(0,3))

