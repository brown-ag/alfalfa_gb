library(gstat)
library(sp)
library(maptools)
library(raster)

#scott valley biomass krige
locs=read.csv("sv_bio_locations.csv")
bios=read.csv("27-05-15_SV_bio.csv")
bio2=read.csv("15-7-15_SV_bio_Rinput.csv")
data.shape<-readShapePoly("gis\\jm_field.shp",IDvar="Check_Num",CRS("+proj=utm +zone=10N +ellps=WGS84"))
data.shape
writePolyShape(data.shape,"gis\\jm_field")
?readSh

bioloc=merge(locs,bios,by.x="subplot",by.y = "Plot" )
df=SpatialPointsDataFrame(bioloc[,2:3],bioloc,proj4string=CRS("+proj=longlat +datum=WGS84"))
df <- spTransform(df, CRS("+proj=utm +zone=10N +ellps=WGS84"))
df$water=rep(c(0,6,10,29),each=8)
df$bio=df$AlfBag+df$WeedBag
df$bio2=bio2$bio*2*0.0040468564
df$bio=df$bio*2*0.0040468564

r=raster(data.shape)
res(r)=1
g <- as(r, 'SpatialGrid')
df
gs=gstat(formula=bio~ipc,locations=df)
gs2=gstat(formula=bio2~ipc,locations=df)
v=variogram(gs,width=3)
v2=variogram(gs2,width=3)
plot(v)
plot(v2)
kp=predict(gs,g)
kp2=predict(gs2,g)
ip=interpolate(r,gs)
spplot(kp,"var1.pred", sp.layout = list("sp.points", df, pch = 16, cex = 0.1, col = "black"))
spplot(kp2,"var1.pred", sp.layout = list("sp.points", df, pch = 16, cex = 0.1, col = "black"))
summary(kp)
head(g)
latitude=coordinates(df)[,2]
longitude=coordinates(df)[,1]
mbio1=lm(data=df,bio~water+latitude+longitude)
mbio2=lm(data=df,bio2~water+latitude+longitude)
library(MASS)
library(car)
vif(mbio1)
vif(mbio2)
obio1=stepAIC(mbio1)
obio2=stepAIC(mbio2)
vif(obio1)
vif(obio2)
summary(obio2)

cor(df$water,latitude)
plot(df$water,latitude)
summary(obio1)
summary(obio2)
plot(obio2)

#Thin plate spline
library(fields)
m=Tps(coordinates(df),df$ipc)
tps=interpolate(r,m)
interppc=extract(tps,df)

md=lm(bio2~interppc+latitude+longitude+dmap[which(foob2$cut==2)]+water,data=df)
summary(md)
omd=stepAIC(md)
summary(omd)
dmap
df$water

m2=Tps(coordinates(df),data.frame(predict(omd)))
tps2=interpolate(r,m2)

spplot(tps, sp.layout = list("sp.points", df, pch = 16, cex = 0.3, col = "white"))
spplot(tps2, sp.layout = list("sp.points", df, pch = 16, cex = 0.3, col = "white"))

