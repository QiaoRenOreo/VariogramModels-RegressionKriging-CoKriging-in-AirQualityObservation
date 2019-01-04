### R code from vignette source 'D:/Dropbox/Teaching/M12/2016/Day5/M12_MappingExercise.rnw'

###################################################
### code chunk number 1: M12_MappingExercise.rnw:52-57
###################################################
library(gstat)  #The downloaded binary packages are in C:\Users\oreol\AppData\Local\Temp\RtmpkZkEJr\downloaded_packages
library(maptools) #The downloaded binary packages are in C:\Users\oreol\AppData\Local\Temp\RtmpkZkEJr\downloaded_packages
library(rgdal)  #The downloaded binary packages are in C:\Users\oreol\AppData\Local\Temp\RtmpkZkEJr\downloaded_packages
getwd()
setwd("D:/Study/Module12/5")
load("AirBasePM10_070409.RData")
head(abpm10)


###################################################
### code chunk number 2: M12_MappingExercise.rnw:61-63
###################################################
coordinates(abpm10) <- ~easting + northing
proj4string(abpm10) <- CRS("+init=epsg:3035 +units=km")


###################################################
### code chunk number 3: M12_MappingExercise.rnw:70-72
###################################################
plot(abpm10, axes=T)
bubble(abpm10, zcol="pm10.obs", scales=list(draw=T), main="Sensible title")


###################################################
### code chunk number 4: M12_MappingExercise.rnw:79-82
#variogram
###################################################
# abpm10.ev <- variogram(pm10.obs~1, data=abpm10)
abpm10.ev <- variogram(pm10.obs~1, data=abpm10, cutoff=1020, width=30)
plot(abpm10.ev)
abpm10.ev


###################################################
### code chunk number 5: M12_MappingExercise.rnw:89-92
###################################################
abpm10.mv <- fit.variogram(abpm10.ev,vgm(150, "Exp", 300, 50)) # it uses esponential model, with sill, nugget and range paprameters.
abpm10.mv
plot(abpm10.ev, model=abpm10.mv)
str(abpm10.mv) 

#abpm10.mv <- fit.variogram(abpm10.ev,vgm(150, "Spr", 300, 50)) # it uses esponential model, with sill, nugget and range paprameters.
#abpm10.mv
#plot(abpm10.ev, model=abpm10.mv)
#str(abpm10.mv)

###################################################
### code chunk number 6: M12_MappingExercise.rnw:98-105
### Use the modelled variogram for interpolation
# make changes on grid size: x= by=..., y= by=...
###################################################

# xy  <- expand.grid(x=seq(3800, 5000, by=2), y=seq(2500,3600, by=2))
xy  <- expand.grid(x=seq(3800, 5000, by=30), y=seq(2500,5000, by=30))
xys <- SpatialPoints(xy)
gridded(xys) <- TRUE
proj4string(xys) <- CRS("+init=epsg:3035 +units=km")
plot(xys, axes=T)
points(as.data.frame(abpm10)[,4:5], col=2, pch=19)
pmk <- krige(pm10.obs~1, abpm10, xys, model=abpm10.mv)


###################################################
### code chunk number 7: M12_MappingExercise.rnw:112-114
### Display a map with the interpolated values
###################################################
#plot Kringing Prediction
spplot(pmk, "var1.pred", sp.layout=list("sp.points", pch="+", abpm10), 
	scales=list(draw=TRUE), main="Kringing Prediction")
#var1.prediction

###################################################
### code chunk number 8: M12_MappingExercise.rnw:121-123
### Display a map with the interpolated values
###################################################
#plot Kringing Variance
spplot(pmk, "var1.var", sp.layout=list("sp.points", pch="+", abpm10), 
	scales=list(draw=TRUE), main="kringing Variance")


###################################################
### code chunk number 9: M12_MappingExercise.rnw:133-135
### read the shape file of country boundary 
###################################################
ceShape.shp <- readOGR("./ceShape/CentralEuropeShape.shp")
proj4string(ceShape.shp) <- CRS("+init=epsg:3035 +units=km")


###################################################
### code chunk number 10: M12_MappingExercise.rnw:140-144
### load the shape file of country boundary
###################################################
spl1 <- list("sp.polygons", ceShape.shp, first = FALSE) 
spl2 <- list("sp.points", pch="+", abpm10, col=2, cex=2)
spl <- list(spl1, spl2)
# spplot(pmk, "var1.pred", sp.layout = spl, scales=list(draw=TRUE), main="Kriged prediction")
spplot(pmk, "var1.var", sp.layout = spl, scales=list(draw=TRUE), main="Kriging variance")


###################################################
### code chunk number 11: M12_MappingExercise.rnw:151-152
### calculate the statistics for cross validation
###################################################
pmk.exp.cv = krige.cv(pm10.obs~1, abpm10, model=abpm10.mv)


###################################################
### code chunk number 12: M12_MappingExercise.rnw:155-156
### Plot the histogram of the residuals and examine this. 
### Calculate the mean error (ME) and RMSE (RMSE). 
###################################################
ME.sph.cv = sum(pmk.exp.cv$residual)/length(pmk.exp.cv$residual)

#find coord of max predicted value in kringed prediction.
max(pmk.exp.cv$var1.pred) #57.43498
max(pmk.exp.cv$var1.var) #192.4693
min(pmk.exp.cv$var1.var) #91.77019
write.table(pmk.exp.cv,"D:/Study/Module12/5/mydata.txt",sep="\t")

