# Load libraries
library(sp)
library(maptools)
library(snippets)
library(rgdal)
library(rgeos)

OUTDIR = "../img/ch07/"
dir.create(OUTDIR, FALSE, TRUE)
options(width=70)

#############################################
# Berlin border crossing
berlin = read.csv("../data/ch07/berlinBorderCrossing.csv", as.is=TRUE)
cityCenterId = c(1:6,8)

pdf(paste0(OUTDIR, "berlinBorderCrossingPlain.pdf"), 8, 4)
par(mar=c(0,0,0,0))
plot(x=berlin$Longitude[cityCenterId],
     y=berlin$Latitude[cityCenterId],
     pch=19,
     axes=FALSE,
     xlim=range(berlin$Longitude[cityCenterId]) + c(0,0.015))
text(x=berlin$Longitude[cityCenterId],
     y=berlin$Latitude[cityCenterId],
     labels=berlin$Crossing[cityCenterId],
     pos=4,
     col="blue")
box()
dev.off()

pdf(paste0(OUTDIR, "berlinBorderCrossingOsmNoAsp.pdf"), 8, 4)
par(mar=c(0,0,0,0))
plot(x=berlin$Longitude[cityCenterId],
     y=berlin$Latitude[cityCenterId],
     pch=19,
     axes=FALSE,
     xlim=range(berlin$Longitude[cityCenterId]) + c(0,0.015))
osmap()
points(x=berlin$Longitude[cityCenterId],
       y=berlin$Latitude[cityCenterId],
       pch=19)
text(x=berlin$Longitude[cityCenterId],
     y=berlin$Latitude[cityCenterId],
     labels=berlin$Crossing[cityCenterId],
     pos=4,
     col="blue")
box()
dev.off()

pdf(paste0(OUTDIR, "berlinBorderCrossingOsmAspSmall.pdf"), 8, 4)
par(mar=c(0,0,0,0))
plot(x=berlin$Longitude[cityCenterId],
     y=berlin$Latitude[cityCenterId],
     pch=19,
     axes=FALSE,
     asp=1/abs(cos(berlin$Latitude[1]*pi/180)),
     xlim=range(berlin$Longitude[cityCenterId]) + c(0,0.015))
osmap()
points(x=berlin$Longitude[cityCenterId],
       y=berlin$Latitude[cityCenterId],
       pch=19)
text(x=berlin$Longitude[cityCenterId],
     y=berlin$Latitude[cityCenterId],
     label=berlin$Crossing[cityCenterId],
     pos=4,
     col="blue")
box()
dev.off()

pdf(paste0(OUTDIR, "berlinBorderCrossingOsmAspSmallTN.pdf"), 8, 4)
par(mar=c(0,0,0,0))
plot(x=berlin$Longitude[cityCenterId],
     y=berlin$Latitude[cityCenterId],
     pch=19,
     axes=FALSE,
     asp=1/abs(cos(berlin$Latitude[1]*pi/180)),
     xlim=range(berlin$Longitude[cityCenterId]) + c(0,0.015))
osmap(tiles.url="http://c.tile.stamen.com/toner/")
points(x=berlin$Longitude[cityCenterId],
       y=berlin$Latitude[cityCenterId],
       pch=19)
text(x=berlin$Longitude[cityCenterId],
     y=berlin$Latitude[cityCenterId],
     label=berlin$Crossing[cityCenterId],
     pos=4,
     col="blue")
box()
dev.off()

pdf(paste0(OUTDIR, "berlinBorderCrossingOsmAspBig.pdf"), 8, 4)
par(mar=c(0,0,0,0))
plot(x=berlin$Longitude,
     y=berlin$Latitude,
     pch=19,
     axes=FALSE,
     asp=1/abs(cos(berlin$Latitude[1]*pi/180)),
     xlim=range(berlin$Longitude) + c(0,0.1))
osmap(tiles.url="http://c.tile.stamen.com/toner/")
points(x=berlin$Longitude,
       y=berlin$Latitude,
       pch=19)
text(x=berlin$Longitude,
     y=berlin$Latitude,
     label=berlin$Crossing,
     pos=4,
     col="blue")
box()
dev.off()

#############################################
# Read in state file and create four simple plots
projectionObj = CRS(projargs="+proj=longlat")
state = readShapeSpatial(fn="../data/ch07/State_2010Census_DP1/State_2010Census_DP1",
  proj4string=projectionObj)

state = state[-which(state@data$STUSPS10 %in% c("AK", "HI")),]

pdf(paste0(OUTDIR, "flatStateMap.pdf"), 8, 4)
par(mar=c(0,0,0,0))
plot(state)
dev.off()

stateTrans = spTransform(x=state, CRSobj=CRS("+proj=utm +zone=14"))


pdf(paste0(OUTDIR, "transformedStateMap.pdf"), 8, 4)
par(mar=c(0,0,0,0))
plot(stateTrans)
dev.off()

centroid = gCentroid(spgeom=stateTrans, byid=TRUE)

pdf(paste0(OUTDIR, "namesStateMap.pdf"), 8, 4)
par(mar=c(0,0,0,0))
plot(stateTrans)
text(x=centroid$x, y=centroid$y,
     label=stateTrans@data$NAME10, cex=0.7)
dev.off()

percHouseRec = stateTrans@data$DP0180008 / stateTrans@data$DP0180001
bins = quantile(percHouseRec, seq(0,1,length.out=8))
binId = findInterval(percHouseRec, bins)
densityVals = seq_len(length(bins)) * 5

pdf(paste0(OUTDIR, "densityStateMap.pdf"), 8, 4)
par(mar=c(0,0,0,0))
plot(stateTrans, density=densityVals[binId])
dev.off()

#############################################
# Read in photogrammar data and make a simple
z = read.csv(file="../data/ch07/photoDatasetAllRaw.csv", as.is=TRUE)
z = z[,c("cnumber2","pname","year","longitude","latitude")]
names(z)[1] = "cnumber"
z = z[!is.na(z$latitude) & !is.na(z$longitude) & !is.na(z$pname),]
pnameSet = names(sort(table(z$pname),TRUE))[1:20]
z = z[z$pname %in% pnameSet,]
head(z)

pts = SpatialPointsDataFrame(cbind(z$longitude, z$latitude), z)
cnty = readShapeSpatial("../data/ch07/County_2010Census_DP1/County_2010Census_DP1")

joinedDataF = pts %over% cnty

joinedDataF$popDen = joinedDataF$DP0010001 / joinedDataF$ALAND10 *1000^2
medianPerc = tapply(joinedDataF$popDen, z$pname, median, na.rm=TRUE)
index = order(medianPerc)
joinedDataF$pnameFactor = factor(z$pname, levels=names(medianPerc)[index])

pdf(paste0(OUTDIR, "popDenByPhotographer.pdf"), 8, 5)
par(mar=c(5.1,10,4.1,2.1))
boxplot(log(popDen) ~ pnameFactor, data=joinedDataF, horizontal=TRUE, las=1, outline=FALSE,
        col="grey", main="Boxplot of Photographer by County Demographics",
        xlab="Population Density", axes=FALSE)
box()
axis(1, at=seq(-2,10,2), label=round(exp(seq(-2,10,2))))
axis(2,at=1:20,label=levels(joinedDataF$pnameFactor),las=1)
dev.off()

# Aggregate photogrammar counts over counties; look at different ways
# to plot this; smooth?
projection = CRS("+proj=longlat")
z = read.csv("../data/ch07/photoDatasetAllRaw.csv", as.is=TRUE)
z = z[!is.na(z$latitude) & !is.na(z$longitude),]
pts = SpatialPointsDataFrame(cbind(z$longitude, z$latitude), z, proj4string=projection)
cnty = readShapeSpatial("../data/ch07/County_2010Census_DP1/County_2010Census_DP1", proj4string=projection)
state = readShapeSpatial("../data/ch07/State_2010Census_DP1/State_2010Census_DP1", proj4string=projection)
centroidC = gCentroid(cnty, byid=TRUE)
centroidS = gCentroid(state, byid=TRUE)
state = state[centroidS$x > -79.3 & centroidS$x < -71 & centroidS$y > 37 & centroidS$y < 44,]
cnty$GEOID10 = as.character(cnty$GEOID10)

matchIndex = cnty %over% state
cnty = cnty[!is.na(matchIndex$GEOID10),]

x = over(pts,cnty)
tab = table(x$GEOID10)

index = match(as.character(cnty$GEOID10), names(tab))

cnty$photoCount = 0
cnty$photoCount[!is.na(index)] = tab[index[!is.na(index)]]

var = cnty$photoCount
bins = unique(quantile(var, seq(0,1,length.out=30)))
cnty$binId01 = findInterval(var, bins)
colSet01 = rev(heat.colors(length(bins)))

var = cnty$photoCount / cnty$DP0010001
bins = unique(quantile(var, seq(0,1,length.out=30)))
cnty$binId02 = findInterval(var, bins)
colSet02 = rev(heat.colors(length(bins)))

var = cnty$photoCount / cnty$Shape_Area
bins = unique(quantile(var, seq(0,1,length.out=30)))
cnty$binId03 = findInterval(var, bins)
colSet03 = rev(heat.colors(length(bins)))

colSet = heat.colors(length(bins))
cntyTrans = spTransform(cnty, CRS("+proj=utm +zone=14"))
stateTrans = spTransform(state, CRS("+proj=utm +zone=14"))

coords = cbind(longitude=c(-71.0589,-74.0059,-77.0164),
               latitude=c(42.3601,40.7127,38.9047))
cities = SpatialPointsDataFrame(coords=coords,
          data=data.frame(city=c("Boston", "NYC", "Washington, DC")),
          proj4string=projection)
citiesTrans = spTransform(cities, CRS("+proj=utm +zone=14"))
citiesTransCoords = coordinates(citiesTrans)

png(paste0(OUTDIR, "photogrammerChoroplethRaw.png"), width = 850, height = 850)
par(mar=c(0,0,0,0))
plot(cntyTrans, col=colSet01[cntyTrans$binId01], border=NA)
plot(stateTrans, add=TRUE)
points(x=citiesTransCoords[,1],
       y=citiesTransCoords[,2],
       pch=19, cex=2)
text(x=citiesTransCoords[,1],
     y=citiesTransCoords[,2],
     label=citiesTrans$city,
     pos=4, cex=2)
dev.off()

png(paste0(OUTDIR, "photogrammerChoroplethPop.png"), width = 850, height = 850)
par(mar=c(0,0,0,0))
z = plot(cntyTrans, col=colSet02[cntyTrans$binId02], border=NA)
plot(stateTrans, add=TRUE)
points(x=citiesTransCoords[,1],
       y=citiesTransCoords[,2],
       pch=19, cex=2)
text(x=citiesTransCoords[,1],
     y=citiesTransCoords[,2],
     label=citiesTrans$city,
     pos=4, cex=2)
dev.off()



