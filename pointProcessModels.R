#################################
###### Point process models #####
#################################
library(spatstat)
library(RPostgreSQL)
library(sp)
library(adehabitatHR)
library(maptools)
library(raster)

memory.limit(1e6)

#######################
##### Set up data #####
#######################
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'diadema_pilot_2018_all_data', host = 'localhost', port = 5432,
								 user = 'postgres', password = 'Animalbehavior1#')
listScans	<- dbGetQuery(con, 'select * from main_tables.list_scans;')
ffFull		<- dbGetQuery(con, "select * from main_tables.list_behaviors
				left join main_tables.trees
				on main_tables.list_behaviors.tree_number_final = main_tables.trees.tree_name
				where behavior ='Feeding' and start_stop = 'Start';")
grmFull		<- dbGetQuery(con, "select * from main_tables.list_behaviors
				left join main_tables.trees
				on main_tables.list_behaviors.tree_number_final = main_tables.trees.tree_name
				where behavior ='Groom' and start_stop = 'Start';")
grm			<- grmFull[,c(1:5, 9, 14, 22:25, 30, 33, 36, 38:45, 49:50, 52, 54:55)]
ff			<- ffFull[,c(1:5, 9, 14, 22:25, 30, 33, 36, 38:45, 49:50, 52, 54:55)]
grm$DyadID	<- NA
for(i in 1:dim(grm)[1]){
	actor			<- grm[i,]$actor
	recip			<- grm[i,]$subject
	grm[i,]$DyadID	<- paste(sort(c(actor, recip))[1], sort(c(actor, recip))[2], sep = '')
}	

grmSub		<- grm[!duplicated(grm[,c('focal_start_time', 'DyadID', 'latitude', 'longitude')]),]
ffSub			<- ff[!duplicated(ff[,c('focal_start_time', 'latitude', 'longitude')]),]

#Filter NA's and 0's
hrData2018	<- listScans[listScans$longitude > 0 & is.na(listScans$longitude) == FALSE,]
ffData2018	<- ffSub[is.na(ffSub$latUTM) == FALSE,]
grmData2018	<- grmSub[is.na(grmSub$longUTM) == FALSE,]

hrData2018SP	<- SpatialPointsDataFrame(hrData2018[,c('longitude', 'latitude')], hrData2018, proj4string=CRS("+proj=longlat +datum=WGS84"))
hrData2018UTM	<- spTransform(hrData2018SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

ffData2018UTM	<- SpatialPointsDataFrame(ffData2018[,c('longUTM', 'latUTM')], ffData2018, proj4string=CRS("+proj=utm +zone=38 +south +datum=WGS84"))

grmData2018UTM	<- SpatialPointsDataFrame(grmData2018[,c('longUTM', 'latUTM')], grmData2018, proj4string=CRS("+proj=utm +zone=38 +south +datum=WGS84"))

#######################
##### Covariates ######
#######################
setwd('D:/Google Drive/Graduate School/Research/Projects/SpatialClustering/MF/Analysis/SpatialAnalysisClustering')
slope		<- raster('slopeRaster.TIF')
slopeClip		<- crop(slope, extent(hrData2018SP))

projected_slope	<- projectRaster(slope, crs = "+proj=utm +zone=38 +south +datum=WGS84")
slope_trans		<- shift(projected_slope, x = -864749, y = -7897710)

########################
##### Translations #####
########################
#Need to translate (864749, 7897710) to (0, 0)
ffData2018UTM$transLatUTM	<- ffData2018UTM$latUTM - 7897710
grmData2018UTM$transLatUTM	<- grmData2018UTM$latUTM - 7897710
ffData2018UTM$transLongUTM	<- ffData2018UTM$longUTM - 864749
grmData2018UTM$transLongUTM	<- grmData2018UTM$longUTM - 864749
hrData2018UTM$transLatUTM	<- hrData2018UTM$latitude - 7897710
hrData2018UTM$transLongUTM	<- hrData2018UTM$longitude - 864749

allPoints		<- rbind(hrData2018UTM[,1], ffData2018UTM[,1], grmData2018UTM[,1])
allPoints		<- data.frame(allPoints)
allPoints$transLong	<- allPoints$longitude - 864749
allPoints$transLat	<- allPoints$latitude - 7897710
allPointsTrans	<- SpatialPointsDataFrame(coords = allPoints[,5:6], data = allPoints)
hrDataTrans		<- SpatialPointsDataFrame(coords = data.frame(hrData2018UTM)[,c('transLongUTM', 'transLatUTM')], data = data.frame(hrData2018UTM))
ffDataTrans		<- SpatialPointsDataFrame(coords = data.frame(ffData2018UTM)[,c('transLongUTM', 'transLatUTM')], data = data.frame(ffData2018UTM))
grmDataTrans	<- SpatialPointsDataFrame(coords = data.frame(grmData2018UTM)[,c('transLongUTM', 'transLatUTM')], data = data.frame(grmData2018UTM))

mcpHR			<- mcp(allPointsTrans[,1], percent = 100)
minXs			<- c(grmDataTrans@bbox[1], ffDataTrans@bbox[1], hrDataTrans@bbox[1])
maxXs			<- c(grmDataTrans@bbox[3], ffDataTrans@bbox[3], hrDataTrans@bbox[3])
minYs			<- c(grmDataTrans@bbox[2], ffDataTrans@bbox[2], hrDataTrans@bbox[2])
maxYs			<- c(grmDataTrans@bbox[4], ffDataTrans@bbox[4], hrDataTrans@bbox[4])
maxWin		<- as.owin(c(min(minXs), max(maxXs), min(minYs), max(maxYs)))
mcpWin		<- as.owin(mcpHR)

mcpHRUT		<- mcp(allPoints[,1], percent = 100)
minXsUT		<- c(grmData2018UTM@bbox[1], ffData2018UTM@bbox[1], hrData2018UTM@bbox[1])
maxXsUT		<- c(grmData2018UTM@bbox[3], ffData2018UTM@bbox[3], hrData2018UTM@bbox[3])
minYsUT		<- c(grmData2018UTM@bbox[2], ffData2018UTM@bbox[2], hrData2018UTM@bbox[2])
maxYsUT		<- c(grmData2018UTM@bbox[4], ffData2018UTM@bbox[4], hrData2018UTM@bbox[4])
maxWinUT		<- as.owin(c(min(minXsUT), max(maxXsUT), min(minYsUT), max(maxYsUT)))
mcpWinUT		<- as.owin(mcpHRUT)

#####################################
##### Convert to spatial points #####
#####################################
hrppp		<- ppp(hrDataTrans$transLongUTM, hrDataTrans$transLatUTM, window = mcpWin)
ffppp		<- ppp(ffDataTrans$transLongUTM, ffDataTrans$transLatUTM, window = mcpWin)
grmppp	<- ppp(grmDataTrans$transLongUTM, grmDataTrans$transLatUTM, window = mcpWin)

hrpppNT	<- ppp(hrData2018UTM$longitude, hrData2018UTM$latitude, window = mcpWinUT)
ffpppNT	<- ppp(ffData2018UTM$longitude, ffData2018UTM$latitude, window = mcpWinUT)
grmpppNT	<- ppp(grmData2018UTM$longitude, grmData2018UTM$latitude, window = mcpWinUT)


#######################
##### Basic plots #####
#######################
plot(density(hrppp, 25))
plot(grmppp, add = TRUE, col = 'white', pch = 16)
plot(density(ffppp, 20))
par(mfrow = c(1, 1))
plot(density(grmppp, 25))
plot(grmppp, add = TRUE, col = 'white', pch = 16)
grm2	<- grm[order(grm$behavior_time),]
#grm2$transLong	<- grm2$longUTM - 864749
#grm2$transLat	<- grm2$latUTM - 7897710
grm2$transLong	<- grm2$longUTM
grm2$transLat	<- grm2$latUTM
points(transLat~transLong, data=grm2[!is.na(grm2$transLat),], type = 'o')

#####################
##### Modelling #####
#####################
#HR
model1	<- ppm(hrppp, ~1) #Density is significantly different from 0
model2	<- ppm(hrppp, ~x+y) #Can't compute variance, but the x and y effects are very tiny

#ff
model3	<- ppm(ffppp, ~1) #Same story as HR
model4	<- ppm(ffppp, ~x+y) #Same story as HR

#grm
model5	<- ppm(grmppp, ~1) #Same story as HR
slope_image	<- as.im(projected_slope)
model6	<- ppm(grmppp, ~ff + hr, covariates = list(ff = density(ffppp), hr = density(hrppp)))
 #Need to add the elevation covariates
 #Grm where you already use areas (not sure if we have specific hotspots - still need to deal w/ temporal autocorrelation
 #Hr is a better predictor than ff, but estimates are huge

par(mfrow = c(1,3))
plot(density(hrppp))
plot(density(grmppp))
plot(predict(model6))