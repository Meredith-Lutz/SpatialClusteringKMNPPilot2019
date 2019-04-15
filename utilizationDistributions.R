#########################################################################
###### Create utilization distn for HR, ff, and grm - 2016 and 2018 #####
#########################################################################

library(RPostgreSQL)
library(sp)
library(adehabitatHR)
library(maptools)
library(rgdal)
library(raster)
library(OpenStreetMap)
library(spatstat)
library(osmdata)

## Pull 2019 data
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'verreauxi_2019_all_data', host = 'localhost', port = 5432,
								 user = 'postgres', password = 'Animalbehavior1#')
listScans	<- dbGetQuery(con, 'select main_tables.list_focals.focal_start_time, scan_time, latitude, longitude, gps_horizontal_precision, altitude, main_tables.list_sessions.session_start_time, set_duration, focal_individual_id, group_id from main_tables.list_scans
		left join main_tables.list_focals
		on main_tables.list_scans.focal_start_time = main_tables.list_focals.focal_start_time
		left join main_tables.list_sessions
		on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;')
mgrm		<- dbGetQuery(con, "select main_tables.list_focals.focal_start_time, behavior_time, actor, subject, behavior, context, start_stop, latitude, longitude, gps_horizontal_precision, altitude, tree_number, height, main_tables.list_sessions.session_start_time, set_duration, focal_individual_id, group_id from main_tables.list_behaviors
			left join main_tables.list_focals
			on main_tables.list_behaviors.focal_start_time = main_tables.list_focals.focal_start_time
			left join main_tables.list_sessions
			on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time where behavior ='Mutual Groom' and start_stop = 'Start';")
grm		<- dbGetQuery(con, "select main_tables.list_focals.focal_start_time, behavior_time, actor, subject, behavior, context, start_stop, latitude, longitude, gps_horizontal_precision, altitude, tree_number, height, main_tables.list_sessions.session_start_time, set_duration, focal_individual_id, group_id from main_tables.list_behaviors
			left join main_tables.list_focals
			on main_tables.list_behaviors.focal_start_time = main_tables.list_focals.focal_start_time
			left join main_tables.list_sessions
			on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time where behavior ='Groom' and start_stop = 'Start';")
ply		<- dbGetQuery(con, "select main_tables.list_focals.focal_start_time, behavior_time, actor, subject, behavior, context, start_stop, latitude, longitude, gps_horizontal_precision, altitude, tree_number, height, main_tables.list_sessions.session_start_time, set_duration, focal_individual_id, group_id from main_tables.list_behaviors
			left join main_tables.list_focals
			on main_tables.list_behaviors.focal_start_time = main_tables.list_focals.focal_start_time
			left join main_tables.list_sessions
			on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time where behavior ='Play' and start_stop = 'Start';")
grt		<- dbGetQuery(con, "select main_tables.list_focals.focal_start_time, behavior_time, actor, subject, behavior, context, start_stop, latitude, longitude, gps_horizontal_precision, altitude, tree_number, height, main_tables.list_sessions.session_start_time, set_duration, focal_individual_id, group_id from main_tables.list_behaviors
			left join main_tables.list_focals
			on main_tables.list_behaviors.focal_start_time = main_tables.list_focals.focal_start_time
			left join main_tables.list_sessions
			on main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time where behavior ='Greet' and start_stop = 'Start';")
allAff		<- rbind(mgrm, grm, ply, grt)
listScans$species	<- 'sifaka'
grm$species		<- 'sifaka'
ply$species		<- 'sifaka'
allAff$species	<- 'sifaka'

#Filter NA's and 0's
hrData2019all	<- listScans[listScans$longitude > 0 & is.na(listScans$longitude) == FALSE,]
hrData2019		<- hrData2019all[hrData2019all$group_id == '2' | hrData2019all$group_id == '3' | hrData2019all$group_id == '6',]
hrData20196		<- hrData2019all[hrData2019all$group_id == '6',]
hrData20192		<- hrData2019all[hrData2019all$group_id == '2',]
hrData20193		<- hrData2019all[hrData2019all$group_id == '3',]

plyData2019all	<- ply[is.na(ply$longitude) == FALSE & ply$longitude > 0,]
plyData2019		<- plyData2019all[plyData2019all$group_id == '2' | plyData2019all$group_id == '3' | plyData2019all$group_id == '6',]
plyData20196	<- plyData2019all[plyData2019all$group_id == '6',]

affData2019all	<- allAff[is.na(allAff$longitude) == FALSE & allAff$longitude > 0,]
affData2019		<- affData2019all[affData2019all$group_id == '2' | affData2019all$group_id == '3' | affData2019all$group_id == '6',]
affData20196	<- affData2019all[affData2019all$group_id == '6',]
affData20192	<- affData2019all[affData2019all$group_id == '2',]
affData20193	<- affData2019all[affData2019all$group_id == '3',]

grmData2019all	<- grm[is.na(grm$longitude) == FALSE & grm$longitude > 0,]
grmData2019		<- grmData2019all[grmData2019all$group_id == '2' | grmData2019all$group_id == '3' | grmData2019all$group_id == '6',]
grmData20196	<- grmData2019all[grmData2019all$group_id == '6',]
grmData20192	<- grmData2019all[grmData2019all$group_id == '2',]
grmData20193	<- grmData2019all[grmData2019all$group_id == '3',]

plot(hrData2019$longitude, hrData2019$latitude, xlab = 'Longitude', ylab = 'Latitude', col = hrData2019$group_id)
plot(hrData20196$longitude, hrData20196$latitude, xlab = 'Longitude', ylab = 'Latitude', col = hrData20196$group_id)

#Convert to Spatial points object
hrData2019SP	<- SpatialPointsDataFrame(hrData2019[,c('longitude', 'latitude')], hrData2019, proj4string=CRS("+proj=longlat +datum=WGS84"))
hrData2019UTM	<- spTransform(hrData2019SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))
hrData2019SP2	<- SpatialPointsDataFrame(hrData20192[,c('longitude', 'latitude')], hrData20192, proj4string=CRS("+proj=longlat +datum=WGS84"))
hrData2019UTM2	<- spTransform(hrData2019SP2, CRS("+proj=utm +zone=38 +south +datum=WGS84"))
hrData2019SP3	<- SpatialPointsDataFrame(hrData20193[,c('longitude', 'latitude')], hrData20193, proj4string=CRS("+proj=longlat +datum=WGS84"))
hrData2019UTM3	<- spTransform(hrData2019SP3, CRS("+proj=utm +zone=38 +south +datum=WGS84"))
hrData2019SP6	<- SpatialPointsDataFrame(hrData20196[,c('longitude', 'latitude')], hrData20196, proj4string=CRS("+proj=longlat +datum=WGS84"))
hrData2019UTM6	<- spTransform(hrData2019SP6, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

plyData2019SP	<- SpatialPointsDataFrame(plyData2019[,c('longitude', 'latitude')], plyData2019, proj4string=CRS("+proj=longlat +datum=WGS84"))
plyData2019UTM	<- spTransform(plyData2019SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

grmData2019SP	<- SpatialPointsDataFrame(grmData2019[,c('longitude', 'latitude')], grmData2019, proj4string=CRS("+proj=longlat +datum=WGS84"))
grmData2019UTM	<- spTransform(grmData2019SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

affData2019SP	<- SpatialPointsDataFrame(affData2019[,c('longitude', 'latitude')], affData2019, proj4string=CRS("+proj=longlat +datum=WGS84"))
affData2019UTM	<- spTransform(affData2019SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))
affData2019SP2	<- SpatialPointsDataFrame(affData20192[,c('longitude', 'latitude')], affData20192, proj4string=CRS("+proj=longlat +datum=WGS84"))
affData2019UTM2	<- spTransform(affData2019SP2, CRS("+proj=utm +zone=38 +south +datum=WGS84"))
affData2019SP3	<- SpatialPointsDataFrame(affData20193[,c('longitude', 'latitude')], affData20193, proj4string=CRS("+proj=longlat +datum=WGS84"))
affData2019UTM3	<- spTransform(affData2019SP3, CRS("+proj=utm +zone=38 +south +datum=WGS84"))
affData2019SP6	<- SpatialPointsDataFrame(affData20196[,c('longitude', 'latitude')], affData20196, proj4string=CRS("+proj=longlat +datum=WGS84"))
affData2019UTM6	<- spTransform(affData2019SP6, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

#MCP
par(mfrow = c(1, 1))
mcpHR2019	<- mcp(hrData2019UTM[,'group_id'], percent = 95)
mcpHR2019Raster	<- raster(extent(mcpHR2019), crs = CRS("+proj=utm +zone=38 +south +datum=WGS84"), vals = 1)
plot(mcpHR2019)
scalebar(400, xy = click(), type = 'bar', divs = 2, below = 'meters', label = c(0, 200, 400))
plot(hrData2019UTM[,1], add = TRUE)
#writePolyShape(mcpHR2018, "mcpHR2018")

#Base Map
#Group 2
baseMap2		<- openmap(c(-20.7868, 44.16914), c(-20.78215, 44.17526), type = 'bing')
baseMapUTM2		<- openproj(baseMap2, projection = CRS("+proj=utm +zone=38 +south +datum=WGS84"))
baseMapUTMRaster2	<- raster(baseMapUTM2)
baseMapExtent2	<- extent(baseMapUTMRaster2)

xy2	<- matrix(c(-20.7868, 44.16914, -20.78215, 44.17526), nrow = 2, byrow = T)
project(xy2, "+proj=utm +zone=38 +south datum=WGS84")

#Group 3
baseMap3		<- openmap(c(-20.78416, 44.17214), c(-20.78096, 44.1768), type = 'bing')
baseMapUTM3		<- openproj(baseMap3, projection = CRS("+proj=utm +zone=38 +south +datum=WGS84"))
baseMapUTMRaster3	<- raster(baseMapUTM3)
baseMapExtent3	<- extent(baseMapUTMRaster3)

#Group 6
baseMap6		<- openmap(c(-20.78207, 44.16965), c(-20.77868, 44.17362), type = 'bing')
baseMapUTM6		<- openproj(baseMap6, projection = CRS("+proj=utm +zone=38 +south +datum=WGS84"))
baseMapUTMRaster6	<- raster(baseMapUTM6)
baseMapExtent6	<- extent(baseMapUTMRaster6)

#All
baseMap	<- openmap(c(-20.78680, 44.16914), c(-20.77868, 44.1768), type = 'bing')
baseMapUTM	<- openproj(baseMap, projection = CRS("+proj=utm +zone=38 +south +datum=WGS84"))
baseMapUTMRaster	<- raster(baseMapUTM)
#Can downsample the raster - average over neighboring pixels
baseMapExtent	<- extent(baseMapUTMRaster)

x	<- seq(413518,414325.2,by=.576) # where resolution is the pixel size you desire 
y	<- seq(7701222,7702131,by=.576)
xy	<- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

#Kernel based
udHR2019	<- kernelUD(hrData2019UTM[,'group_id'], h='href', grid=xy, kern=c("bivnorm"))
udply2019	<- kernelUD(plyData2019UTM[,'species'], h = 'href', grid=xy, kern=c("bivnorm"))
udgrm2019	<- kernelUD(grmData2019UTM[,'species'], h = 'href',  grid=xy, kern=c("bivnorm"))
udaff2019	<- kernelUD(affData2019UTM[,'group_id'], h = 'href',  grid=xy, kern=c("bivnorm"))

udHRRaster2	<- raster(as(udHR2019$"2","SpatialPixelsDataFrame"))
udHRRaster3	<- raster(as(udHR2019$"3","SpatialPixelsDataFrame"))
udHRRaster6	<- raster(as(udHR2019$"6","SpatialPixelsDataFrame"))

udHRRasterall	<- raster(as(udHR2019$"sifaka","SpatialPixelsDataFrame"))
udaffRasterall	<- raster(as(udaff2019$"sifaka","SpatialPixelsDataFrame"))

udplyRaster2	<- raster(as(udply2019$"2","SpatialPixelsDataFrame"))
udplyRaster3	<- raster(as(udply2019$"3","SpatialPixelsDataFrame"))
udplyRaster6	<- raster(as(udply2019$"6","SpatialPixelsDataFrame"))

udgrmRaster2	<- raster(as(udgrm2019$"2","SpatialPixelsDataFrame"))
udgrmRaster3	<- raster(as(udgrm2019$"3","SpatialPixelsDataFrame"))
udgrmRaster6	<- raster(as(udgrm2019$"6","SpatialPixelsDataFrame"))

udaffRaster2	<- raster(as(udaff2019$"2","SpatialPixelsDataFrame"))
udaffRaster3	<- raster(as(udaff2019$"3","SpatialPixelsDataFrame"))
udaffRaster6	<- raster(as(udaff2019$"6","SpatialPixelsDataFrame"))

kernel95HR2019	<- getverticeshr(udHR2019, percent = 95)
kernel95ply2019	<- getverticeshr(udply2019, percent = 95)
kernel95grm2019	<- getverticeshr(udgrm2019, percent = 95)
kernel95aff2019	<- getverticeshr(udaff2019, percent = 95)

vud <- getvolumeUD(udHR2019)
vudff <- getvolumeUD(udff2018)
vudgrm <- getvolumeUD(udgrm2018)

image(vud, main = '95% Kernel Home Range for P. verreauxi - 2019')
plot(hrData2018UTM[,1])
plot(kernel95HR2018, add = TRUE)

setwd('D:/Google Drive/Graduate School/Research/Projects/SpatialClustering/KMNP/SpatialClusteringKMNPPilot2019')
jpeg("utilizationDistributions2019.jpg", width = 5, height = 10, units = 'in', res = 300)
par(mfrow = c(3, 2))
plotRGB(raster(baseMapUTM))
plot(udHRRaster6, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)

plotRGB(raster(baseMapUTM))
plot(udaffRaster6, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)

plotRGB(raster(baseMapUTM))
plot(udHRRaster3, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)

plotRGB(raster(baseMapUTM))
plot(udaffRaster3, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)

plotRGB(raster(baseMapUTM))
plot(udHRRaster2, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)

plotRGB(raster(baseMapUTM))
plot(udaffRaster2, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)

dev.off()