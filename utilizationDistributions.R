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

## Pull 2018 data
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
allAff	<- rbind(mgrm, grm, ply, grt)

#Filter NA's and 0's
hrData2019all	<- listScans[listScans$longitude > 0 & is.na(listScans$longitude) == FALSE,]
hrData2019		<- hrData2019all[hrData2019all$group_id == '2' | hrData2019all$group_id == '3' | hrData2019all$group_id == '6',]
plyData2019all	<- ply[is.na(ply$longitude) == FALSE,]
plyData2019		<- plyData2019all[plyData2019all$group_id == '2' | plyData2019all$group_id == '3' | plyData2019all$group_id == '6',]
affData2019all	<- allAff[is.na(allAff$longitude) == FALSE,]
affData2019		<- affData2019all[affData2019all$group_id == '2' | affData2019all$group_id == '3' | affData2019all$group_id == '6',]
grmData2019all	<- grm[is.na(grm$longitude) == FALSE,]
grmData2019		<- grmData2019all[grmData2019all$group_id == '2' | grmData2019all$group_id == '3' | grmData2019all$group_id == '6',]

plot(hrData2019$longitude, hrData2019$latitude, xlab = 'Longitude', ylab = 'Latitude')

#Convert to Spatial points object
hrData2019SP	<- SpatialPointsDataFrame(hrData2019[,c('longitude', 'latitude')], hrData2019, proj4string=CRS("+proj=longlat +datum=WGS84"))
hrData2019UTM	<- spTransform(hrData2019SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

plyData2019SP	<- SpatialPointsDataFrame(plyData2019[,c('longitude', 'latitude')], plyData2019, proj4string=CRS("+proj=longlat +datum=WGS84"))
plyData2019UTM	<- spTransform(plyData2019SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

grmData2019SP	<- SpatialPointsDataFrame(grmData2019[,c('longitude', 'latitude')], grmData2019, proj4string=CRS("+proj=longlat +datum=WGS84"))
grmData2019UTM	<- spTransform(grmData2019SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

affData2019SP	<- SpatialPointsDataFrame(affData2019[,c('longitude', 'latitude')], affData2019, proj4string=CRS("+proj=longlat +datum=WGS84"))
affData2019UTM	<- spTransform(affData2019SP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

#MCP
mcpHR2019	<- mcp(hrData2019UTM[,'group_id'], percent = 95)
plot(mcpHR2019)
plot(hrData2019UTM[,1], add = TRUE)
#writePolyShape(mcpHR2018, "mcpHR2018")

#Base Map
baseMap	<- openmap(c(-20.788, 44.1691), c(-20.7787, 44.18), type = 'bing')
baseMapUTM	<- openproj(baseMap, projection = CRS("+proj=utm +zone=38 +south +datum=WGS84"))
baseMapUTMRaster	<- raster(baseMapUTM)
baseMapExtent	<- extent(baseMapUTMRaster)

x <- seq(413511,414662,by=1) # where resolution is the pixel size you desire 
y <- seq(7701087,7702133,by=1)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

#Kernel based
udHR2019	<- kernelUD(hrData2019UTM[,'group_id'], h='href', grid=xy, kern=c("bivnorm"))
udply2019	<- kernelUD(plyData2019UTM[,'group_id'], h = 'href', grid=xy, kern=c("bivnorm"))
udgrm2019	<- kernelUD(grmData2019UTM[,'group_id'], h = 'href',  grid=xy, kern=c("bivnorm"))
udaff2019	<- kernelUD(affData2019UTM[,'group_id'], h = 'href',  grid=xy, kern=c("bivnorm"))

udHRRaster2	<- raster(as(udHR2019$"2","SpatialPixelsDataFrame"))
udHRRaster3	<- raster(as(udHR2019$"3","SpatialPixelsDataFrame"))
udHRRaster6	<- raster(as(udHR2019$"6","SpatialPixelsDataFrame"))

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

jpeg("utilizationDistributions2018.jpg", width = 8, height = 4, units = 'in', res = 300)
par(mfrow = c(1, 3))
plotRGB(raster(baseMapUTM))
plot(udaffRaster2, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)
plot(udaffRaster3, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)
plot(udaffRaster6, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)


plotRGB(raster(baseMapUTM))
plot(udffRaster, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Feeding locations', alpha = 0.3)

plotRGB(raster(baseMapUTM))
plot(udgrmRaster, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Grooming locations', alpha = 0.3)
dev.off()