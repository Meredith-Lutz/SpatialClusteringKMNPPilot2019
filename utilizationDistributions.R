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
mgrm		<- dbGetQuery(con, "select * from main_tables.list_behaviors where behavior ='Mutual Groom' and start_stop = 'Start';")
grm		<- dbGetQuery(con, "select * from main_tables.list_behaviors where behavior ='Groom' and start_stop = 'Start';")
ply		<- dbGetQuery(con, "select * from main_tables.list_behaviors where behavior ='Play' and start_stop = 'Start';")
grt		<- dbGetQuery(con, "select * from main_tables.list_behaviors where behavior ='Greet' and start_stop = 'Start';")
allAff	<- rbind(mgrm, grm, ply, grt)

#Filter NA's and 0's
hrData2019all	<- listScans[listScans$longitude > 0 & is.na(listScans$longitude) == FALSE,]
hrData2019	<- hrData2019all[hrData2019all$group_id == '2' | hrData2019all$group_id == '3' | hrData2019all$group_id == '6',]
plyData2019	<- ply[is.na(ply$longitude) == FALSE,]
affData2019	<- allAff[is.na(allAff$longitude) == FALSE,]
grmData2019	<- grm[is.na(grm$longitude) == FALSE,]

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
q	<- opq(bbox = c(-20.78680, 44.16914, -20.77868, 44.1768))
baseMap	<- openmap(c(-20.80, 44.14), c(-20.77868, 44.2), type = 'bing')
baseMapUTM	<- openproj(baseMap, projection = CRS("+proj=utm +zone=38 +south +datum=WGS84"))
baseMapUTMRaster	<- raster(baseMapUTM)
baseMapExtent	<- extent(baseMapUTMRaster)

x <- seq(413523.8,414320.8,by=0.567) # where resolution is the pixel size you desire 
y <- seq(7701225,7702125,by=0.567)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

#Kernel based
udHR2019	<- kernelUD(hrData2019UTM[,'group_id'], h='href', grid=xy, kern=c("bivnorm"))
udply2019	<- kernelUD(plyData2019UTM[,'group_id'], h = 'href', grid=xy, kern=c("bivnorm"))
udgrm2019	<- kernelUD(grmData2019UTM[,'group_id'], h = 'href',  grid=xy, kern=c("bivnorm"))

udHRRaster2	<- raster(as(udHR2019$"2","SpatialPixelsDataFrame"))
udHRRaster3	<- raster(as(udHR2019$"3","SpatialPixelsDataFrame"))
udHRRaster6	<- raster(as(udHR2019$"6","SpatialPixelsDataFrame"))

udffRaster	<- raster(as(udff2018$"8FC693C1-AE6A-4530-A7D0-DC9F376A8CE7","SpatialPixelsDataFrame"))
udgrmRaster	<- raster(as(udgrm2018$"8FC693C1-AE6A-4530-A7D0-DC9F376A8CE7","SpatialPixelsDataFrame"))

kernel95HR2019	<- getverticeshr(udHR2019, percent = 95)
kernel95ff2018	<- getverticeshr(udff2018, percent = 95)
kernel95grm2018	<- getverticeshr(udgrm2018, percent = 95)

vud <- getvolumeUD(udHR2019)
vudff <- getvolumeUD(udff2018)
vudgrm <- getvolumeUD(udgrm2018)

image(vud, main = '95% Kernel Home Range for P. verreauxi - 2019')
plot(hrData2018UTM[,1])
plot(kernel95HR2018, add = TRUE)

jpeg("utilizationDistributions2018.jpg", width = 8, height = 4, units = 'in', res = 300)
par(mfrow = c(1, 3))
plotRGB(raster(baseMapUTM))
plot(udHRRaster2, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)
plot(udHRRaster3, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)
plot(udHRRaster6, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Utilization distribution', alpha = 0.3)


plotRGB(raster(baseMapUTM))
plot(udffRaster, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Feeding locations', alpha = 0.3)

plotRGB(raster(baseMapUTM))
plot(udgrmRaster, add = TRUE, axes = FALSE, box = FALSE, legend = F, main = 'Grooming locations', alpha = 0.3)
dev.off()