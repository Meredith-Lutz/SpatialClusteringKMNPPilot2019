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
con	<- dbConnect(drv, dbname = 'verreauxi_2019_all_data', host = 'localhost', port = 5432,
								 user = 'postgres', password = 'Animalbehavior1#')
listScansFull	<- dbGetQuery(con, 'select * from main_tables.list_scans
					LEFT JOIN main_tables.list_focals
					ON main_tables.list_scans.focal_start_time = main_tables.list_focals.focal_start_time
					LEFT JOIN main_tables.list_sessions
					ON main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time;')
grmFull		<- dbGetQuery(con, "SELECT * FROM main_tables.list_behaviors
				LEFT JOIN main_tables.list_focals
					ON main_tables.list_behaviors.focal_start_time = main_tables.list_focals.focal_start_time
				LEFT JOIN main_tables.list_sessions
					ON main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time
				LEFT JOIN main_tables.trees
					ON main_tables.list_behaviors.tree_number = main_tables.trees.tree_number
				WHERE (behavior = 'Groom' or behavior = 'Mutual Groom') AND (start_stop = 'Start');")

listScans		<- listScans[,c(1:7, 14, 19, 28)]
grm			<- grmFull[,c(1:5, 7, 12, 26, 28:30, 37, 44, 46, 60:64, 70, 73:75)]
colnames(grm)[17]	<- 'tree_height'

grm$DyadID	<- NA
for(i in 1:dim(grm)[1]){
	actor			<- grm[i,]$actor
	recip			<- grm[i,]$subject
	grm[i,]$DyadID	<- paste(sort(c(actor, recip))[1], sort(c(actor, recip))[2], sep = '')
}	

grmSub		<- grm[!duplicated(grm[,c('focal_start_time', 'DyadID', 'latitude', 'longitude')]),]

#Filter NA's and 0's
hrData	<- listScans[listScans$longitude > 0 & is.na(listScans$longitude) == FALSE,]
grmData	<- grmSub[is.na(grmSub$longitude) == FALSE,]

hrDataSP	<- SpatialPointsDataFrame(hrData[,c('longitude', 'latitude')], hrData, proj4string=CRS("+proj=longlat +datum=WGS84"))
hrDataUTM	<- spTransform(hrDataSP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

grmDataSP	<- SpatialPointsDataFrame(grmData[,c('longitude', 'latitude')], grmData, proj4string=CRS("+proj=longlat +datum=WGS84"))
grmDataUTM	<- spTransform(grmDataSP, CRS("+proj=utm +zone=38 +south +datum=WGS84"))

#####################################
##### Defining the bounding box #####
#####################################
minXs			<- c(hrDataUTM@bbox[1], grmDataUTM@bbox[1])
maxXs			<- c(hrDataUTM@bbox[3], grmDataUTM@bbox[3])
minYs			<- c(hrDataUTM@bbox[2], grmDataUTM@bbox[2])
maxYs			<- c(hrDataUTM@bbox[4], grmDataUTM@bbox[4])
maxWin		<- as.owin(c(min(minXs), max(maxXs), min(minYs), max(maxYs)))

#######################
##### Covariates ######
#######################
setwd('D:/Google Drive/Graduate School/Research/Projects/SpatialClustering/MF/Analysis/SpatialAnalysisClustering')
slope		<- raster('slopeRaster.TIF')
slopeClip		<- crop(slope, extent(hrData2018SP))

projected_slope	<- projectRaster(slope, crs = "+proj=utm +zone=38 +south +datum=WGS84")
slope_trans		<- shift(projected_slope, x = -864749, y = -7897710)

#####################################
##### Convert to spatial points #####
#####################################
hrppp		<- ppp(hrDataUTM$longitude, hrDataUTM$latitude, window = maxWin)
grmppp	<- ppp(grmDataUTM$longitude, grmDataUTM$latitude, window = maxWin)

#######################
##### Basic plots #####
#######################
par(mfrow = c(1, 2)) ###Grming and hr look pretty much the same, at least at the population level
plot(density(hrppp, 50))
plot(grmppp, add = TRUE, col = 'white', pch = 16)
plot(density(grmppp, 50))
plot(grmppp, add = TRUE, col = 'white', pch = 16)

#####################
##### Modelling #####
#####################
#grm
model1	<- ppm(grmppp, ~1) #Same story as HR
model2	<- ppm(grmppp, ~ hr, covariates = list(hr = density(hrppp)
	#At pop level grm and hr look the same, but group 6 is drowning out everyone else, need to seperate into 3 groups
