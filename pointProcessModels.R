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

### Covariates of social trees

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

listScans		<- listScansFull[,c(1:7, 14, 19, 28)]
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
setwd('G:/My Drive/Graduate School/Research/Projects/SpatialClustering/KMNP')
treeNames	<- read.csv('tree_names.csv', stringsAsFactors = FALSE)
treeNums	<- read.csv('tree_gps_points.csv')
trees		<- merge(treeNames, treeNums, by.x = 'No', by.y = 'name', all.x = TRUE)
treesNoNA	<- trees[!is.na(trees$lat),]

treesSP	<- SpatialPointsDataFrame(treesNoNA[,c('lon', 'lat')], treesNoNA, proj4string=CRS("+proj=longlat +datum=WGS84"))
treesUTM	<- spTransform(treesSP[,c('lon', 'lat', 'Arbre')], CRS("+proj=utm +zone=38 +south +datum=WGS84"))

hazoboengaUTM	<- treesUTM[treesUTM$Arbre == 'Hazomboenga',]
hazombyUTM		<- treesUTM[treesUTM$Arbre == 'Hazomby',]
anakarakaUTM	<- treesUTM[treesUTM$Arbre == 'Anakaraky',]
harofyUTM		<- treesUTM[treesUTM$Arbre == 'Arofy',]

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
model2	<- ppm(grmppp, ~ hr, covariates = list(hr = density(hrppp)))
	#At pop level grm and hr look the same, but group 6 is drowning out everyone else, need to seperate into 3 groups

###############################
##### Split into 3 groups #####
###############################
hr2	<- hrDataUTM[hrDataUTM$group_id == 2,]
hr3	<- hrDataUTM[hrDataUTM$group_id == 3,]
hr6	<- hrDataUTM[hrDataUTM$group_id == 6,]
grm2	<- grmDataUTM[grmDataUTM$group_id == 2,]
grm3	<- grmDataUTM[grmDataUTM$group_id == 3,]
grm6Big	<- grmDataUTM[grmDataUTM$group_id == 6,]
grm6	<- grm6Big[grm6Big$latitude != min(grm6Big$latitude),]
all2	<- rbind(hr2[, c('group_id', 'latitude', 'longitude')], grm2[, c('group_id', 'latitude', 'longitude')])
all3	<- rbind(hr3[, c('group_id', 'latitude', 'longitude')], grm3[, c('group_id', 'latitude', 'longitude')])
all6	<- rbind(hr6[, c('group_id', 'latitude', 'longitude')], grm6[, c('group_id', 'latitude', 'longitude')])

mcpHR2	<- mcp(all2[,1], percent = 100)
minXs2	<- c(hr2@bbox[1], grm2@bbox[1])
maxXs2	<- c(hr2@bbox[3], grm2@bbox[3])
minYs2	<- c(hr2@bbox[2], grm2@bbox[2])
maxYs2	<- c(hr2@bbox[4], grm2@bbox[4])
maxWin2	<- as.owin(c(min(minXs2), max(maxXs2), min(minYs2), max(maxYs2)))
mcpWin2	<- as.owin(mcpHR2)

mcpHR3	<- mcp(all3[,1], percent = 100)
minXs3	<- c(hr3@bbox[1], grm3@bbox[1])
maxXs3	<- c(hr3@bbox[3], grm3@bbox[3]) ###Need to calculate mcp as the bounding boxes
minYs3	<- c(hr3@bbox[2], grm3@bbox[2])
maxYs3	<- c(hr3@bbox[4], grm3@bbox[4])
maxWin3	<- as.owin(c(min(minXs3), max(maxXs3), min(minYs3), max(maxYs3)))
mcpWin3	<- as.owin(mcpHR3)

mcpHR6	<- mcp(all6[,1], percent = 100)
minXs6	<- c(hr6@bbox[1], grm6@bbox[1])
maxXs6	<- c(hr6@bbox[3], grm6@bbox[3])
minYs6	<- c(hr6@bbox[2], grm6@bbox[2])
maxYs6	<- c(hr6@bbox[4], grm6@bbox[4])
maxWin6	<- as.owin(c(min(minXs6), max(maxXs6), min(minYs6), max(maxYs6)))
mcpWin6	<- as.owin(mcpHR6)

hrppp2	<- ppp(hr2$longitude, hr2$latitude, window = mcpWin2)
grmppp2	<- ppp(grm2$longitude, grm2$latitude, window = mcpWin2)

hrppp3	<- ppp(hr3$longitude, hr3$latitude, window = mcpWin3)
grmppp3	<- ppp(grm3$longitude, grm3$latitude, window = mcpWin3)

hrppp6	<- ppp(hr6$longitude, hr6$latitude, window = mcpWin6)
grmppp6	<- ppp(grm6$longitude, grm6$latitude, window = mcpWin6)

trees2		<- ppp(treesUTM$lon, treesUTM$lat, window = mcpWin2)
hazoboenga2		<- ppp(hazoboengaUTM$lon, hazoboengaUTM$lat, window = mcpWin2)
hazomby2		<- ppp(hazombyUTM$lon, hazombyUTM$lat, window = mcpWin2)
anakaraka2		<- ppp(anakarakaUTM$lon, anakarakaUTM$lat, window = mcpWin2)
harofy2		<- ppp(harofyUTM$lon, harofyUTM$lat, window = mcpWin2)

trees3		<- ppp(treesUTM$lon, treesUTM$lat, window = mcpWin3)
hazoboenga3		<- ppp(hazoboengaUTM$lon, hazoboengaUTM$lat, window = mcpWin3)
hazomby3		<- ppp(hazombyUTM$lon, hazombyUTM$lat, window = mcpWin3)
anakaraka3		<- ppp(anakarakaUTM$lon, anakarakaUTM$lat, window = mcpWin3)
harofy3		<- ppp(harofyUTM$lon, harofyUTM$lat, window = mcpWin3)

trees6		<- ppp(treesUTM$lon, treesUTM$lat, window = mcpWin6)
hazoboenga6		<- ppp(hazoboengaUTM$lon, hazoboengaUTM$lat, window = mcpWin6)
hazomby6		<- ppp(hazombyUTM$lon, hazombyUTM$lat, window = mcpWin6)
anakaraka6		<- ppp(anakarakaUTM$lon, anakarakaUTM$lat, window = mcpWin6)
harofy6		<- ppp(harofyUTM$lon, harofyUTM$lat, window = mcpWin6)

##########################
##### 3 groups plots #####
##########################
par(mfrow = c(1, 3))
plot(density(hrppp2, 50))
plot(grmppp2, add = TRUE, pch = 16, col = 'white')
plot(trees2, add = TRUE, pch = 16, col = 'yellow')

plot(density(hrppp3, 50))
plot(grmppp3, add = TRUE, pch = 16, col = 'white')

plot(density(hrppp6, 50))
plot(grmppp6, add = TRUE, pch = 16, col = 'white')

##########################
##### 3 group models #####
##########################
model3	<- ppm(grmppp2, ~ hr + trees + hazoboenga + hazomby + harofy + anakaraka, 
	covariates = list(hr = density(hrppp2), trees = density(trees2), hazoboenga = density(hazoboenga2),
	hazomby = density(hazomby2), harofy = density(harofy2), anakaraka = density(anakaraka2)))
model4	<- ppm(grmppp2, ~ hr + trees + hazoboenga + hazomby + anakaraka, 
	covariates = list(hr = density(hrppp2), trees = density(trees2), hazoboenga = density(hazoboenga2),
	hazomby = density(hazomby2), anakaraka = density(anakaraka2))) 
model5	<- ppm(grmppp2, ~ hr + hazoboenga + hazomby + anakaraka, 
	covariates = list(hr = density(hrppp2), hazoboenga = density(hazoboenga2),
	hazomby = density(hazomby2), anakaraka = density(anakaraka2))) 

model6	<- ppm(grmppp3, ~ hr + trees + hazoboenga + hazomby + harofy + anakaraka, 
	covariates = list(hr = density(hrppp3), trees = density(trees3), hazoboenga = density(hazoboenga3),
	hazomby = density(hazomby3), harofy = density(harofy3), anakaraka = density(anakaraka3)))
model7	<- ppm(grmppp3, ~ hr + hazoboenga + hazomby + harofy + anakaraka, 
	covariates = list(hr = density(hrppp3), hazoboenga = density(hazoboenga3),
	hazomby = density(hazomby3), harofy = density(harofy3), anakaraka = density(anakaraka3)))
model8	<- ppm(grmppp3, ~ hr + hazoboenga + hazomby + harofy, 
	covariates = list(hr = density(hrppp3), hazoboenga = density(hazoboenga3),
	hazomby = density(hazomby3), harofy = density(harofy3)))
model9	<- ppm(grmppp3, ~ hr + hazoboenga + harofy, 
	covariates = list(hr = density(hrppp3), hazoboenga = density(hazoboenga3),
	harofy = density(harofy3)))
model10	<- ppm(grmppp3, ~ hr + hazoboenga, 
	covariates = list(hr = density(hrppp3), hazoboenga = density(hazoboenga3)))

model11 <- ppm(grmppp6, ~ hr + trees, 
	covariates = list(hr = density(hrppp6), trees = density(trees6)))
model12 <- ppm(grmppp6, ~ hr, 
	covariates = list(hr = density(hrppp6)))



#group 2 is least heavily tied to hr, then group 3, group 6 is highly tied

par(mfrow = c(1, 3))
plot(predict(model3))
plot(grmppp2, add = TRUE, pch = 16, col = 'white')
plot(predict(model4))
plot(grmppp3, add = TRUE, pch = 16, col = 'white')
plot(predict(model5))
plot(grmppp6, add = TRUE, pch = 16, col = 'white')

