##########################
##### Looking at IGE #####
##########################
library(RPostgreSQL)
library(sp)
library(adehabitatHR)

### IGE Data
setwd('G:/My Drive/Graduate School/Research/Projects/SpatialClustering/KMNP/Data')
ige	<- read.csv('IGEDataFromBeccaFeb2020.csv')
conversion	<- read.csv('QuadratsToXY.csv')
colnames(conversion)	<- c('quadrat', 'x', 'y')

plot(jitter(ige$Location.X), jitter(ige$Location.Y), pch = 16)

#### Scan data
drv	<- dbDriver('PostgreSQL') ##Be sure to use real database name
con	<- dbConnect(drv, dbname = 'verreauxi_2019_all_data', host = 'localhost', port = 5433,
								 user = 'postgres', password = 'Animalbehavior1#')
scanData	<- dbGetQuery(con, 'select * from main_tables.all_scan_data_view
					where main_tables.all_scan_data_view.y_position = 0;')


### Social data
grmFull		<- dbGetQuery(con, "SELECT * FROM main_tables.list_behaviors
				LEFT JOIN main_tables.list_focals
					ON main_tables.list_behaviors.focal_start_time = main_tables.list_focals.focal_start_time
				LEFT JOIN main_tables.list_sessions
					ON main_tables.list_focals.session_start_time = main_tables.list_sessions.session_start_time
				LEFT JOIN main_tables.trees
					ON main_tables.list_behaviors.tree_number = main_tables.trees.tree_number
				WHERE (behavior = 'Groom' or behavior = 'Mutual Groom') AND (start_stop = 'Start');")
colnames(grmFull)[59]	<- 'treeQuadrat'
grmFullQuad	<- merge(grmFull, conversion, by.x = 'treeQuadrat', by.y = 'quadrat')

focalAnimalsOnly	<- scanData[scanData$x_position == 0,]
focalAnimalXY	<- merge(focalAnimalsOnly, conversion, by.x = 'quadrat', by.y = 'quadrat')
focalAnimalXYNo12	<- focalAnimalXY[focalAnimalXY$group_id == '2' | focalAnimalXY$group_id == '3' | focalAnimalXY$group_id == '6',]
plot(jitter(focalAnimalXYNo12$x), jitter(focalAnimalXYNo12$y), cex = 0.75, col = focalAnimalXYNo12$group_id, xlab = 'Grid Letter', ylab = 'Grid Number', xlim = c(1,41), ylim = c(1,41), pch = 16)

points(jitter(ige$Location.X), jitter(ige$Location.Y), pch = 16, col = 'yellow')
points(jitter(grmFullQuad$x), jitter(grmFullQuad$y), pch = 16, col = 'black')
