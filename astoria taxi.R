library(bigrquery)
library(foreign)
library(rgdal)
library(proj4)
library(ggplot2)
library(ggmap)
library(RgoogleMaps)
library(plotGoogleMaps)

### Read taxi_zones.shp download from http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml
### Use this shapefile as the boundary of astoria
shape <- readOGR(dsn = ".", layer = "taxi_zones")
astoshape <- shape[shape$zone == 'Astoria',]
mhtshape <- shape[(shape$borough == 'Manhattan') 
                  & (!shape$LocationID %in% c(42, 103, 116, 120, 127, 128, 152, 153, 
                                              194, 202, 243, 244)),]
lgashape <- shape[shape$zone == 'LaGuardia Airport',]
ueshape <- shape[(shape$zone == 'Upper East Side North') |
                 (shape$zone == 'Upper East Side South'),]
mtshape <- shape[(shape$zone == 'Midtown Center'), ]


plot(shape)
shapemap <- spTransform(shape, CRS("+proj=longlat +datum=WGS84"))
shapemap <- fortify(shapemap)
#geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='green', data=shapemap, alpha=0)
plotGoogleMaps(shape, zcol = 1, col = 'BLUE', legend = FALSE, 
               layerName = 'Taxi Zone', fillOpacity = 0.2, 
               streetViewControl = TRUE, 
               mapTypeId = "ROADMAP", filename = "ZoneMap.html")
pj <- project(bbox(astoshape), proj4string(astoshape), inverse=TRUE)

#### 1. Select rides within astoria
project <- "extreme-outpost-169218"
sql <- "
SELECT *
FROM [imjasonh-storage:nyctaxi.trip_data]
WHERE
(round(cast(dropoff_longitude as float), 4) between -73.93860 and -73.90202 and
 round(cast(dropoff_latitude as float), 4) between 40.75214 and 40.77002) and
(round(cast(pickup_longitude as float), 6) between -73.93860 and -73.90202 and
 round(cast(pickup_latitude as float), 6) between 40.75214 and 40.77002)
"

raw1 <- query_exec(sql, project=project, max_pages = Inf)
save(raw1, file = 'raw1.RData')
load('raw1.RData')

rawastodata <- raw1
rawastodata$pickup_longitude <- as.numeric(rawastodata$pickup_longitude)
rawastodata$pickup_latitude <- as.numeric(rawastodata$pickup_latitude)
rawastodata$dropoff_longitude <- as.numeric(rawastodata$dropoff_longitude)
rawastodata$dropoff_latitude <- as.numeric(rawastodata$dropoff_latitude)

##### Pick up within Astoria
prawasto <- rawastodata
coordinates(prawasto) <- ~ pickup_longitude + pickup_latitude
proj4string(prawasto) <- CRS("+proj=longlat")
prawasto<- spTransform(prawasto, proj4string(astoshape))
proj4string(prawasto) <- proj4string(astoshape)

##### Drop off within Astoria
drawasto <- rawastodata
coordinates(drawasto) <- ~ dropoff_longitude + dropoff_latitude
proj4string(drawasto) <- CRS("+proj=longlat")
drawasto<- spTransform(drawasto, proj4string(astoshape))
proj4string(drawasto) <- proj4string(astoshape)

#### within Astoria rows
withinasto <- rawastodata[(!is.na(over(drawasto, astoshape)[,1])) & 
                          (!is.na(over(prawasto, astoshape)[,1])),]
save(withinasto, file = 'within.asto.RData')
load('within.asto.RData')

#### plot the pick up location
plot(prawasto[row.names(withinasto), ], cex = 0.3)
plot(astoshape, add = TRUE, border = "red")

#### plot the pick up location
plot(drawasto[row.names(withinasto), ], cex = 0.3)
plot(astoshape, add = TRUE, border = "red")

plotGoogleMaps(prawasto[sample(row.names(withinasto), 5000), ], zcol = 8, col = 'orangered', legend = FALSE, 
               layerName = 'Pickup within Astoria', fillOpacity = 0.2, 
               streetViewControl = TRUE,
               mapTypeId = "ROADMAP", filename = "PickupPointMap.html")

plotGoogleMaps(drawasto[sample(row.names(withinasto), 5000), ], zcol = 8, col = 'orangered', legend = FALSE, 
               layerName = 'Dropoff within Astoria', fillOpacity = 0.2, 
               streetViewControl = TRUE,
               mapTypeId = "ROADMAP", filename = "DropoffPointMap.html")

withinasto$trip_distance <- as.numeric(withinasto$trip_distance)
withinasto$passenger_count <- as.numeric(withinasto$passenger_count)
withinasto$trip_time_in_secs <- as.numeric(withinasto$trip_time_in_secs)
hist(withinasto$trip_distance)
hist(withinasto$passenger_count, main = 'Passenger counts for rides within Astoria', 
     xlab = 'passenger count')
hist(withinasto$trip_time_in_secs/withinasto$trip_distance)

qplot(withinasto$passenger_count, geom="histogram", binwidth = 0.5,
      main = 'Passenger counts for rides within Astoria', 
      xlab = 'passenger count', 
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))

smgroup1 <- sum(withinasto$passenger_count <= 4)/nrow(withinasto)
pdisp1 <- var(withinasto$pickup_longitude)/
  (max(withinasto$pickup_longitude) - min(withinasto$pickup_longitude))^2 +
  var(withinasto$pickup_latitude)/
  (max(withinasto$pickup_latitude) - min(withinasto$pickup_latitude))^2
ddisp1 <- var(withinasto$dropoff_longitude)/
  (max(withinasto$dropoff_longitude) - min(withinasto$dropoff_longitude))^2 +
  var(withinasto$dropoff_latitude)/
  (max(withinasto$dropoff_latitude) - min(withinasto$dropoff_latitude))^2

kmeans(withinasto$pickup_longitude, 10)

##### Time analysis
withinasto$pickup_time <- t(as.data.frame(strsplit(withinasto$pickup_datetime, ' ')))[,2]
t <- format(strptime(withinasto$pickup_time, format="%H:%M:%S"), "%H:00:00")
plot(table(t))
ggplot(as.data.frame(table(t)), aes(x = t, y = Freq)) + 
  geom_bar(stat="identity", fill = 'blue', alpha = 0.2, col = 'red') +
  ggtitle('Hourly Pick up Frequency') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())

#### 2. Select rides from astoria to manhattan
## Get a rough boundray of manhattan (via service area)
project(bbox(shape[shape$zone == 'Battery Park City',]), 
        proj4string(shape[shape$zone == 'Battery Park City',]), inverse=TRUE)
project(bbox(shape[shape$zone == 'East Harlem North',]), 
        proj4string(shape[shape$zone == 'East Harlem North',]), inverse=TRUE)
project(bbox(shape[shape$zone == 'Morningside Heights',]), 
        proj4string(shape[shape$zone == 'Morningside Heights',]), inverse=TRUE)
project(bbox(shape[shape$zone == 'Financial District South',]), 
        proj4string(shape[shape$zone == 'Financial District South',]), inverse=TRUE)

sql <- "
SELECT *
FROM [imjasonh-storage:nyctaxi.trip_data]
WHERE
(round(cast(dropoff_longitude as float), 4) between -74.01934 and -73.92649 and
round(cast(dropoff_latitude as float), 4) between 40.69977 and 40.81808) and
(round(cast(pickup_longitude as float), 6) between -73.93860 and -73.90202 and
round(cast(pickup_latitude as float), 6) between 40.75214 and 40.77002)
"

raw2 <- query_exec(sql, project=project, max_pages = Inf)
save(raw2, file = 'raw2.RData')
load('raw2.RData')

rawastomhtdata <- raw2
rawastomhtdata$pickup_longitude <- as.numeric(rawastomhtdata$pickup_longitude)
rawastomhtdata$pickup_latitude <- as.numeric(rawastomhtdata$pickup_latitude)
rawastomhtdata$dropoff_longitude <- as.numeric(rawastomhtdata$dropoff_longitude)
rawastomhtdata$dropoff_latitude <- as.numeric(rawastomhtdata$dropoff_latitude)

##### Pick up within Astoria
prawasto <- rawastomhtdata
coordinates(prawasto) <- ~ pickup_longitude + pickup_latitude
proj4string(prawasto) <- CRS("+proj=longlat")
prawasto<- spTransform(prawasto, proj4string(astoshape))
proj4string(prawasto) <- proj4string(astoshape)

##### Drop off within Manhattan
drawasto <- rawastomhtdata
coordinates(drawasto) <- ~ dropoff_longitude + dropoff_latitude
proj4string(drawasto) <- CRS("+proj=longlat")
drawasto<- spTransform(drawasto, proj4string(mhtshape))
proj4string(drawasto) <- proj4string(mhtshape)

##### select qualified rows
astomht <- rawastomhtdata[(!is.na(over(drawasto, mhtshape)[,1])) & 
                          (!is.na(over(prawasto, astoshape)[,1])),]
save(astomht, file = 'asto.mht.RData')
load('asto.mht.RData')

plot(drawasto[row.names(astomht), ], cex = 0.1)
plot(mhtshape, add = TRUE, border = "red")

plot(prawasto[row.names(astomht), ], cex = 0.3)
plot(astoshape, add = TRUE, border = "red")

astomht$passenger_count <- as.numeric(astomht$passenger_count)

qplot(astomht$passenger_count, geom="histogram", binwidth = 0.5,
      main = 'Passenger counts for rides from Astoria to Manhattan', 
      xlab = 'passenger count', 
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))

smgroup2 <- sum(astomht$passenger_count <= 4)/nrow(astomht)
pdisp2 <- var(astomht$pickup_longitude)/
  (max(astomht$pickup_longitude) - min(astomht$pickup_longitude))^2 +
  var(astomht$pickup_latitude)/
  (max(astomht$pickup_latitude) - min(astomht$pickup_latitude))^2
ddisp2 <- var(astomht$dropoff_longitude)/
  (max(astomht$dropoff_longitude) - min(astomht$dropoff_longitude))^2 +
  var(astomht$dropoff_latitude)/
  (max(astomht$dropoff_latitude) - min(astomht$dropoff_latitude))^2

#### 3. Select rides from LGA to manhattan
## Get a rough boundray of LGA
project(bbox(shape[shape$zone == 'LaGuardia Airport',]), 
        proj4string(shape[shape$zone == 'LaGuardia Airport',]), inverse=TRUE)

sql <- "
SELECT dropoff_longitude, dropoff_latitude, pickup_longitude, pickup_latitude, passenger_count, 
       pickup_datetime, dropoff_datetime
FROM [imjasonh-storage:nyctaxi.trip_data]
WHERE
(round(cast(dropoff_longitude as float), 4) between -74.01934 and -73.92649 and
round(cast(dropoff_latitude as float), 4) between 40.69977 and 40.81808) and
(round(cast(pickup_longitude as float), 4) between -73.89185 and -73.85502 and
round(cast(pickup_latitude as float), 4) between 40.76370 and 40.78602)
"

raw3 <- query_exec(sql, project=project, max_pages = Inf)
save(raw3, file = 'raw3.RData')
load('raw3.RData')

rawlgamhtdata <- raw3
rawlgamhtdata$pickup_longitude <- as.numeric(rawlgamhtdata$pickup_longitude)
rawlgamhtdata$pickup_latitude <- as.numeric(rawlgamhtdata$pickup_latitude)
rawlgamhtdata$dropoff_longitude <- as.numeric(rawlgamhtdata$dropoff_longitude)
rawlgamhtdata$dropoff_latitude <- as.numeric(rawlgamhtdata$dropoff_latitude)

##### Pick up within LGA
prawasto <- rawlgamhtdata
coordinates(prawasto) <- ~ pickup_longitude + pickup_latitude
proj4string(prawasto) <- CRS("+proj=longlat")
prawasto<- spTransform(prawasto, proj4string(lgashape))
proj4string(prawasto) <- proj4string(lgashape)

##### Drop off within Manhattan
drawasto <- rawlgamhtdata
coordinates(drawasto) <- ~ dropoff_longitude + dropoff_latitude
proj4string(drawasto) <- CRS("+proj=longlat")
drawasto<- spTransform(drawasto, proj4string(mhtshape))
proj4string(drawasto) <- proj4string(mhtshape)

##### select qualified rows
lgamht <- rawlgamhtdata[(!is.na(over(drawasto, mhtshape)[,1])) & 
                            (!is.na(over(prawasto, lgashape)[,1])),]
save(lgamht, file = 'lga.mht.RData')

plot(drawasto[row.names(lgamht), ], cex = 0.1)
plot(mhtshape, add = TRUE, border = "red")

plot(prawasto[row.names(lgamht), ], cex = 0.3)
plot(lgashape, add = TRUE, border = "red")

lgamht$passenger_count <- as.numeric(lgamht$passenger_count)

qplot(lgamht$passenger_count, geom="histogram", binwidth = 0.5,
      main = 'Passenger counts for rides from LGA to Manhattan', 
      xlab = 'passenger count', 
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))

smgroup3 <- sum(lgamht$passenger_count <= 4)/nrow(lgamht)
pdisp3 <- var(lgamht$pickup_longitude)/
  (max(lgamht$pickup_longitude) - min(lgamht$pickup_longitude))^2 +
  var(lgamht$pickup_latitude)/
  (max(lgamht$pickup_latitude) - min(lgamht$pickup_latitude))^2
ddisp3 <- var(lgamht$dropoff_longitude)/
  (max(lgamht$dropoff_longitude) - min(lgamht$dropoff_longitude))^2 +
  var(lgamht$dropoff_latitude)/
  (max(lgamht$dropoff_latitude) - min(lgamht$dropoff_latitude))^2

#### 4. Select rides from UES to midtown
## Get a rough boundray of two district
project(bbox(ueshape), proj4string(ueshape), inverse=TRUE)
project(bbox(mtshape), proj4string(mtshape), inverse=TRUE)


sql <- "
SELECT dropoff_longitude, dropoff_latitude, pickup_longitude, pickup_latitude, passenger_count
FROM [imjasonh-storage:nyctaxi.trip_data]
WHERE
((round(cast(dropoff_longitude as float), 4) between -73.98412 and -73.97124 and
round(cast(dropoff_latitude as float), 4) between 40.75242 and 40.76364) and
(round(cast(pickup_longitude as float), 4) between -73.97302 and -73.94933 and
round(cast(pickup_latitude as float), 4) between 40.76155 and 40.78791)) or

((round(cast(pickup_longitude as float), 4) between -73.98412 and -73.97124 and
round(cast(pickup_latitude as float), 4) between 40.75242 and 40.76364) and
(round(cast(dropoff_longitude as float), 4) between -73.97302 and -73.94933 and
round(cast(dropoff_latitude as float), 4) between 40.76155 and 40.78791))

"


raw4 <- query_exec(sql, project=project, max_pages = Inf)
save(raw4, file = 'raw4.RData')
load('raw4.RData')

raw4data <- raw4
raw4data$pickup_longitude <- as.numeric(raw4data$pickup_longitude)
raw4data$pickup_latitude <- as.numeric(raw4data$pickup_latitude)
raw4data$dropoff_longitude <- as.numeric(raw4data$dropoff_longitude)
raw4data$dropoff_latitude <- as.numeric(raw4data$dropoff_latitude)

##### Pick up within upper east
prawasto <- raw4data
coordinates(prawasto) <- ~ pickup_longitude + pickup_latitude
proj4string(prawasto) <- CRS("+proj=longlat")
prawasto<- spTransform(prawasto, proj4string(ueshape))
proj4string(prawasto) <- proj4string(ueshape)

##### Drop off within midtown
drawasto <- raw4data
coordinates(drawasto) <- ~ dropoff_longitude + dropoff_latitude
proj4string(drawasto) <- CRS("+proj=longlat")
drawasto<- spTransform(drawasto, proj4string(mtshape))
proj4string(drawasto) <- proj4string(mtshape)

##### select qualified rows
uemt <- raw4data[(!is.na(over(drawasto, rbind(mtshape, ueshape))[,1])) & 
                 (!is.na(over(prawasto, rbind(mtshape, ueshape))[,1])),]
save(uemt, file = 'ue.mt.RData')

plot(drawasto[row.names(uemt), ], cex = 0.02)
plot(rbind(mtshape, ueshape), add = TRUE, border = "red")

plot(prawasto[row.names(uemt), ], cex = 0.02)
plot(rbind(mtshape, ueshape), add = TRUE, border = "red")

uemt$passenger_count <- as.numeric(uemt$passenger_count)

qplot(uemt$passenger_count, geom="histogram", binwidth = 0.5,
      main = 'Passenger counts for rides from Upper East to Midtown', 
      xlab = 'passenger count', 
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))

smgroup4 <- sum(uemt$passenger_count <= 4)/nrow(uemt)
pdisp4 <- var(uemt$pickup_longitude)/
  (max(uemt$pickup_longitude) - min(uemt$pickup_longitude))^2 +
  var(uemt$pickup_latitude)/
  (max(uemt$pickup_latitude) - min(uemt$pickup_latitude))^2
ddisp4 <- var(uemt$dropoff_longitude)/
  (max(uemt$dropoff_longitude) - min(uemt$dropoff_longitude))^2 +
  var(uemt$dropoff_latitude)/
  (max(uemt$dropoff_latitude) - min(uemt$dropoff_latitude))^2

