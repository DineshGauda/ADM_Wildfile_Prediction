

#install.packages("raster")
#install.packages("bfastSpatial")
#install.packages("fst")
#install.packages("ncdf4")
#install.packages("zoo")
#install.packages("dplyr")
#install.packages("stringr")

library("dplyr")
library(zoo)
library(fst)
library(ncdf4)
library(bfastSpatial)
library(raster)
library(rgdal)
library(ggplot2)
library(stringr)



############ Code to extract NDVI index ###########################

setwd("D:/MsDataAnalytics/AMD_project/test")
#getwd()
weather_data <- read.csv("resultUnique.csv", stringsAsFactors = F, na.strings = "")
weather_data <- weather_data[!weather_data$V2=="V1",]

#str(weather_data)
weather_data$V2 <- as.Date(weather_data$V2, "%Y-%m-%d")
weather_data$V3 <- as.integer(weather_data$V3)
weather_data$V4 <- as.integer(weather_data$V4)
#weather_data$V2 <- as.Date(weather_data$V2, "%d-%m-%Y")
weather_data <- weather_data[weather_data$V2 < "2011-01-01",]

weatherUniqueCols<-unique(weather_data[c(1,2,3)])


for (i in 1:length(weatherUniqueCols$V2)){
#Use nc_open to read the data into a data structure 
nc_file_data <- nc_open(paste(weatherUniqueCols$V2[i],"nc",sep="."))

#capture these data in the lat, lon, and time dimensions.
lon <- ncvar_get(nc_file_data, "longitude")
lat <- ncvar_get(nc_file_data, "latitude", verbose = F)
time_dim <- ncvar_get(nc_file_data, "time")

# look at the first few entries in the longitude vector
#head(lon)

#Read in the data from the NDVI variable and verify the dimensions of the array. 
ndvi.array <- ncvar_get(nc_file_data, "NDVI")
#dim(ndvi.array) 

#Other pertinent information about the NDVI variable: Lets’s see what fill value was used for missing data
fillvalue <- ncatt_get(nc_file_data, "NDVI", "_FillValue")
#fillvalue

#All done reading in the data. We can close the netCDF file.
nc_close(nc_file_data) 

#replace all those pesky fill values with the R-standard ‘NA’.
ndvi.array[ndvi.array == fillvalue$value] <- NA

ndvi.slice <- ndvi.array[, ] 

#save this data in a raster. Note that we provide the coordinate reference system “CRS” in the standard well-known text format. For this data set, it is the common WGS84 system.
raster_file <- raster(t(ndvi.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))


#plot the raster to take a look at the NDVI
#plot(raster_file)

#save it to a GeoTIFF file
writeRaster(raster_file, "testData.tif", "GTiff", overwrite=TRUE)

tifData = raster("testData.tif")

#plot(tifData)


test.dat <- structure(list(latitude = c(weatherUniqueCols$V4[i]),
                           longitude = c(weatherUniqueCols$V3[i])),
                      .Names = c("latitude","longitude"), class = "data.frame", row.names = c(NA, 1))

test.dat;
points <- cbind(test.dat$longitude,test.dat$latitude)
pts2 = SpatialPoints(points,proj4string=CRS("+init=epsg:4326"))
pts3 = spTransform(pts2, projection(tifData))
coordinates(pts3)
extract(tifData, pts3)
print(i)
print(extract(tifData, pts3))

weatherUniqueCols$NDVI[i] <- extract(tifData, pts3)

# if(is.integer(extract(tifData, pts3))){
#   weatherUniqueCols$NDVI[i] <- extract(tifData, pts3)
# }
# else{
#   weatherUniqueCols$NDVI[i] <- weatherUniqueCols$NDVI[i-1]
# }

}
write.csv(weatherUniqueCols, 'weather_data_new.csv', row.names=FALSE)


weather_data_new <- read.csv("weather_data_new.csv", stringsAsFactors = F, na.strings = "")

weather_data_new$NDVI <- as.numeric(weather_data_new$NDVI)

str(weather_data_new)
weather_data_new$NDVI <- na.locf(weather_data_new$NDVI)
colnames(weather_data_new) <- c("Date","Longitude","Latitude","NDVI")

write.csv(weather_data_new, 'weather_data_NDVI.csv', row.names=FALSE)

#mainFile.csv generated from integrated parser
weather_main_data <- read.csv("mainFile.csv", stringsAsFactors = F, na.strings = "")

str(weather_main_data)

weather_main_data$V1 <- NULL
weather_main_data <- weather_main_data[!weather_main_data$V2=="V1",]
colnames(weather_main_data) <- c("Date","Longitude","Latitude","v5","V6", "V7","V8", "V9", "V10", "V11","V12")
write.csv(weather_main_data, 'weather_maindata.csv', row.names=FALSE)
str(weather_main_data)

merged_Data = Reduce(function(...) merge(..., by=c("Date","Longitude","Latitude")), mget(c('weather_main_data', 'weather_data_new')))
str(merged_Data)

merged_Data$v5 <- as.numeric(merged_Data$v5)

resultUniqueNew <- read.csv("resultUniqueNew.csv", stringsAsFactors = F, na.strings = "")
dim(resultUniqueNew)

str(resultUniqueNew)
colnames(resultUniqueNew) <- c("Date","Longitude","Latitude","Fire")
file_with_values <- read.csv("file_with_values.csv", stringsAsFactors = F, na.strings = "")
dim(file_with_values)
str(file_with_values)
file_with_values$Latitude
merged_Data = Reduce(function(...) merge(..., by=c("Date","Longitude","Latitude")), mget(c('resultUniqueNew', 'file_with_values')))
dim(merged_Data)
merged_Data$X <- NULL
write.csv(merged_Data, 'Final_merged_Data.csv', row.names=FALSE)


test <- merged_Data %>%
  group_by(Date, Longitude, Latitude) %>%
  summarize(mean_size = mean(V5, na.rm = TRUE))


write.csv(merged_Data, 'merged_Data_Final.csv', row.names=FALSE)


###################################################################


################# CODE FOR EXTRACTING NDVI FILES FROM URL #######################

#takes around 2 hours to download. Total Size : 21.2GB

library(rvest)
library(httr)
library(pbapply)
library(stringi)

URL <- "https://www.ncei.noaa.gov/data/avhrr-land-normalized-difference-vegetation-index/access/2010/"

pg <- read_html(URL)
ex_data <- grep("nc$", html_attr(html_nodes(pg, "a[href^='AVHRR']"), "href"), value=TRUE)

invisible(pbsapply(ex_data, function(nc_file) {
  GET(URL %s+% nc_file, write_disk(nc_file))
}))

#####################################################################