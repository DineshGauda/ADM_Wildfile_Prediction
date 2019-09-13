# automatically installed required packages
list.of.packages <- c("httr","raster","sp","rgdal","xlsx","geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(httr)
library(raster)
library(sp)
library(rgdal)
library("xlsx")
library("geosphere")


setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/../wildfires-adm"))
getwd()


fire_data <- read.delim("Data/canada.monthly.stage1.20130912/NFDB_point_20181129_large_fires.txt", header = TRUE, sep = ",")
head(fire_data$LATITUDE)
str(fire_data)


fire_data$Date <- as.Date(fire_data$REP_DATE, "%Y-%m-%d")
fire_data$FIRE_TYPE <- as.factor(fire_data$FIRE_TYPE)
# year filtering
fire_data<- fire_data[(fire_data$Date >= "2010-01-01" & fire_data$Date <= "2010-12-31"),]

fire_data<- fire_data[,c("LATITUDE","LONGITUDE","YEAR_","MONTH_","DAY_","SIZE_HA","FIRE_TYPE","Date")]
fire_data<-fire_data[(!is.na(fire_data$LONGITUDE)),]
summary(fire_data)

#summary()


wildfire_data <- fire_data[(fire_data$FIRE_TYPE=="Wildfire"),]
head(wildfire_data)
head(wildfire_data$LONGITUDE)
head(wildfire_data$LATITUDE)

summary(wildfire_data)
wildfire_data <- wildfire_data[(!is.na(wildfire_data$LONGITUDE)),]
summary(wildfire_data)
str(wildfire_data)

write.csv(wildfire_data, file = "wildfire_data.csv",row.names=FALSE, na="", quote = FALSE)