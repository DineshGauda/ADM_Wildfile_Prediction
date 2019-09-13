setwd("/home/pratham/Documents/admProject/")

#files = list.files(pattern="*.csv")

wildfireData=read.csv("/home/pratham/Documents/admProject/wildfire_data.csv")

#myfiles = do.call(rbind, lapply(files, function(x) read.csv(x,header = F, stringsAsFactors = T)))

weatherData<-read.csv(file = "2010.csv",header = F,stringsAsFactors = T)

weatherData$V1<-gsub(".*:","",weatherData$V1)
weatherData$V1<-as.Date(weatherData$V1,format = "%m/%d/%Y")

weatherData$V2<-round(weatherData$V2)

weatherData$V3<-round(weatherData$V3)

wildfireData$Date<-as.Date(wildfireData$Date)
wildfireData$LATITUDE<-round(wildfireData$LATITUDE)
wildfireData$LONGITUDE<-round(wildfireData$LONGITUDE)

wildfireUniqueCols<-unique(wildfireData[c(1,2,8)])

weatherUniqueCols<-unique(weatherData[c(1,3,2)])

#106 105 104 103
#56 57 58

wildfireUniqueCols <- wildfireUniqueCols[which(wildfireUniqueCols$LATITUDE %in% c(56,57)),]


wildfireUniqueCols <- wildfireUniqueCols[which(wildfireUniqueCols$LONGITUDE %in% c(-105,-104)),]

for(i in 1:nrow(wildfireUniqueCols))
{
  #get weather data for common lat long of fire and dont check for date yet
  allDaysForLatLong<-weatherData[which(weatherData$V3==wildfireUniqueCols$LATITUDE[i] 
                                       & weatherData$V2==wildfireUniqueCols$LONGITUDE[i]),]
  
  #fire days for same lat long
  fireDays<-weatherData[which(weatherData$V3==wildfireUniqueCols$LATITUDE[i] 
                              & weatherData$V2==wildfireUniqueCols$LONGITUDE[i] 
                              & weatherData$V1==wildfireUniqueCols$Date[i]),]
  #label the data
  allDaysForLatLong[allDaysForLatLong$V1==fireDays$V1,]$V11<-"Yes"
  allDaysForLatLong[allDaysForLatLong$V1!=fireDays$V1,]$V11<-"No"
  
  #add new col for ndvi
  
  write.csv(allDaysForLatLong,file = paste(i,"fireLabel.csv"))
}

files = list.files(pattern="*fireLabel.csv")

myfiles = do.call(rbind, lapply(files, function(x) read.csv(x,header = F, stringsAsFactors = T)))

resultUnique<-unique(myfiles[c(2,3,4,12)])




