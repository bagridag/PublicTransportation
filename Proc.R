
require(shiny)
require(rCharts)
require(RJSONIO)
require(rCharts)
require(RColorBrewer)
require(httr)
library(readr)
library(lubridate)

options(stringsAsFactors = F)

library(dplyr)
library(ggmap)
library(timeDate)
library(shinythemes)
library(shiny)
library(leaflet)
library(jsonlite)
library(shinydashboard)

setwd("C:/Users/burcakagridag/Documents/BikeNYCVis")


#functions
processTime <- function(df){
  df$starttime <- strptime(df$starttime, "%m/%d/%Y %H:%M:%S")
  df$stoptime <- strptime(df$stoptime, "%m/%d/%Y %H:%M:%S")
  df$starttime <- as.POSIXct((df$starttime), tz= "America/New_York")
  df$stoptime <- as.POSIXct((df$stoptime), tz= "America/New_York")
  df$weekend <- sapply(df$starttime, isWeekend)
  df$startTime <- format(df$starttime, "%T")
  df$start <- df$starttime
  df$endTime <- format(df$stoptime, "%T")
  df$date <- format(df$starttime, format="%Y-%m-%d")
  df$date <- as.POSIXct(format((df$date), format = "%Y-%m-%d"))
  #df$date <- as.POSIXct((df$date), tz= "America/New_York")
  df$endDay <- format(df$stoptime, format="%Y-%m-%d")
  df$endDay <- as.POSIXct((df$endDay), tz= "America/New_York")
  df$morning <- df$startTime >= "05:00" & df$startTime < "10:00"
  df$evening <- df$startTime >= "16:00" & df$startTime < "20:00"
  df$night <- df$startTime >= "20:00" & df$startTime <= "02:00"
  df$stLat <- df$`start station latitude` 
  df$stLon <- df$`start station longitude` 
  df$endLat <- df$`end station latitude` 
  df$endLon <- df$`end station longitude` 
  df$starttime <- NULL
  df$stoptime<- NULL
  df$`start station latitude` <- NULL
  df$`start station longitude` <- NULL
  df$`end station latitude` <- NULL 
  df$`end station longitude` <- NULL
  df <- inner_join(df, weather, by="date")
  
  return(df)
}


processTime2 <- function(df){
  df$starttime <- df$`Start Time`
  df$stoptime <- df$`Stop Time`
  df$weekend <- sapply(df$starttime, isWeekend)
  df$startTime <- format(df$starttime, "%T")
  df$start <- df$starttime
  df$endTime <- format(df$stoptime, "%T")
  df$date <- format(df$starttime, format="%Y-%m-%d")
  df$date <- as.POSIXct(format((df$date), format = "%Y-%m-%d"))
  #df$date <- as.POSIXct((df$date), tz= "America/New_York")
  df$endDay <- format(df$stoptime, format="%Y-%m-%d")
  df$endDay <- as.POSIXct((df$endDay), tz= "America/New_York")
  df$morning <- df$startTime >= "05:00" & df$startTime < "10:00"
  df$evening <- df$startTime >= "16:00" & df$startTime < "20:00"
  df$night <- df$startTime >= "20:00" & df$startTime <= "02:00"
  df$`start station id`  <- df$`Start Station ID` 
  df$`start station name`  <- df$`Start Station Name` 
  df$`end station id`  <- df$`End Station ID` 
  df$`end station name`  <- df$`End Station Name` 
  df$bikeid  <- df$`Bike ID` 
  df$usertype  <- df$`User Type` 
  df$`birth year`  <- df$`Birth Year` 
  df$gender  <- df$Gender 
  
  df$`Start Station ID` <- NULL
  df$`End Station ID` <- NULL 
  df$`Start Station Name` <- NULL
  df$`End Station Name` <- NULL
  df$`Bike ID` <- NULL
  df$`User Type` <- NULL
  df$`Birth Year` <- NULL
  df$Gender <- NULL
  
  df$`Start Time` <- NULL
  df$`Stop Time`<- NULL
  df$stLat <- df$`Start Station Latitude` 
  df$stLon <- df$`Start Station Longitude` 
  df$endLat <- df$`End Station Latitude` 
  df$endLon <- df$`End Station Longitude` 
  df$tripduration <- df$`Trip Duration`
  
  df$`Start Station Latitude` <- NULL
  df$`Start Station Longitude` <- NULL
  df$`End Station Latitude` <- NULL 
  df$`End Station Longitude` <- NULL
  df$`Trip Duration` <- NULL
  df$starttime <- NULL
  df$stoptime<- NULL
  df <- inner_join(df, weather, by="date")
  
  return(df)
}






#data aggregate
processData <- function(data, data2){
  summarise(group_by(data, date), 
            count=n(), 
            precipitation=mean(precipitation), 
            maxTmp=mean(maxTmp), 
            minTmp=mean(minTmp)) %>%
    mutate(., weekend = sapply(.$date, isWeekend)) %>%
    rbind(data2, .)
}


processData2 <- function(data){
  summarise(group_by(data, time), 
            count=n())
}

#load data
JC_20161_citibike_tripdata <- read_csv("201601-citibike-tripdata.csv")
JC_20162_citibike_tripdata <- read_csv("201602-citibike-tripdata.csv")
JC_20163_citibike_tripdata <- read_csv("201603-citibike-tripdata.csv")
JC_201604_citibike_tripdata <- read_csv("201604-citibike-tripdata.csv")
JC_201605_citibike_tripdata <- read_csv("201605-citibike-tripdata.csv")
JC_201606_citibike_tripdata <- read_csv("201606-citibike-tripdata.csv")
JC_201607_citibike_tripdata <- read_csv("201607-citibike-tripdata.csv")
JC_201608_citibike_tripdata <- read_csv("201608-citibike-tripdata.csv")
JC_201609_citibike_tripdata <- read_csv("201609-citibike-tripdata.csv")
JC_201610_citibike_tripdata <- read_csv("201610-citibike-tripdata.csv")
JC_201611_citibike_tripdata <- read_csv("201611-citibike-tripdata.csv")
JC_201612_citibike_tripdata <- read_csv("201612-citibike-tripdata.csv")


weather <- read_csv("weather_data_nyc.csv")
weather[] <- apply(weather, 2, function(x) ifelse(x == "T", 1, x))
weather$date <- as.Date(weather$date, "%d-%m-%Y")
weather$date <- as.POSIXct(format((weather$date), format = "%Y-%m-%d"))
weather$precipitation <- as.numeric(weather$precipitation)
weather$`snow fall` <- as.numeric(weather$`snow fall`)
weather$`snow depth` <- as.numeric(weather$`snow depth`)
weather$avgTmp <- as.numeric(weather$`average temperature`)
weather$maxTmp <- as.numeric(weather$`maximum temperature`)
weather$minTmp <-  as.numeric(weather$`minimum temperature`)
weather$`average temperature` <- NULL
weather$`maximum temperature` <- NULL
weather$`minimum temperature` <- NULL


#create data tables for further processing 

data01 <- JC_20161_citibike_tripdata
data02 <- JC_20162_citibike_tripdata
data03 <- JC_20163_citibike_tripdata
data04 <- JC_201604_citibike_tripdata
data05 <- JC_201605_citibike_tripdata
data06 <- JC_201606_citibike_tripdata
data07 <- JC_201607_citibike_tripdata
data08 <- JC_201608_citibike_tripdata
data09 <- JC_201609_citibike_tripdata
data10 <- JC_201610_citibike_tripdata
data11 <- JC_201611_citibike_tripdata
data12 <- JC_201612_citibike_tripdata

#remove for space
rm(JC_20161_citibike_tripdata)
rm(JC_20162_citibike_tripdata)
rm(JC_20163_citibike_tripdata)
rm(JC_201604_citibike_tripdata)
rm(JC_201605_citibike_tripdata)
rm(JC_201606_citibike_tripdata)
rm(JC_201607_citibike_tripdata)
rm(JC_201608_citibike_tripdata)
rm(JC_201609_citibike_tripdata)
rm(JC_201610_citibike_tripdata)
rm(JC_201611_citibike_tripdata)
rm(JC_201612_citibike_tripdata)

#clean the data
data01 <- na.omit(data01)
data02 <- na.omit(data02)
data03 <- na.omit(data03)
data04 <- na.omit(data04)
data05 <- na.omit(data05)
data06 <- na.omit(data06)
data07 <- na.omit(data07)
data08 <- na.omit(data08)
data09 <- na.omit(data09)
data10 <- na.omit(data10)
data11 <- na.omit(data11)
data12 <- na.omit(data12)

data01Sample <- sample_n(data01, 5000, replace=FALSE)
data02Sample <- sample_n(data02, 5000, replace=FALSE)
data03Sample <- sample_n(data03, 8000, replace=FALSE)
data04Sample <- sample_n(data04, 8500, replace=FALSE)
data05Sample <- sample_n(data05, 10000, replace=FALSE)
data06Sample <- sample_n(data06, 12000, replace=FALSE)
data07Sample <- sample_n(data07, 12000, replace=FALSE)
data08Sample <- sample_n(data08, 13000, replace=FALSE)
data09Sample <- sample_n(data09, 14000, replace=FALSE)
data10Sample <- sample_n(data10, 14000, replace=FALSE)
data11Sample <- sample_n(data11, 10000, replace=FALSE)
data12Sample <- sample_n(data12, 7000, replace=FALSE)




data01Sample <- processTime(data01Sample)
data02Sample <- processTime(data02Sample)
data03Sample <- processTime(data03Sample)
data04Sample <- processTime(data04Sample)
data05Sample <- processTime(data05Sample)
data06Sample <- processTime(data06Sample)
data07Sample <- processTime(data07Sample)
data08Sample <- processTime(data08Sample)
data09Sample <- processTime(data09Sample)
data10Sample <- processTime2(data10Sample)
data11Sample <- processTime2(data11Sample)
data12Sample <- processTime2(data12Sample)

sumDataPre <- rbind(data01Sample, data02Sample)
sumDataPre <- rbind(data03Sample, sumDataPre)
sumDataPre <- rbind(data04Sample, sumDataPre)
sumDataPre <- rbind(data05Sample, sumDataPre)
sumDataPre <- rbind(data06Sample, sumDataPre)
sumDataPre <- rbind(data07Sample, sumDataPre)
sumDataPre <- rbind(data08Sample, sumDataPre)
sumDataPre <- rbind(data09Sample, sumDataPre)
sumDataPre <- rbind(data10Sample, sumDataPre)
sumDataPre <- rbind(data11Sample, sumDataPre)
sumDataPre <- rbind(data12Sample, sumDataPre)


sumDataPre$stLat <- sumDataPre$stLat
sumDataPre$stLon <- sumDataPre$stLon
sumDataPre$endLat <- sumDataPre$endLat
sumDataPre$endLon <- sumDataPre$endLon
sumDataPre$tripDuration <- sumDataPre$tripduration

sumDataPre$`start station id` <- NULL
sumDataPre$`end station id` <- NULL
sumDataPre$`start station latitude` <- NULL
sumDataPre$`start station longitude` <- NULL
sumDataPre$`tripduration` <- NULL


sumDataPre$DATElt <- as.POSIXlt(sumDataPre$start)
sumDataPre$mon <- sumDataPre$DATElt$mon+1
sumDataPre$hour <- hour(ymd_hms(sumDataPre$start))

sumDataPre$DATElt <- NULL



sumDataPreSel <- select(sumDataPre, stLat, stLon, endLat, endLon, weekend, hour, morning, evening, night, precipitation, minTmp, maxTmp,mon)


sumData <- group_by(data01Sample, date) %>%
  summarise(., count=n(), precipitation=mean(precipitation), maxTmp=mean(maxTmp), minTmp=mean(minTmp))
sumData$weekend <- sapply(sumData$date, isWeekend)

#bind data
sumData <- processData(data02Sample, sumData)
sumData <- processData(data03Sample, sumData)
sumData <- processData(data04Sample, sumData)
sumData <- processData(data05Sample, sumData)
sumData <- processData(data06Sample, sumData)
sumData <- processData(data07Sample, sumData)
sumData <- processData(data08Sample, sumData)
sumData <- processData(data09Sample, sumData)
sumData <- processData(data10Sample, sumData)
sumData <- processData(data11Sample, sumData)
sumData <- processData(data12Sample, sumData)

sumData$avgTmp <- (sumData$maxTmp + sumData$minTmp)/2;






sumYear <- rbind(data01Sample,data02Sample)
sumYear <- rbind(data03Sample, sumYear)
sumYear <- rbind(data04Sample, sumYear)
sumYear <- rbind(data05Sample, sumYear)
sumYear <- rbind(data06Sample, sumYear)
sumYear <- rbind(data07Sample, sumYear)
sumYear <- rbind(data08Sample, sumYear)
sumYear <- rbind(data09Sample, sumYear)
sumYear <- rbind(data10Sample, sumYear)
sumYear <- rbind(data11Sample, sumYear)
sumYear <- rbind(data12Sample, sumYear)



sumYear$times <- as.POSIXct(strftime(ymd_hms(sumYear$start), format="T"), format="T")
sumYear$jitter_times <- sumYear$start+minutes(round(runif(nrow(sumYear),min=0,max=59)))
sumYear$day <- wday(ymd_hms(sumYear$start), label=TRUE)

sumYear$hour <- hour(ymd_hms(sumYear$start))
sumYear$day   <- wday(ymd_hms(sumYear$start), label=TRUE)
sumYear$time <- as.POSIXct(strftime(ymd_hms(sumYear$start), format="%H"), format="%H")


sumYear %>%
  group_by(day,time) %>%
  count() %>%
  ggplot() +aes(time, n, color = day) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")

sumYear2 <- processData2(sumYear)




sumDataMonth <- sumData
sumDataMonth$DATElt <- as.POSIXlt(sumDataMonth$date)
sumDataMonth$mon <- sumDataMonth$DATElt$mon+1
sumDataMonth <- select(sumDataMonth, count, mon)
sumDataMonth <- group_by(sumDataMonth, mon) %>% summarise(., sum(count))
sumDataMonth[1,] <-  c(1, nrow(data01))
sumDataMonth[2,] <-  c(2, nrow(data02))
sumDataMonth[3,] <-  c(3, nrow(data03))
sumDataMonth[4,] <-  c(4, nrow(data04))
sumDataMonth[5,] <-  c(5, nrow(data05))
sumDataMonth[6,] <-  c(6, nrow(data06))
sumDataMonth[7,] <-  c(7, nrow(data07))
sumDataMonth[8,] <-  c(8, nrow(data08))
sumDataMonth[9,] <-  c(9, nrow(data09))
sumDataMonth[10,] <-  c(10, nrow(data10))
sumDataMonth[11,] <-  c(11, nrow(data11))
sumDataMonth[12,] <-  c(12, nrow(data12))




json_data <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")

n <- length(json_data[[1]])
 #DF <- structure(json_data, row.names = c(NA, -n), class = "data.frame")
 bikeTest2 <- json_data$data

 bikeTest2 <- lapply(bikeTest2, function(station){within(station, { 
  name <- NULL
  short_name <- NULL
  region_id <- NULL
  rental_methods <- NULL
  rental_url <- NULL
  eightd_has_key_dispenser <- NULL
  capacity <- NULL
  station_id <- as.numeric(station_id)
  
 })})
 

bikeTest2 <- bikeTest2[[1]]







names(df) = c("bikes", "name", "idx", "id", "free", "number", "popupData", "time", "day", "availability", "stat", "longitude", "latitude")


