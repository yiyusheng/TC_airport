#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: dataLoad.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-09 10:26:33
#
# Last   modified: 2016-10-09 10:26:42

rm(list = ls())
source('head.R')

tsFormat <- function(ts,tz = 'UTC'){
  tmp <- paste(substr(ts,1,16),'00',sep='-')
  tmp <- as.POSIXct(tmp,tz = tz, format = '%Y-%m-%d-%H-%M-%S')
}

# S1.load data
data.ap <- read.csv(file.path(dir_data,'WIFI_AP_Passenger_Records_chusai_1stround.csv'))
data.securecheck <- read.csv(file.path(dir_data,'airport_gz_security_check_chusai_1stround.csv'))
data.departure <- read.csv(file.path(dir_data,'airport_gz_departure_chusai_1stround.csv'))
data.flights <- read.csv(file.path(dir_data,'airport_gz_flights_chusai_1stround.csv'))
data.gates <- read.csv(file.path(dir_data,'airport_gz_gates.csv'))

# S2.Time format
data.ap$timeStamp <- tsFormat(data.ap$timeStamp)

data.securecheck$security_time <- as.POSIXct(data.securecheck$security_time,tz = 'UTC')

data.departure$flight_time <- as.POSIXct(data.departure$flight_time,tz = 'UTC',format = '%Y/%m/%d %H:%M:%S')
data.departure$checkin_time <- as.POSIXct(data.departure$checkin_time,tz = 'UTC',format = '%Y/%m/%d %H:%M:%S')

data.flights$scheduled_flt_time <- fct2ori(data.flights$scheduled_flt_time)
data.flights$actual_flt_time <- fct2ori(data.flights$actual_flt_time)

data.flights$scheduled_flt_time[nchar(data.flights$scheduled_flt_time) < 10] <- 
  paste(data.flights$scheduled_flt_time[nchar(data.flights$scheduled_flt_time) < 10],' 0:00:00',sep='')
data.flights$actual_flt_time[nchar(data.flights$actual_flt_time) < 10 & nchar(data.flights$actual_flt_time) > 0] <- 
  paste(data.flights$actual_flt_time[nchar(data.flights$actual_flt_time) < 10 & nchar(data.flights$actual_flt_time) > 0],' 0:00:00',sep='')

data.flights$scheduled_flt_time <- as.POSIXct(data.flights$scheduled_flt_time,tz = 'UTC',format = '%Y/%m/%d %H:%M:%S') + 8*3600
data.flights$actual_flt_time <- as.POSIXct(data.flights$actual_flt_time,tz = 'UTC',format = '%Y/%m/%d %H:%M:%S') + 8*3600

# S3.AP area and ID
data.ap$floor <- factor(substr(fct2ori(data.ap$WIFIAPTag),4,4))
data.ap$area <- substr(fct2ori(data.ap$WIFIAPTag),1,2)
data.ap$area[data.ap$area == 'Ec'] <- 'EC'
data.ap$area <- factor(data.ap$area)
data.ap$areaNew <- factor(paste(data.ap$area,data.ap$floor,sep='-'))

tmp <- sort(levels(data.ap$WIFIAPTag))
tmp <- data.frame(WIFIAPTag = tmp,wid = 1:length(tmp))
data.ap$wid <- tmp$wid[match(data.ap$WIFIAPTag,tmp$WIFIAPTag)]

# S4.sum pc of each AP during 10 mins.
mmt.start <- as.POSIXct('2016-09-10 19:00:00',tz = 'UTC')
cut.point <- mmt.start + 60*10*(0:((difftime(max(data.ap$timeStamp),mmt.start,tz = 'UTC',units = 'mins')/10)+1))
data.ap$ts10min <- cut.POSIXt(data.ap$timeStamp,cut.point)
data.ap <- factorX(subset(data.ap,timeStamp >= mmt.start))

data.ap10min <- melt(tapply(data.ap$passengerCount,list(data.ap$WIFIAPTag,data.ap$ts10min),sum))
names(data.ap10min) <- c('WIFIAPTag','timeStamp','passengerCount')

data.ap10min$timeStamp <- as.POSIXct(fct2ori(data.ap10min$timeStamp),tz = 'UTC')
data.ap10min$floor <- factor(substr(fct2ori(data.ap10min$WIFIAPTag),4,4))

data.ap10min$area <- factor(substr(fct2ori(data.ap10min$WIFIAPTag),1,2))
data.ap10min$area[data.ap10min$area == 'Ec'] <- 'EC'
data.ap10min$area <- factor(data.ap10min$area)

data.ap10min$areaNew <- factor(paste(data.ap10min$area,data.ap10min$floor,sep='-'))

data.ap10min <- factorX(subset(data.ap10min,!is.na(passengerCount)))
data.ap$ts10min <- NULL

tmp <- sort(levels(data.ap10min$WIFIAPTag))
tmp <- data.frame(WIFIAPTag = tmp,wid = 1:length(tmp))
data.ap10min$wid <- tmp$wid[match(data.ap10min$WIFIAPTag,tmp$WIFIAPTag)]

# S4.Save
split.dataAP <- split(data.ap,data.ap$WIFIAPTag)
split.dataAP10min <- split(data.ap10min,data.ap10min$WIFIAPTag)
save(data.ap,split.dataAP,
     data.ap10min,split.dataAP10min,
     data.securecheck,
     data.departure,data.flights,data.gates,
     file = file.path(dir_data,'dataLoad.Rda'))
