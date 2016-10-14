#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: areaPC_pred.R
#
# Description: use area strength to predict each aps in this area and weight pc of each ap by its mean pc.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-10 09:44:26
#
# Last   modified: 2016-10-13 20:06:49

rm(list = ls())
source('head.R')
source('dirFunc.R')
load(file.path(dir_data,'dataLoad.Rda'))
load(file.path(dir_data,'areaPC.Rda'))

# Add arrival time which is 20 minutes after check in
get_arr_time <- function(df,lag){
  df <- factorX(df)
  df$arrTime <- mapply(min,(df$checkin_time + lag*60),df$actual_flt_time)
  df$arrTime <- as.POSIXct.numeric(df$arrTime,tz = 'UTC',origin = '1970-01-01')
}

round_ts <- function(start,end){
  round.start <- as.POSIXct.numeric(floor(as.numeric(start)/600)*600,
                                    tz = 'UTC',origin = '1970-01-01')
  round.end <- as.POSIXct.numeric(ceiling(as.numeric(end)/600)*600,
                                  tz = 'UTC',origin = '1970-01-01')
  list(round.start,round.end)
}

numPC_area <- function(df,start = as.POSIXct('2016-09-10 19:00:00',tz = 'UTC'),
                       end = as.POSIXct('2016-09-14 15:00:00',tz = 'UTC')){
  ts <- seq.POSIXt(start,end,by = 60*10)
  numPC <- data.frame(timeStamp = ts,passengerCount = 0)
  
  for(i in 1:nrow(df)){
    numPC$passengerCount[numPC$timeStamp >= df$roundStart[i] & numPC$timeStamp <= df$roundEnd[i]] <- 
      numPC$passengerCount[numPC$timeStamp >= df$roundStart[i] & numPC$timeStamp <= df$roundEnd[i]] + 1
  }
  numPC$class <- df$BGATE_AREA[1]
  numPC
}

gen_data.depa <- function(data.departure,data.flights,data.gates){
  # S1. Add info for table data.departure
  
  # Add area for flight
  data.flights$BGATE_AREA <- data.gates$BGATE_AREA[match(data.flights$BGATE_ID,data.gates$BGATE_ID)]
  # Add area,actual time for departure
  data.departure <- merge(data.departure,data.flights,by.x = c('flight_ID','flight_time'),
                          by.y = c('flight_ID','scheduled_flt_time'),all.x = T)
  
  #remove na for flight_time and use flight_time as the actual time for those without actual time
  data.depWithTime <- subset(data.departure,!is.na(flight_time))
  data.depWithTime$actual_flt_time[is.na(data.depWithTime$actual_flt_time)] <- 
    data.depWithTime$flight_time[is.na(data.depWithTime$actual_flt_time)]
  
  # S2. Filter some wrong passenger items and round time for arrival and flight
  # Calculate rest time between check in and departure and filter passengers 
  # with rest time less than 0 or larger than 1091(95% quantile).
  
  data.depWithTime$restTime <- as.numeric(difftime(data.depWithTime$actual_flt_time,
                                                   data.depWithTime$checkin_time,units = 'mins'))
  data.depWithTime <- factorX(subset(data.depWithTime,restTime >= 0 & 
                                       # restTime < 1440 & 
                                       !is.na(BGATE_AREA)))
  data.depWithTime
}
####################################



data.depWithTime$arrTime <- get_arr_time(data.depWithTime,20)
tmp <- round_ts(data.depWithTime$arrTime,data.depWithTime$actual_flt_time)
data.depWithTime$roundStart <- tmp[[1]]
data.depWithTime$roundEnd <- tmp[[2]]

# S2.simulate passengers' arrival of each area
split.departure <- split(data.depWithTime,data.depWithTime$BGATE_AREA)
area.pc.pred <- lapply(split.departure,numPC_area)
area.pc.pred <- do.call(rbind,area.pc.pred)

# S3.calculate the real number of passengers from data.ap
split.dataArea <- split(data.ap10min,data.ap10min$area)
area.pc.real <- lapply(split.dataArea,function(df){
  tmp <- tapply(df$passengerCount,df$timeStamp,sum)
  tmp <- data.frame(timeStamp = as.POSIXct(names(tmp),tz = 'UTC'),
                    passengerCount = as.numeric(tmp),
                    class = df$area[1])
})
area.pc.real <- do.call(rbind,area.pc.real)

save(area.pc.pred,area.pc.real,file = file.path(dir_data,'areaPC.Rda'))

# S4.evaluate the simulate one and the real one.
area.pc.merge <- merge(area.pc.pred,area.pc.real,by = c('class','timeStamp'),all.x = T)
area.pc.merge$rate <- area.pc.merge$passengerCount.x/area.pc.merge$passengerCount.y

area.pc.pred$group <- 'pred'
area.pc.real$group <- 'real'
area.pc.comp <- rbind(area.pc.pred,area.pc.real)
ggplot(subset(area.pc.comp,class == 'E1'),
       aes(x = timeStamp,y = passengerCount,group = group,color = group)) + geom_line()
