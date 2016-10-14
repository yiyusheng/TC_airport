#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: areaPCFunc.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-13 16:47:17
#
# Last   modified: 2016-10-13 16:47:19

# F1. generate data.departure
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

# F2. generate pc for pair(area,timeStamp)
gen_pc_area_ts <- function(df){
  df <- factorX(df)
  df$timeStamp <- factor(df$timeStamp)
  tmp <- data.frame(area = df$area[1],
                    timeStamp = levels(df$timeStamp),
                    passengerCount = as.numeric(tapply(df$passengerCount,df$timeStamp,sum)))
  tmp$timeStamp <- as.POSIXct(fct2ori(tmp$timeStamp),tz = 'UTC')
  tmp
}

# F3. Add ftr of all ts together
add_ftr_area_ts <- function(data.psg,data.area){
  r <- lapply(data.area$timeStamp,gen_ftr_area_ts,data.psg)
  r <- do.call(rbind,r)
  r <- data.frame(r)
}

format_num <- function(int,hour,ftrX){
  tmp <- data.frame(id = 1:(hour*60/int))
  tmp$value <- ftrX$value[match(tmp$id,ftrX$Var1)]
  tmp$value[is.na(tmp$value)] <- 0
  tmp$value
}

# F4. generate feature for pair(area,timeStamp) to predict pc for each (area,timeStamp)
gen_ftr_area_ts <- function(ts,data.psg,int.ftr1 = 20,hour.ftr1 = 5){
  cat(sprintf('[areaPCFunc::gen_ftr_area_ts]ts: %s\n',ts))
  # Ftr1. time between ts and checkin/flight
  data.psg$fromCheckIn <- as.numeric(difftime(ts,data.psg$checkin_time,tz = 'UTC',units = 'mins'))
  data.psg$toFlight <- as.numeric(difftime(data.psg$actual_flt_time,ts,tz = 'UTC',units = 'mins'))
  
  int.from <- 180;hour.from <- 6
  int.to <- 60;hour.to <- 2
  
  data.psg$cutFCI <- ceiling(data.psg$fromCheckIn/int.from)
  data.psg$cutFLT <- ceiling(data.psg$toFlight/int.to)
  data.psg <- subset(data.psg,cutFCI > 0 & cutFLT > 0)
  ftrFCI <- melt(table(data.psg$cutFCI))
  ftrFLT <- melt(table(data.psg$cutFLT))
  
  # format
  tmp1 <- c(format_num(int.from,hour.from,ftrFCI),
            format_num(int.to,hour.to,ftrFLT))
  
  # Ftr2. Hours
  tmp2 <- as.numeric(format(ts,format = '%H'))
  
  # Ftr3. area id
  tmp <- melt(table(data.psg$BGATE_AREA))
  tmp3 <- data.frame(area = c('E1','E2','E3','W1','W2','W3'))
  tmp3$count <- tmp$value[match(tmp3$area,tmp$Var1)]
  
  ftr <- c(tmp1,tmp2,
           tmp3$count)
  ftr
}

# F5. generate trainset and testset with area_pc and 
gen_train_test <- function(data.area,data.psg,t1,t2,t3,t4){
  trainIdx <- which(data.area$timeStamp >= t1 & data.area$timeStamp < t2)
  testIdx <- which(data.area$timeStamp >= t3 & data.area$timeStamp <= t4)
  
  trainftr <- add_ftr_area_ts(data.psg,data.area[trainIdx,])
  testftr <- add_ftr_area_ts(data.psg,data.area[testIdx,])
  
  len_ftrcol <- ncol(trainftr)
  train_data <- cbind(trainftr,data.area$passengerCount[trainIdx])
  test_data <- cbind(testftr,data.area$passengerCount[testIdx])
  
  names(train_data) <- c(names(train_data)[1:len_ftrcol],'pc')
  names(test_data) <- c(names(test_data)[1:len_ftrcol],'pc')
  
  list(train_data,test_data,testIdx)
}


