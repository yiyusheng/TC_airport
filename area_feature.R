#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: area_feature.R
#
# Description: generate feature for each area(E + W) and time
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-18 10:39:03
#
# Last   modified: 2016-10-18 10:39:07

# ts.test <- seq.POSIXt(as.p(factor('2016-09-25 15:00:00')),as.p(factor('2016-09-25 18:00:00')),600)
# ts.test <- ts.test[-length(ts.test)]
# 
# ts.train <- seq.POSIXt(as.p(factor('2016-09-10 19:00:00')),as.p(factor('2016-09-25 15:00:00')),600)
# ts.train <- ts.train[-length(ts.train)]
# 
# tsSet <- ts.train

get_area_feature <- function(tsSet,data.flights,data.gates,n = 5,attr,attrTime){
  data.flights$BGATE_ID <- gsub('.*,','',data.flights$BGATE_ID)
  data.flights$BGATE_ID[data.flights$BGATE_ID == ''] <- 'unknown_gate'
  
  data.flights$BGATE_AREA <- as.character(data.gates$BGATE_AREA[match(data.flights$BGATE_ID,
                                                                      data.gates$BGATE_ID)])
  data.flights$BGATE_AREA[is.na(data.flights$BGATE_AREA)] <- 'unknown_area'
  
  data.flights$BGATE_AREA <- factor(data.flights$BGATE_AREA)
  data.flights$BGATE_ID <- factor(data.flights$BGATE_ID)
  data.flights <- factorX(data.flights)
  
  ftr.area <- lapply(tsSet,group_flight_feature,data.flights,n,attr,attrTime)
  ftr.area <- data.frame(do.call(rbind,ftr.area))
  
  tmp <- c(levels(data.flights$BGATE_AREA),levels(data.flights$BGATE_ID))
  names(ftr.area) <- paste(rep(tmp,(n+1)),rep(0:n,each = length(tmp)),sep='#')
  ftr.area$timeStamp <- tsSet
  
  ftr.area
}

group_flight_feature <- function(ts,data.flights,n = 5,attr,attrTime){
  num_flight <- lapply(0:n,num_flight_hour,ts,data.flights,attr,attrTime)
  num_flight <- do.call(c,num_flight)
  ftr <- num_flight
}

num_flight_hour <- function(n,ts,data.flights,attr,attrTime){
  data.fli <- data.flights[data.flights[[attrTime]] >= ts + n * 3600 & data.flights[[attrTime]] < ts + 3600 * (n+1),]
  sta.area <- melt(tapply(data.fli[[attr]],data.fli$BGATE_AREA,sum))
  sta.gate <- melt(tapply(data.fli[[attr]],data.fli$BGATE_ID,sum))
  sta.area$value[is.na(sta.area$value)] <- 0
  sta.gate$value[is.na(sta.gate$value)] <- 0
  
  c(sta.area$value,sta.gate$value)
}

# mean passengerCount of flight/area/gate
avg_psg <- function(df,data.flights){
  names(df)[names(df) == 'security_time' | names(df) == 'checkin_time'] <- 'dtime'
  df <- subset(df,dtime >= as.p(factor('2016-09-11')) & dtime < as.p(factor('2016-09-26')))
  df$flt_date <- as.Date(df$dtime)
  
  # How many passengers per flight in each day
  tmp <- melt(table(df$flight_ID,df$flt_date))
  names(tmp) <- c('flight_ID','date','passengerCount')
  tmp$date <- as.Date(tmp$date)
  # Filter data on date with uncomplete records
  tmp <- factorX(subset(tmp,passengerCount != 0 & 
                                  date != as.Date('2016-09-25') & date != as.Date('2016-09-10')))
  
  # calculate the mean passengercount for each flight
  tmp1 <- melt(tapply(tmp$passengerCount,tmp$flight_ID,function(x){
    # if(length(x) <= 3)return(0)
    tmp <- quantile(x)
    mean(tmp[-c(1,10)])
  }))
  names(tmp1) <- c('flight_ID','meanPsg')
  
  # Because we only have flight data to do prediction. so flight not in flight table is useless. We add passengerCount for each flight
  tmp1 <- factorX(subset(tmp1,meanPsg != 0 & flight_ID %in% data.flights$flight_ID))
  data.flights$meanPsg <- tmp1$meanPsg[match(data.flights$flight_ID,tmp1$flight_ID)]
  
  # calculate for each gate/area
  data.fli <- factorX(subset(data.flights,!is.na(meanPsg)))
  meanPsg.gate <- melt(tapply(data.fli$meanPsg,data.fli$BGATE_ID,mean))
  meanPsg.gate <- meanPsg.gate[-1,]
  meanPsg.area <- melt(tapply(data.fli$meanPsg,data.fli$BGATE_AREA,mean))
  names(meanPsg.gate) <- c('BGATE_ID','meanPsg')
  names(meanPsg.area) <- c('BGATE_AREA','meanPsg')
  
  idx.addgate <- is.na(data.flights$meanPsg)
  data.flights$meanPsg[idx.addgate] <- meanPsg.gate$meanPsg[match(data.flights$BGATE_ID[idx.addgate],meanPsg.gate$BGATE_ID)]
  
  idx.addarea <- is.na(data.flights$meanPsg)
  data.flights$meanPsg[idx.addarea] <- meanPsg.area$meanPsg[match(data.flights$BGATE_AREA[idx.addarea],meanPsg.area$BGATE_AREA)]
  
  idx.addunknow <- is.na(data.flights$meanPsg)
  data.flights$meanPsg[idx.addunknow] <- 150
  data.flights
}

# mean time of delay for each flight/area/gate
add_delay <- function(data.flights){
  df <- factorX(subset(data.flights,scheduled_flt_time >= as.p('2016-09-11') & scheduled_flt_time <= as.p('2016-09-26')))
  df$delay <- as.numeric(difftime(df$actual_flt_time,df$scheduled_flt_time,units = 'mins'))
  df <- factorX(subset(df,delay >= 0 | is.na(delay)))
  
  df.test <- factorX(subset(df,is.na(delay)))
  df.train <- factorX(subset(df,!is.na(delay)))
  
  delay.train <- melt(tapply(df.train$delay,df.train$flight_ID,mean))
  delay.area <- melt(tapply(df.train$delay,df.train$BGATE_AREA,mean))
  df.test$delay <- delay.train$value[match(df.test$flight_ID,delay.train$Var1)]
  df.test$delay[is.na(df.test$delay)] <- mean(delay.train$value)
  
  df.test$actual_flt_time <- df.test$scheduled_flt_time + df.test$delay*60
  rbind(df.test,df.train)
}