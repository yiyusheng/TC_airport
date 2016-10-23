#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: external_ftr.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-15 20:00:24
#
# Last   modified: 2016-10-15 20:05:09
#
#
#

ext_all <- function(i,trainF,testF){
  tr.d <- melt(trainF[i,],id.vars = 'WIFIAPTag')
  names(tr.d) <- c('WIFIAPTag','timeStamp','passengerCount')
  tr.d$timeStamp <- as.p(tr.d$timeStamp)
  tr.ftr <- add_ftr_ap(data.ext,tr.d)
  tr.ftr$target <- as.numeric(trainF[i,2:ncol.train])
  fit <- lm(target ~ .,data = tr.ftr)
  
  te.d <- melt(testF[i,],id.vars = 'WIFIAPTag')
  names(te.d) <- c('WIFIAPTag','timeStamp','passengerCount')
  te.d$timeStamp <- as.p(te.d$timeStamp)
  te.ftr <- add_ftr_ap(data.ext,te.d)
  
  forecast(fit,te.ftr)$mean
}

add_ftr_ap <- function(data.ext,data){
  cat(sprintf('[areaPCFunc::add_ftr_ap]AP: %s\n',data$WIFIAPTag[1]))
  r <- lapply(data$timeStamp,gen_ftr_area_ts,data.ext)
  r <- do.call(rbind,r)
  r <- data.frame(r)
  r$WIFIAPTag <- fct2ori(data$WIFIAPTag[1])
  r$timeStamp <- data$timeStamp
  r
}

format_num <- function(int,hour,ftrX){
  tmp <- data.frame(id = 1:(hour*60/int))
  tmp$value <- ftrX$value[match(tmp$id,ftrX$Var1)]
  tmp$value[is.na(tmp$value)] <- 0
  tmp$value
}

gen_ftr_area_ts <- function(ts,data.ext){
  # cat(sprintf('[areaPCFunc::gen_ftr_area_ts]ts: %s\n',ts))
  # Ftr1. time between ts and checkin/flight
  data.ext$fromCheckIn <- as.numeric(difftime(ts,data.ext$checkin_time,tz = 'UTC',units = 'mins'))
  data.ext$toFlight <- as.numeric(difftime(data.ext$actual_flt_time,ts,tz = 'UTC',units = 'mins'))
  
  int.from <- 60;hour.from <- 6
  int.to <- 60;hour.to <- 6
  
  data.ext$cutFCI <- ceiling(data.ext$fromCheckIn/int.from)
  data.ext$cutFLT <- ceiling(data.ext$toFlight/int.to)
  data.ext <- subset(data.ext,cutFCI > 0 & cutFLT > 0)
  ftrFCI <- melt(table(data.ext$cutFCI))
  ftrFLT <- melt(table(data.ext$cutFLT))
  
  # format
  tmp1 <- c(format_num(int.from,hour.from,ftrFCI),
            format_num(int.to,hour.to,ftrFLT))
  
  # Ftr2. Hours
  tmp2 <- as.numeric(format(ts,format = '%H'))
  
  # Ftr3. area id
  tmp <- melt(table(data.ext$BGATE_AREA))
  tmp3 <- data.frame(area = c('E1','E2','E3','W1','W2','W3'))
  tmp3$count <- tmp$value[match(tmp3$area,tmp$Var1)]
  
  ftr <- c(tmp1,tmp2,tmp3$count)
  ftr
}

area <- 'W1';ts <- as.POSIXct('2016-09-14 16:25:00',tz = 'UTC')
right.flight <- revise_data(data.departure,data.flights,data.securecheck)

get_ftr.area <- function(area,ts,data.departure,data.securecheck,data.flights,data.gates){
  data.dep <- factorX(subset(data.departure,flight_ID %in% right.flight$flight_ID))
  data.sec <- factorX(subset(data.securecheck,flight_ID %in% right.flight$flight_ID))
  
  data.flights$BGATE_AREA <- data.gates$BGATE_AREA[match(data.flights$BGATE_ID,data.gates$BGATE_ID)]
  data.dep <- merge(data.dep,data.flights,by.x = c('flight_ID','flight_time'),
                          by.y = c('flight_ID','scheduled_flt_time'),all.x = T)
  
  data.dep$flight_date <- as.Date(data.dep$flight_time)
  data.sec$security_date <- as.Date(data.sec$security_time)
  data.flights$flight_date <- as.Date(data.flights$scheduled_flt_time)
  
  flightInfo <- right.flight
  
  ts.plus <- ts + 3600*3
  ts.minus <- ts - 3600*3
  
  flight.inarea.plan <- factorX(subset(data.flights,BGATE_AREA == area & flight_date == as.Date(ts) &
                                    scheduled_flt_time >= ts & scheduled_flt_time < ts.plus))
  flight.inarea.plan$flight_date_minus <- flight.inarea.plan$flight_date
  flight.inarea.plan <- merge(flight.inarea.plan,
                              flightInfo[,c('flight_ID','flight_date','passengerCount','passengerCount.sec')],
                              by.x = c('flight_ID','flight_date_minus'),by.y = c('flight_ID','flight_date'),
                              all.x = T)

  flight.inarea.act <- factorX(subset(data.flights,BGATE_AREA == area & flight_date == as.Date(ts) &
                                        actual_flt_time >= ts & actual_flt_time < ts.plus))
  flight.inarea.act <- merge(flight.inarea.act,
                             flightInfo[,c('flight_ID','flight_date','passengerCount.x','passengerCount.y')],
                              by = c('flight_ID','flight_date'))
  get_flight_detail(flight_ID,date,data.dep,data.sec)

}

get_flight_detail <- function(fid,dt,data.departure,data.securecheck){
  fid <- as.character(flight.inarea.plan$flight_ID[20])
  dt <- as.Date(ts)
  
  tmp.departure <- subset(data.departure,flight_ID == fid & flight_date == dt)
}