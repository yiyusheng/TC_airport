#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: util.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-15 15:15:57
#
# Last   modified: 2016-10-15 15:16:00

rawTrain <- function(int = 10,
                     start = as.POSIXct('2016-09-10 19:00:00',tz = 'UTC'),
                     end = as.POSIXct('2016-09-25 12:00:00',tz = 'UTC')){
  ifelse(int == 10,r <- data.ap10min,r <- data.ap)
  subset(r,timeStamp >= start & timeStamp < end)
}

rawTestA <- function(int = 10,
                     start = as.POSIXct('2016-09-25 12:00:00',tz = 'UTC'),
                     end = as.POSIXct('2016-09-25 15:00:00',tz = 'UTC')){
  ifelse(int == 10,r <- data.ap10min,r <- data.ap)
  subset(r,timeStamp >= start & timeStamp < end)
}

rawTestB <- function(int = 10,
                     start = as.POSIXct('2016-09-14 15:00:00',tz = 'UTC'),
                     end = as.POSIXct('2016-09-14 18:00:00',tz = 'UTC')){
  ifelse(int == 10,r <- data.ap10min,r <- data.ap)
  subset(r,timeStamp >= start & timeStamp < end)
}

get_ap_position <- function(df){
  tmp <- gsub('Ec','EC',df$WIFIAPTag)
  df$block <- substr(tmp,1,5)
  df$floor <- substr(tmp,1,4)
  df$area <- substr(tmp,1,2)
  df
}

error_position <- function(df){
  error.block <- melt(tapply(df$cumError,df$block,function(x)sum(x,na.rm = T)/length(x)))
  error.floor <- melt(tapply(df$cumError,df$floor,function(x)sum(x,na.rm = T)/length(x)))
  error.area <- melt(tapply(df$cumError,df$area,function(x)sum(x,na.rm = T)/length(x)))
  list(error.block,error.floor,error.area)
}

plot_comp_matrix <- function(APtag,ori = test.ori,pred = r){
  ori <- subset(ori,WIFIAPTag == APtag)
  pred <- dcast(pred[pred$WIFIAPTag == APtag,c('WIFIAPTag','timeStamp','pred')],
                WIFIAPTag ~ timeStamp,value.var = 'pred')
  
  its.time <- sort(as.p(factor(intersect(names(ori)[-1],names(pred[-1])))))
  tmp <- data.frame(WIFIAPTag = ori$WIFIAPTag[1],
                    timeStamp  = its.time,
                    passengerCount = as.numeric(ori[1,as.character(its.time)]),
                    pred = as.numeric(pred[1,as.character(its.time)]))
  tmp$error <- error_calc(tmp)
  cat(sprintf('Cumsum of %s: %.2f\n',tmp$WIFIAPTag[1],sum(tmp$error)))
  plot_comp(tmp)
}

raw.flight <- function(data.flights,data.gates,psg.flight){
  data.flights$BGATE_AREA <- data.gates$BGATE_AREA[match(data.flights$BGATE_ID,data.gates$BGATE_ID)]
  data.flights$subcol <- factor(paste(data.flights$scheduled_flt_time,
                                      data.flights$actual_flt_time,data.flights$BGATE_ID,sep='-'))
  data.flights$inpsg <- 2
  data.flights$inpsg[data.flights$flight_ID %in% psg.flight] <- 1
  data.flights <- factorX(data.flights)
  
  # find the shared code flights for one day and select a main one(priori to select flight in the data.departure table as the main flight)
  tmp <- by(data.flights,data.flights$subcol,function(x){
    x <- x[order(x$inpsg,x$flight_ID),]
    fid <- fct2ori(x$flight_ID[1])
    
    tmp <- x[1,]
    
    # collect the shared one
    tmp1 <- setdiff(x$flight_ID,fid)
    if(length(tmp1) == 0)tmp1 <- 'Only'
    
    share.fli <- data.frame(main = fid,share = tmp1,date = x$scheduled_flt_time[1])
    list(fli = fct2oriX(tmp),share = fct2oriX(share.fli))
  })
  data.fli <- do.call(rbind,lapply(tmp,function(x)x$fli))
  data.flishare <- do.call(rbind,lapply(tmp,function(x)x$share))
  
  data.fli$subcol <- NULL
  row.names(data.fli) <- NULL
  row.names(data.flishare) <- NULL
  data.flishare <- data.flishare[!duplicated(data.flishare[,c('main','share')]),]
  data.fli$flight_ID <- factor(data.fli$flight_ID)
  data.fli$BGATE_ID <- factor(data.fli$BGATE_ID)
  data.fli$BGATE_AREA <- factor(data.fli$BGATE_AREA)
  data.flishare$main <- factor(data.flishare$main)
  data.flishare$share <- factor(data.flishare$share)
  
  # detail change
  dup.fli <- factorX(subset(meltX(data.fli$flight_ID,as.Date(data.fli$scheduled_flt_time)),value != 1))
  data.fli$date <- as.Date(data.fli$scheduled_flt_time)
  data.fli <- merge(data.fli,dup.fli,by.x = c('flight_ID','date'),by.y = c('x','y'),all.x = T)
  dup.fli0 <- subset(data.fli,!is.na(value))
  data.fli <- subset(data.fli,is.na(value))
  
  allNeed <- c('CA3804','CA4351','HU7272')
  dup.fliA <- subset(dup.fli0,flight_ID %in% allNeed)
  dup.fliB <- subset(dup.fli0,!(flight_ID) %in% allNeed)
  dup.fliB <- subset(dup.fliB,BGATE_ID != '')
  dup.fliC <- factorX(rbind(dup.fliA,dup.fliB))
  
  data.fli <- factorX(rbind(dup.fliC,data.fli))
  data.fli$date <- NULL
  data.fli$inpsg <- NULL
  data.fli$value <- NULL

  # flight with two gate
  data.fli$BGATE_ID <- gsub('.*,','',data.fli$BGATE_ID)
  list(data.fli,data.flishare)
}



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

# F2.revise scheduled_flt_time for data.departure and data.flights
revise_data <- function(data.departure,data.flights,data.securecheck){
  data.flights$flight_date <- as.Date(data.flights$scheduled_flt_time)
  data.departure$checkin_date <- as.Date(data.departure$checkin_time)
  data.departure <- merge(data.departure,data.flights[,c('flight_ID','flight_date','scheduled_flt_time')],
                          by.x = c('flight_ID','checkin_date'),by.y = c('flight_ID','flight_date'),all.x = T)
  data.departure$flight_time[is.na(data.departure$flight_time)] <- 
    data.departure$scheduled_flt_time[is.na(data.departure$flight_time)]
  
  data.departure$flight_date <- as.Date(data.departure$flight_time)
  data.flights$actual_flt_date <- as.Date(data.flights$actual_flt_time)
  data.securecheck$security_date <- as.Date(data.securecheck$security_time)
  
  tmp <- melt(table(data.departure$flight_ID,data.departure$flight_date))
  names(tmp) <- c('flight_ID','flight_date','passengerCount')
  tmp <- factorX(subset(tmp,passengerCount != 0))
  tmp <- factorX(subset(tmp,as.p(flight_date) < as.p(factor('2016-09-15'))))
  tmp <- join(tmp,data.flights[,c('flight_ID','scheduled_flt_time','actual_flt_time','flight_date')],
              by = c('flight_ID','flight_date'))
  
  tmp1 <- tapply(tmp$flight_date,tmp$flight_ID,function(x)length(x) == length(unique(x)))
  tmp1 <- names(tmp1)[tmp1]
  
  tmp.sec <- melt(table(data.securecheck$flight_ID,data.securecheck$security_date))
  names(tmp.sec) <- c('flight_ID','flight_date','passengerCount')
  tmp.sec <- factorX(subset(tmp.sec,passengerCount != 0))
  
  tmp <- merge(tmp,tmp.sec,by = c('flight_ID','flight_date'))
  names(tmp) <- c('flight_ID','flight_date','passengerCount',
                  'scheduled_flt_time','actual_flt_time','passengerCount.sec')
  
  right.flight <- factorX(subset(tmp,flight_ID %in% tmp1))
  wrong.flight <- factorX(subset(tmp,!(flight_ID %in% tmp1)))
  right.flight <- right.flight[order(right.flight$flight_ID,right.flight$flight_date),]
  save(right.flight,wrong.flight,file = file.path(dir_data,'flightInfo.Rda'))
  right.flight
}

# estimate a poison process for each flight in each day.
flt <- '3U8738';dt <- as.Date('2016-09-12')
est_poison <- function(flt,dt){
  dep <- subset(data.departure,flight_ID == flt & as.Date(flight_time) == dt)
  sec <- subset(data.securecheck,flight_ID == flt & as.Date(security_time) == dt)
  
  dep <- subset(dep,checkin_time < flight_time)
  sec <- subset(sec,security_time < dep$flight_time[1])
  
  #dep
  dep$resTime <- round(240 - as.numeric(difftime(dep$flight_time,dep$checkin_time,units = 'mins')))
  dep <- subset(dep,resTime >= 0)
  
  tmp <- melt(table(dep$resTime))
  names(tmp) <- c('name','count')
  
  dep.process <- data.frame(ts = 1:240)
  dep.process <- merge(dep.process,tmp,by.x = 'ts',by.y = 'name',all.x = T)
  dep.process$count[is.na(dep.process$count)] <- 0
  dep.process$cumCount <- cumsum(dep.process$count)
  
  model.dep <- fitdistr(dep.process$cumCount,'poisson')
  
  lambda <- model.dep$estimate
  tMax <- 240
  
  ## find the number 'n' of exponential r.vs required by imposing that
  ## Pr{N(t) <= n} <= 1 - eps for a small 'eps'
  n <- qpois(1 - 1e-8, lambda = lambda * tMax)
  
  ## simulate exponential interarrivals the
  X <- rexp(n = n, rate = lambda)
  S <- c(0, cumsum(X))
  plot(x = S, y = 0:n, type = "s", xlim = c(0, tMax)) 

  #sec
  sec$resTime <- round(240 - as.numeric(difftime(dep$flight_time[1],sec$security_time,units = 'mins')))
  sec <- subset(sec,resTime >= 0)
  
  tmp <- melt(table(sec$resTime))
  names(tmp) <- c('name','count')
  
  sec.process <- data.frame(ts = 1:240,count = 0)
  sec.process$count <- tmp$count[match(sec.process$ts,tmp$name)]
  sec.process$count[is.na(sec.process$count)] <- 0
  sec.process$cumCount <- cumsum(sec.process$count)
  
  
}

plot_process <- function(dep.process,sec.process){
  dep.process$class <- 'dep'
  sec.process$class <- 'sec'
  tmp <- rbind(dep.process,sec.process)
  ggplot(tmp,aes(x = ts,y = cumCount,group = class,color = class)) + geom_line()
}