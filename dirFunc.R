#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: dirFunc.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-10 09:22:35
#
# Last   modified: 2016-10-10 09:22:38

# F1. extract data based on time between apTime and special moment
get_data_by_time <- function(apData,mmt,t1,t2,right = F){
  if(right == F){
    apData <- subset(apData,difftime(mmt,timeStamp,units = 'hours') >= t1 &
                       difftime(mmt,timeStamp,units = 'hours') < t2)
  }else{
    apData <- subset(apData,difftime(mmt,timeStamp,units = 'hours') > t1 &
                       difftime(mmt,timeStamp,units = 'hours') <= t2)
  }
  apData
}

# F2. generate time point 3 hours after special moment
gen_time_point <- function(mmt){
  tmp1 <- paste(format(mmt,format = '%Y'),format(mmt,format = '%m'),
                format(mmt,format = '%d'),format(mmt,format = '%H'),
                1:6,sep = '-')
  tmp2 <- paste(format(mmt,format = '%Y'),format(mmt,format = '%m'),
                format(mmt,format = '%d'),as.numeric(format(mmt,format = '%H')) + 1,
                1:6,sep = '-')
  tmp3 <- paste(format(mmt,format = '%Y'),format(mmt,format = '%m'),
                format(mmt,format = '%d'),as.numeric(format(mmt,format = '%H')) + 2,
                1:6,sep = '-')
  tmp <- c(tmp1,tmp2,tmp3)
}

# F3. read passenge Count 3 hours after special moment
real_pc <- function(apData,mmt,tp){
  afterMMT.apData <- get_data_by_time(apData,mmt,-3,0,T)
  cut.point <- mmt + 60*10*(0:18)
  afterMMT.apData$ts10min <- cut.POSIXt(afterMMT.apData$timeStamp,cut.point)
  tmp <- tapply(afterMMT.apData$passengerCount,afterMMT.apData$ts10min,sum)
  r <- data.frame(passengerCount = tmp,
                  WIFIAPtag = rep(apData$WIFIAPTag[1],length(tp)),
                  slice10min = tp)
  r
}

# F4. predicted passenger Count 3 hours after special moment based on mean pc h hours before special moment.
gen_pc_by_meanhour <- function(apData,mmt,t1,t2,tp){
  beforeMMT.apData <- get_data_by_time(apData,mmt,t1,t2)
  r <- data.frame(passengerCount = rep(mean(beforeMMT.apData$passengerCount)*10,length(tp)),
                  WIFIAPtag = rep(apData$WIFIAPTag[1],length(tp)),
                  slice10min = tp)
  r
}

# F5. evaluate the sum error
eval_error <- function(predPC,realPC){
  mergePC <- merge(predPC,realPC,by = c('WIFIAPtag','slice10min'))
  names(mergePC) <- c('WIFIAPtag','slice10min','predPC','realPC')
  mergePC$error <- (mergePC$predPC - mergePC$realPC)^2
  error.sum <- sum(mergePC$error)
  list(mergePC,error.sum)
}

# F6.get ap data
get_ap_data <- function(apTag){
  tmp <- split.dataAP[[apTag]]
  tmp
}

# F7.plot ap data
plot_ap_data <- function(apTag){
  if(is.character(apTag)){
    data <- get_ap_data(apTag)
  }else if(is.data.frame(apTag)){
    data <- apTag
  }
  tt <- data$WIFIAPTag[1]
  p <- ggplot(data,aes(x = timeStamp,y = passengerCount)) + geom_line() +
    ggtitle(tt)
  print(p)
  p
}

# F8. get flight data
get_flight_data <- function(fID,date){
  curDate <- as.POSIXct(date,tz = 'UTC')
  addDate <- curDate + 86400
  tmp <- subset(data.depWithTime,flight_ID == fID & 
                  actual_flt_time >= curDate & actual_flt_time < addDate)
  tmp
}

# F9. Add 10 minutes timeStamp for 1 minutes interval timestamp
add_ts <- function(ts,ts.start,ts.end,interval = 10){
  div <- seq.POSIXt(ts.start,ts.end,interval*60)
  cut.ts <- cut.POSIXt(ts,div)
}

# F10. plot real pc and predicted pc
plot_comp <- function(df){
  tmp1 <- df[,c('timeStamp','pred')]
  names(tmp1) <- c('timeStamp','passengerCount')
  tmp1$passengerCount <- as.numeric(tmp1$passengerCount)
  tmp1$class <- 'pred'
  
  tmp2 <- df[,c('timeStamp','passengerCount')]
  names(tmp2) <- c('timeStamp','passengerCount')
  tmp2$class <- 'real'
  
  tmp <- rbind(tmp1,tmp2)
  p <- ggplot(tmp,aes(x = timeStamp,y = passengerCount,group = class,color = class)) + 
    geom_line() + geom_point()
  print(p)
  p
}

# F11. define margin for plot function
def_margin <- function(){
  par(mar = rep(1,4))
}

# F12. error compute
error_calc <- function(df){
  (df$passengerCount - df$pred)^2
}

# F13. as.POSIXct convert
as.p <- function(ts){
  if(class(ts) == 'factor'){
    r <-   as.POSIXct(fct2ori(ts),tz = 'UTC')
  }else if(class(ts) == 'character'){
    r <-   as.POSIXct(ts,tz = 'UTC')
  }
  r
}

# F14. get area
get_area <- function(df){
  substr(df$WIFIAPTag,1,2)
}