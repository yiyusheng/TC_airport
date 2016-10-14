#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: areaPC_assign.R
#
# Description: Assign passengers of each area to APs.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-11 09:38:59
#
# Last   modified: 2016-10-11 09:39:01

rm(list = ls())
source('head.R')
source('dirFunc.R')
load(file.path(dir_data,'dataLoad.Rda'))
load(file.path(dir_data,'areaPC.Rda'))

# F1.Add last x*10 mins pc for each AP
sum_mins <- function(split.dataAP10min,timepoints){
  r <- lapply(split.dataAP10min,function(df){
    df <- df[order(df$timeStamp),]
    tmp <- rep(0,nrow(df))
    for(i in 1:timepoints){
      df.add <- c(rep(0,i),df$passengerCount10min[1:(nrow(df)-i)])
      tmp <- tmp + df.add
    }
    tmp[1:i] <- 0
    df[[paste('last',timepoints,sep='')]] <- tmp
    df
  })
  r
}

# F2. calculate rate on timeStamp and area
rate_calc <- function(df){
  r <- by(df,list(df$timeStamp,df$area),function(x){
    x$last1Rate <- x$last1/sum(x$last1)
    # x$last2Rate <- x$last2/sum(x$last2)
    # x$last3Rate <- x$last3/sum(x$last3)
    # x$last4Rate <- x$last4/sum(x$last4)
    # x$last5Rate <- x$last5/sum(x$last5)
    # x$last6Rate <- x$last6/sum(x$last6)
    x
  })
  r 
}

# F3. work out pc assigned from area.pc.pred by the rate
assign_pc <- function(df,area.pc.pred){
  tmp <- subset(area.pc.pred,as.character(class) == df$area[1])
  df$areaCum <- tmp$passengerCount[match(df$timeStamp,tmp$timeStamp)]
  df$pcAssign1 <- df$areaCum*df$last1Rate
  # df$pcAssign2 <- df$areaCum*df$last2Rate
  # df$pcAssign3 <- df$areaCum*df$last3Rate
  # df$pcAssign4 <- df$areaCum*df$last4Rate
  # df$pcAssign5 <- df$areaCum*df$last5Rate
  # df$pcAssign6 <- df$areaCum*df$last6Rate
  df
}

# F4.evaluate by r^2
diff_square <- function(arr1,arr2){
  tmp <- sum((arr1 - arr2)^2)
  tmp
}
####################################
# S1.Add last x mins pc and rate on area and ts
# data.ap.cumPC <- sum_mins(sum_mins(sum_mins(sum_mins(sum_mins(sum_mins(split.dataAP10min,1),2),3),4),5),6)
data.ap.cumPC <- sum_mins(split.dataAP10min,1)
data.ap.cumPC <- do.call(rbind,data.ap.cumPC)

data.ap.cumPC <- rate_calc(data.ap.cumPC)
data.ap.cumPC <- do.call(rbind,data.ap.cumPC)

data.ap.cumPC <- data.ap.cumPC[order(data.ap.cumPC$WIFIAPTag,data.ap.cumPC$timeStamp),]
data.ap.cumPC[is.na(data.ap.cumPC)] <- 0
row.names(data.ap.cumPC) <- NULL

# S2.assign passengers based on average rate of the last some minutes
split.dataAP.area <- split(data.ap.cumPC,data.ap.cumPC$area)
split.dataAP.area <- lapply(split.dataAP.area,assign_pc,area.pc.pred = area.pc.pred)
data.ap.cumPC <- do.call(rbind,split.dataAP.area)

tmp <- subset(data.ap.cumPC,area %in% c('E1','E2','E3','W1','W2','W3') & 
                timeStamp > as.POSIXct('2016-09-13 15:00:00',tz = 'UTC') & 
                timeStamp <= as.POSIXct('2016-09-13 18:00:00',tz = 'UTC'))

diff_square(tmp$passengerCount10min,tmp$pcAssign1)
diff_square(tmp$passengerCount10min,tmp$pcAssign2)
diff_square(tmp$passengerCount10min,tmp$pcAssign3)
diff_square(tmp$passengerCount10min,tmp$pcAssign4)
diff_square(tmp$passengerCount10min,tmp$pcAssign5)
diff_square(tmp$passengerCount10min,tmp$pcAssign6)




# x <- melt(tapply(data.ap10min$passengerCount10min,list(data.ap10min$timeStamp,data.ap10min$area),sum))
# names(x) <- c('timeStamp','area','pc')
# x$timeStamp <- as.POSIXct(fct2ori(x$timeStamp),tz = 'UTC')
# ggplot(x,aes(x = timeStamp,y = pc,group =area,color = area)) + geom_line()
# 
# numPC_ap <- function(df){
#   
# }