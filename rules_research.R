#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: rules_research.R
#
# Description: Rules research for each data set
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-09 17:03:43
#
# Last   modified: 2016-10-09 17:03:45

rm(list = ls())
source('head.R')
source('dirFunc.R')
load(file.path(dir_data,'dataLoad.Rda'))

####################################
# S1.data.ap research
# for each ap
sta.ap <- lapply(split.dataAP,function(df){
  tmp <- data.frame(WIFIAPTag = df$WIFIAPTag[1],
                    count = nrow(df),
                    sum.pc = sum(df$passengerCount))
  tmp
})
sta.ap <- do.call(rbind,sta.ap)
row.names(sta.ap) <- NULL

sta.ap$floor <- factor(substr(sta.ap$WIFIAPTag,4,4))

sta.ap$area <- factor(substr(sta.ap$WIFIAPTag,1,2))

sta.ap$areaNew <- factor(substr(sta.ap$WIFIAPTag,1,4))

# for each area
data.ap$areaNew <- paste(data.ap$area,data.ap$floor,sep='-')
split.dataArea <- split(data.ap,data.ap$area)
numArea.time <- lapply(split.dataArea,function(df){
  tmp <- tapply(df$passengerCount,df$timeStamp,sum)
  tmp <- data.frame(time = as.POSIXct(names(tmp),tz = 'UTC'),
                    pc = as.numeric(tmp),
                    areaNew = df$areaNew[1],
                    area = df$area[1])
})
numArea.time <- do.call(rbind,numArea.time)

ggplot(subset(numArea.time,grepl('E1',area)),
       aes(x = time,y = pc,group = area,color = area)) + geom_line()

# # S2.data.departure research
# data.departure$wait_time <- as.numeric(difftime(data.departure$flight_time,data.departure$checkin_time,units = 'mins'))
# data.departure <- subset(data.departure,wait_time < 4000)
# ggplot(subset(data.departure,wait_time > 0 & wait_time <= 2000),aes(x = wait_time)) + geom_histogram(bins = 1000)
# 
# # S3.data.securityCheck
# 
# # S4.data.flight
# data.flights$BGATE_AREA <- data.gates$BGATE_AREA[match(data.flights$BGATE_ID,data.gates$BGATE_ID)]
# area.id <- split(data.gates$BGATE_ID,data.gates$BGATE_AREA)
