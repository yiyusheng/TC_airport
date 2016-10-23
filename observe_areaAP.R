#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: observe_areaAP.R
#
# Description: Observe relationship between pc in a area and pc in an ap belong to the area.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-13 12:29:55
#
# Last   modified: 2016-10-13 12:29:57

rm(list = ls())
source('head.R')
source('dirFunc.R')
source('util.R')

# source('arimaFunc.R')
# require(forecast)
load(file.path(dir_data,'dataLoad2nd.Rda'))

####################################
subts.start <- as.POSIXct('2016-09-10 19:00:00',tz = 'UTC')
subts.end <- as.POSIXct('2016-09-25 15:00:00',tz = 'UTC')

x <- lapply(split.dataAP10min,function(ap10){
  ggsave(plot = plot_ap_data(subset(ap10,as.Date(timeStamp) == as.Date('2016-09-14') | 
                                      as.Date(timeStamp) == as.Date('2016-09-15') | 
                                      as.Date(timeStamp) == as.Date('2016-09-16'))),
         file = file.path(dir_data,'ap_figure',paste(ap10$WIFIAPTag[1],'.png',sep='')),
         width = 12,height = 6,dpi = 100)
})



















get_area_pc <- function(df){
  df$timeStamp <- factor(df$timeStamp)
  df.area <- data.frame(area = df$area[1],
                        timeStamp = levels(df$timeStamp),
                        passengerCount = as.numeric(tapply(df$passengerCount,df$timeStamp,sum)))
}

get_areaNew_pc <- function(df){
  df$timeStamp <- factor(df$timeStamp)
  df.area <- data.frame(areaNew = df$areaNew[1],
                        timeStamp = levels(df$timeStamp),
                        passengerCount = as.numeric(tapply(df$passengerCount,df$timeStamp,sum)))
}


split.area <- split(data.ap,data.ap$area)
x <- lapply(split.area,function(df){
  
  df <- factorX(df)
  df.area <- get_area_pc(df)
  split.ap <- split(df,df$WIFIAPTag)
  split.areaNew <- split(df,df$areaNew)
  
  tmp <- subset(df,WIFIAPTag %in% levels(df$WIFIAPTag)[7:9] & 
                  timeStamp > subts.start & 
                  timeStamp <= subts.end)
  ggplot(tmp,aes(x = timeStamp,y = passengerCount,group = WIFIAPTag,color = WIFIAPTag)) + 
           geom_line()
})