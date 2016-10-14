#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: testAFR.R
#
# Description: PC predict by arima.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-11 17:23:00
#
# Last   modified: 2016-10-11 17:23:02

rm(list = ls())
source('head.R')
source('dirFunc.R')
require(forecast)
load(file.path(dir_data,'dataLoad.Rda'))
def_margin()
####################################
# S1. test of auto.arima(). We use split.dataAP to build the arima model and predict pc. 
# Then we scale timeStamp to 10 minutes and calculate the error
test_arima <- function(df){
  cat(sprintf('test_AFR::test_arima\tWIFIAPTag:%s\twid:%.0f\n',
              df$WIFIAPTag[1],df$wid[1]))
  pred.start <- as.POSIXct('2016-09-14 15:00:00',tz = 'UTC')
  pred.end <- as.POSIXct('2016-09-14 18:00:00',tz = 'UTC')
  div <- seq.POSIXt(pred.start,pred.end,10*60)
  div <- div[1:(length(div)-1)]
  
  dfTrain <- subset(df,timeStamp < pred.start)
  dfTest <- subset(df,timeStamp < pred.end & timeStamp >= pred.start)
  
  tsdata <- ts(dfTrain$passengerCount,start = c(2016),frequency = 144)
  fit <- auto.arima(tsdata,D = 1)
  fc <- forecast.Arima(fit,h = length(div))
  
  tsStr <- as.character(div)
  tsStr <- paste(substr(tsStr,1,10),substr(tsStr,12,13),as.numeric(substr(tsStr,15,16))/10,sep='-')
  r <- data.frame(passengerCount = round(as.numeric(fc$mean),digits = 2),
                  WIFIAPTag = as.numeric(df$WIFIAPTag[1]),
                  slice10min = as.character(tsStr))
  r
  # plot(fc)
  # dfTest$pred <- fc$mean
  # dfTest$error <- (dfTest$passengerCount - dfTest$pred)^2
  # sum.error <- sum(dfTest$error)
  # p <- plot_comp(dfTest) + ggtitle(df$WIFIAPTag[1])
  
  # for 10 mins
  # dfTest$ts10min <- add_ts(dfTest$timeStamp,pred.start,pred.end,10)
  # dfTest.10min <- data.frame(WIFIAPTag = dfTest$WIFIAPTag[1],
  #                            timeStamp = levels(dfTest$ts10min),
  #                            passengerCount = as.numeric(tapply(dfTest$passengerCount,
  #                                                               dfTest$ts10min,sum)),
  #                            pred = as.numeric(tapply(dfTest$pred,dfTest$ts10min,sum)))
  # dfTest.10min$error <- (dfTest.10min$passengerCount - dfTest.10min$pred)^2
  # sum.error.10min <- sum(dfTest.10min$error)

}

# S2. do parallel
obj.Para <- split.dataAP10min
require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(obj.Para)),outfile = '')
registerDoParallel(ck)

r <- foreach(i = obj.Para,.verbose = T,.packages = c('ggplot2','forecast')) %dopar% test_arima(i)
r1 <- do.call(rbind,r)
r1$passengerCount[r1$passengerCount < 0] <- 0
save(r,r1,file = file.path(dir_data,'r1012.Rda'))
write.csv(r1,file = file.path(dir_data,'r1012.csv'),quote = F,row.names = F)

stopCluster(ck)

