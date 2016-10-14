#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: arimaFunc.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-13 10:02:24
#
# Last   modified: 2016-10-13 10:02:28

# F1. Convert 1 minutes pc to 10 minutes pc
conv_10min <- function(df.test){
  df.test$ts10min <- add_ts(df.test$timeStamp,pred.start,pred.end,10)
  df.test.10min <- data.frame(WIFIAPTag = df.test$WIFIAPTag[1],
                              timeStamp = levels(df.test$ts10min),
                              passengerCount = as.numeric(tapply(df.test$passengerCount,df.test$ts10min,sum)),
                              pred = as.numeric(tapply(df.test$pred,df.test$ts10min,sum)))
  df.test.10min
}

# F2.Average move based on predicted value with real value
avg_move <- function(df.test,pred.realstart){
  tmp.known <- subset(df.test,timeStamp < pred.realstart)
  tmp.pred <- subset(df.test,timeStamp >= pred.realstart)
  avg.diff <- mean(tmp.known$passengerCount) - mean(tmp.known$pred)
  tmp.pred$pred <- tmp.pred$pred + avg.diff
  tmp.pred$pred[tmp.pred$pred < 0] <- 0
  tmp.pred
}

# F3.  # generate timeStamp string for result submit
gen_tsStr <- function(div){
  tsStr <- as.character(div)
  tsStr <- paste(substr(tsStr,1,10),substr(tsStr,12,13),as.numeric(substr(tsStr,15,16))/int.min,sep='-')
  tsdf <- data.frame(ts = div,tsstr = tsStr)
}
####################################
# M1. test of auto.arima(). We use split.dataAP/split.dataAP10min to build the arima model and predict pc. 
# Then we scale timeStamp to 10 minutes or not and calculate the error
use10min = T;avgMove = F
test_arima <- function(df,use10min = T,avgMove = F){
  cat(sprintf('test_AFR::test_arima\tWIFIAPTag:%s\twid:%.0f\n',df$WIFIAPTag[1],df$wid[1]))
  train.start <- as.POSIXct('2016-09-10 19:00:00',tz = 'UTC')
  train.end <- as.POSIXct('2016-09-14 12:00:00',tz = 'UTC')
  
  test.start <- as.POSIXct('2016-09-14 12:00:00',tz = 'UTC')
  test.end <- as.POSIXct('2016-09-14 15:00:00',tz = 'UTC')
  
  # timeStamp divide for each forecast
  int.min <- ifelse(use10min,10,1)
  div <- seq.POSIXt(test.start,test.end,int.min*60)
  div <- div[1:(length(div)-1)]
  
  # generate train and test
  df.train <- subset(df,timeStamp < train.end & timeStamp >= train.start)
  df.test <- subset(df,timeStamp < test.end & timeStamp >= test.start)
  
  # generate feature for train and test
  list[train_ftr,test_ftr] <- gen_train_test(df,
                                             subset(data.depar.filter,BGATE_AREA == as.character(df$area[1])),
                                             train.start,train.end,test.start,test.end)
  
  # generate time seris data, build the arima model and forecast new data
  tsdata <- ts(df.train$passengerCount,start = c(2016),frequency = 1440/int.min)
  # train_ftr <- scale(train_ftr[1:(ncol(train_ftr)-1)])
  # test_ftr <- scale(test_ftr[1:(ncol(test_ftr)-1)])
  fit <- auto.arima(tsdata,D = 1,trace = T,
                    # max.P = 0,max.Q = 0,max.p = 1,max.q = 1,
                    # xreg = train_ftr[,1:4],
                    ic = 'aic')
  fc <- forecast.Arima(fit,
                       # xreg = test_ftr[,1:4],
                       h = length(div))
  plot(fc)
  df.test$pred <- fc$mean
  plot_comp(df.test)
  sum(error_calc(df.test))
  
  # average move
  n <- ifelse(avgMove,
             df.test.trunc <- avg_move(df.test,pred.realstart),
             df.test.trunc <- subset(df.test,timeStamp >= pred.realstart))
  
  # for 10 mins
  n <- ifelse(use10min,
              df.error <- df.test.trunc,
              df.error <- conv_10min(df.test.trunc))
  
  # error calculate
  df.error$error <- error_calc(df.error)
  list(df.error,fit)
}