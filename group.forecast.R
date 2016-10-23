#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: group.forecast.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-15 15:30:19
#
# Last   modified: 2016-10-15 15:30:22

group.forecast <- function(train,test,func,...){
  funcNames <- c('seasonal.naive',
                 'product',
                 'tslm.basic',
                 'stlf.svd',
                 'lm.external',
                 'rf.external',
                 'lm.external.flight')
  
  if(func %in% funcNames){
    f <- get(func)
  }else{
    stop(func,' not legal forecast option')
  }
  
  
  test.dates <- unique(test$timeStamp)
  train.dates <- unique(train$timeStamp)
  test.AP <- unique(test$WIFIAPTag)
  train.AP <- unique(test$WIFIAPTag)
  
  test.frame <- data.frame(timeStamp = rep(test.dates,length(test.AP)),
                           WIFIAPTag = rep(test.AP,each = length(test.dates)),
                           passengerCount = 0)
  test.frame <- dcast(test.frame,WIFIAPTag ~ timeStamp,value.var = 'passengerCount')
  test.frame[is.na(test.frame)] <- 0
  test.ori <- dcast(test,WIFIAPTag ~ timeStamp,value.var = 'passengerCount')
  
  train.frame <- data.frame(timeStamp = rep(train.dates,length(test.AP)),
                            WIFIAPTag = rep(test.AP,each = length(train.dates)))
  train.frame <- join(train.frame,train,by = c('timeStamp','WIFIAPTag'),type = 'left')
  train.frame <- dcast(train.frame,WIFIAPTag ~ timeStamp,value.var = 'passengerCount')
  train.frame[is.na(train.frame)] <- 0
  
  # trainF <- train.frame;testF <- test.frame
  # result <- stlf.svd(train.frame,test.frame, model.type = 'arima', n.comp = 28)
  result <- f(train.frame,test.frame,...)
  result <- melt(result,id.vars = 'WIFIAPTag')
  names(result) <- c('WIFIAPTag','timeStamp','pred')
  
  r <- join(test,result,type = 'inner',by = c('WIFIAPTag','timeStamp'))
  r$error <- error_calc(r)
  cat(sprintf('[group.forecast::group.forecast]\tfunc:%s\terror:%.2f\n',func,sum(r$error)))
  r
}

