#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: runOne.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-15 15:26:47
#
# Last   modified: 2016-10-15 15:26:49

rm(list = ls())
source('head.R')
source('util.R')
source('dirFunc.R')

source('group.forecast.R')
source('group.forecast.func.R')
source('area_feature.R')

require(forecast)
require(plyr)
require(e1071)
# require(mxnet)
# require(randomForest)

load(file.path(dir_data,'dataLoad2nd.Rda'))

# S1. Get flight information without share code flight and remove the wrong flight whose actual_flt_time is less than scheduled_flt_time
# data.flights.raw <- data.flights
# list[data.flights,data.share] <- raw.flight(data.flights.raw,data.gates,levels(data.departure$flight_ID))
# data.flights <- add_delay(data.flights)
# save(data.flights,data.flights.raw,data.share,file = file.path(dir_data,'data.flight.Rda'))
load(file.path(dir_data,'data.flight.Rda'))

# S2. remove items in securecheck whose flight_ID is not a main one. 
# All flight_ID in data.departure are main flight_IDs
data.securecheck <- factorX(subset(data.securecheck,!(flight_ID %in% data.share$share)))

# S3. generate data.
train <- rawTrain()

test <- rawTestA()

itv <- 10


# number of passengerCount for each flight
# data.flights.psg <- avg_psg(data.departure,data.flights)
# names(data.flights.psg)[names(data.flights.psg) == 'meanPsg'] <- 'pc.psg'
# data.flights.sec <- avg_psg(data.securecheck,data.flights)
# names(data.flights.sec)[names(data.flights.sec) == 'meanPsg'] <- 'pc.sec'
# data.flights.pc <- merge(data.flights.sec,data.flights.psg)
# save(data.flights.pc,file = file.path(dir_data,'data.flights.pc.Rda'))
load(file.path(dir_data,'data.flights.pc.Rda'))

# Feature1. number of flights.
# tsSet <- seq.POSIXt(as.p(factor('2016-09-10 19:00:00')),as.p(factor('2016-09-25 18:00:00')),60*itv)
# ftr.flight.sec.schedule <- get_area_feature(tsSet,data.flights.pc,data.gates,attr = 'pc.sec',attrTime = 'scheduled_flt_time')
# ftr.flight.psg.schedule <- get_area_feature(tsSet,data.flights.pc,data.gates,attr = 'pc.psg',attrTime = 'scheduled_flt_time')
# ftr.flight.sec.actual <- get_area_feature(tsSet,data.flights.pc,data.gates,attr = 'pc.sec',attrTime = 'actual_flt_time')
# ftr.flight.psg.actual <- get_area_feature(tsSet,data.flights.pc,data.gates,attr = 'pc.psg',attrTime = 'actual_flt_time')
# save(ftr.flight.sec.schedule,ftr.flight.psg.schedule,
#      ftr.flight.sec.actual,ftr.flight.psg.actual,
#      file = file.path(dir_data,'ftr.flight.Rda'))
load(file.path(dir_data,'ftr.flight.Rda'))


require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(numCore,outfile = '')
registerDoParallel(ck)

system.time(pred.ext.flight.lr.sec.sch <- group.forecast(train,test,'lm.external.flight',
                                                     ftr.flight.sec.schedule,mod.func = 'lr',itv = 10))
system.time(pred.ext.flight.lr.sec.act <- group.forecast(train,test,'lm.external.flight',
                                                         ftr.flight.sec.actual,mod.func = 'lr',itv = 10))
system.time(pred.ext.flight.lr.psg.sch <- group.forecast(train,test,'lm.external.flight',
                                                         ftr.flight.psg.schedule,mod.func = 'lr',itv = 10))
system.time(pred.ext.flight.lr.psg.act <- group.forecast(train,test,'lm.external.flight',
                                                         ftr.flight.psg.actual,mod.func = 'lr',itv = 10))

system.time(pred.ext.flight.svm.sec.sch <- group.forecast(train,test,'lm.external.flight',
                                                          ftr.flight.sec.schedule,mod.func = 'svm',itv = 10))
system.time(pred.ext.flight.svm.sec.act <- group.forecast(train,test,'lm.external.flight',
                                                          ftr.flight.sec.actual,mod.func = 'svm',itv = 10))
system.time(pred.ext.flight.svm.psg.sch <- group.forecast(train,test,'lm.external.flight',
                                                          ftr.flight.psg.schedule,mod.func = 'svm',itv = 10))
system.time(pred.ext.flight.svm.psg.act <- group.forecast(train,test,'lm.external.flight',
                                                          ftr.flight.psg.actual,mod.func = 'svm',itv = 10))


stopCluster(ck)






####################################
# intersect.dep_sec_fli <- intersect(levels(data.departure$flight_ID),intersect(levels(data.securecheck$flight_ID),
#                                    levels(data.flights$flight_ID)))
# 
# data.departure.diff <- factorX(subset(data.departure,!(flight_ID %in% intersect.dep_sec_fli)))
# data.securecheck.diff <- factorX(subset(data.securecheck,!(flight_ID %in% intersect.dep_sec_fli)))
# data.flights.diff <- factorX(subset(data.flights,!(flight_ID %in% intersect.dep_sec_fli)))
# 
# data.departure <- factorX(subset(data.departure,flight_ID %in% intersect.dep_sec_fli))
# data.securecheck <- factorX(subset(data.securecheck,flight_ID %in% intersect.dep_sec_fli))
# data.flights <- factorX(subset(data.flights,flight_ID %in% intersect.dep_sec_fli))

####################################

######
# pred.ext.flight <- group.forecast(train,test,data.ext,'lm.external.flight')
# 
# pred.ext <- group.forecast(train,test,data.ext,'lm.external')
# pred.ext <- group.forecast(train,test,data.ext,'rf.external')
# pred.season <- group.forecast(train,test,data.ext,'seasonal.naive')
# pred.product <- group.forecast(train,test,data.ext,'product')
# pred.tslm.basic <- group.forecast(train,test,data.ext,'tslm.basic')
# pred.stlf.svd.els <- group.forecast(train,test,data.ext,'stlf.svd',model.type = 'ets', n.comp = 28)
# pred.stlf.svd.arima <- group.forecast(train,test,data.ext,'stlf.svd',model.type = 'arima', n.comp = 28)
# 
# last.pred <- cbind(pred.season$pred,pred.product$pred,pred.tslm.basic$pred,
#                    pred.stlf.svd.els$pred,pred.stlf.svd.arima$pred)
# pred <- cbind(pred.season[,c('WIFIAPTag','timeStamp','passengerCount')],rowMeans(last.pred))
# names(pred) <- c('WIFIAPTag','timeStamp','passengerCount','pred')
# 
# sum(error_calc(pred))
# # LM
# pred.collect <- data.frame(season = pred.season$pred,
#                            product = pred.product$pred,
#                            tslm = pred.tslm.basic$pred,
#                            stlf.els = pred.stlf.svd.els$pred,
#                            stlf.arima = pred.stlf.svd.arima$pred,
#                            real = test$passengerCount)
# fit <- lm(real ~ .,data = pred.collect)
# fc <- predict(fit,pred.collect)
# tmp <- data.frame(passengerCount = pred.collect$real,
#                   pred = as.numeric(fc))
# sum(error_calc(tmp))
