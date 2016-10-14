#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: ftr_eval.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-14 11:27:55
#
# Last   modified: 2016-10-14 11:27:57

rm(list = ls())
source('head.R')
source('dirFunc.R')
source('arimaFunc.R')
source('areaPCFunc.R')
require(forecast)

load(file.path(dir_data,'dataLoad.Rda'))
load(file.path(dir_data,'areaPC.Rda'))
load(file.path(dir_data,'smpIdx.Rda'))
obj.Para <- split.dataAP10min[idx]
def_margin()
####################################

data.depar.filter <- gen_data.depa(data.departure,data.flights,data.gates)
data.psg <- data.depar.filter

split.area <- split(data.ap10min,data.ap10min$area)
data.area <- gen_pc_area_ts(split.area[[2]])

df <- data.area

train.start <- as.POSIXct('2016-09-10 19:00:00',tz = 'UTC')
train.end <- as.POSIXct('2016-09-14 12:00:00',tz = 'UTC')

test.start <- as.POSIXct('2016-09-14 12:00:00',tz = 'UTC')
test.end <- as.POSIXct('2016-09-14 15:00:00',tz = 'UTC')

# generate train and test
df.train <- subset(df,timeStamp < train.end & timeStamp >= train.start)
df.test <- subset(df,timeStamp < test.end & timeStamp >= test.start)

# generate feature for train and test for all data
list[train_ftr,test_ftr] <- gen_train_test(df,
                                           subset(data.depar.filter,1==1),
                                           train.start,train.end,test.start,test.end)

fit <- lm(pc ~ .,train_ftr)

df.train$pred <- predict(fit,train_ftr)
df.test$pred <- predict(fit,test_ftr)
plot_comp(df.train)
plot_comp(df.test)
sum(error_calc(df.test))

# generate feature for train and test for area data
list[train_ftr.area,test_ftr.area] <- gen_train_test(df,
                                                     subset(data.depar.filter,BGATE_AREA == fct2ori(df$area[1])),
                                                     train.start,train.end,test.start,test.end)

fit.area <- lm(pc ~ .,train_ftr.area)

df.train$pred <- predict(fit.area,train_ftr.area)
df.test$pred <- predict(fit.area,test_ftr.area)
plot_comp(df.train[50:100,])
plot_comp(df.test)
sum(error_calc(df.test))
