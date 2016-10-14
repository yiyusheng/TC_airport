#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: testAFR.R
#
# Description: test autocorrelation of number of people for each AP.
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
source('arimaFunc.R')
source('areaPCFunc.R') 
require(forecast)

load(file.path(dir_data,'dataLoad.Rda'))
load(file.path(dir_data,'areaPC.Rda'))
load(file.path(dir_data,'smpIdx.Rda'))
obj.Para <- split.dataAP10min[idx]
def_margin()

data.depar.filter <- gen_data.depa(data.departure,data.flights,data.gates)
# split.passenger_area <- split(data.depar.filter,data.depar.filter$BGATE_AREA)

split.area <- split(data.ap10min,data.ap10min$area)

data.psg <- data.depar.filter
data.area <- gen_pc_area_ts(split.area[[1]])

# S1. do parallel
require(doParallel)
numCore <- floor(detectCores()*0.9)
ck <- makeCluster(min(numCore,length(obj.Para)),outfile = '')
registerDoParallel(ck)
r <- foreach(i = obj.Para,.verbose = T,.packages = c('ggplot2','forecast')) %dopar% test_arima(i,T,T)
stopCluster(ck)

r1 <- do.call(rbind,r)
cat(sprintf('The sum.error is %.2f for %d APs\n',sum(r1$error),length(r)))
# save(r,file = file.path(dir_data,'rtest.Rda'))