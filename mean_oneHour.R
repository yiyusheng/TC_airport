#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: mean_oneHour.R
#
# Description: Based on the time location, I predicting by the mean number of the passengerCount one hour before targe time point.
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-09 14:58:56
#
# Last   modified: 2016-10-09 14:59:00
rm(list = ls())
source('head.R')
source('dirFunc.R')
load(file.path(dir_data,'dataLoad.Rda'))

####################################
# S1.calculate the mean number before one moment to predict the next hour
mmt <- as.POSIXct('2016-09-13 15:00:00',tz = 'UTC')
split.apData <- split(data.ap,data.ap$WIFIAPTag)
tp <- gen_time_point(mmt)

realPC <- lapply(split.apData,real_pc,mmt = mmt,tp = tp);realPC <- do.call(rbind,realPC)
predPC <- lapply(split.apData,gen_pc_by_meanhour,mmt = mmt,t1 = 0,t2 = 1,tp = tp);predPC <- do.call(rbind,predPC)

list[mergePC,error.sum] <- eval_error(predPC,realPC)
cat(sprintf('mmt:%s\terror.sum:%s\n',mmt,error.sum))


