#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: process_in_others_view.R
#
# Description: follow content of others to process my data 
#(https://www.kaggle.com/c/walmart-recruiting-store-sales-forecasting/forums/t/8125/first-place-entry)
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-15 10:29:07
#
# Last   modified: 2016-10-15 10:29:09

rm(list = ls())
source('head.R')
source('dirFunc.R')
source('arimaFunc.R')
source('areaPCFunc.R')
require(forecast)

load(file.path(dir_data,'dataLoad.Rda'))

####################################
# S1.reshape data from long format to wide format
dataAP <- data.ap10min[,1:3]
dataAP.wide <- dcast(dataAP,WIFIAPTag ~ timeStamp)
cname <- names(dataAP.wide)

dataAP.wide$area <- substr(dataAP.wide$WIFIAPTag,1,2)
dataAP.wide$floor <- substr(dataAP.wide$WIFIAPTag,4,4)

dataAP.wide <- dataAP.wide[,c(cname[1],'area','floor',cname[-1])]
