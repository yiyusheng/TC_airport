#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: areaPC_model_pred.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-13 20:08:33
#
# Last   modified: 2016-10-13 20:08:35

rm(list = ls())
source('head.R')
source('dirFunc.R')
source('areaPCFunc.R')
load(file.path(dir_data,'dataLoad.Rda'))

######
data.depar.filter <- gen_data.depa(data.departure,data.flights,data.gates)
split.passenger_area <- split(data.depar.filter,data.depar.filter$BGATE_AREA)

split.area <- split(data.ap10min,data.ap10min$area)

data.psg <- split.passenger_area[[1]]
data.area <- gen_pc_area_ts(split.area[[1]])

main <- function(){
  list[train_data,test_data,testIdx] <- gen_train_test(data.area,data.depar.filter)
  
  # LR model
  fit.lm <- lm(pc ~ .,data = train_data)
  pred <- predict.lm(fit.lm,test_data)
  
  data.area$pred <- 0
  data.area$pred[testIdx]<- pred
  p <- data.area[testIdx,]
  plot_comp(p)
  sum(error_calc(p))
}

main()
