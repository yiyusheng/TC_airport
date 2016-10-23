#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: gen_external_ftr.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-15 20:37:41
#
# Last   modified: 2016-10-15 20:37:44

rm(list = ls())
source('head.R')
source('util.R')
source('dirFunc.R')
source('external_ftr.R')
require(plyr)

load(file.path(dir_data,'dataLoad.Rda'))
####################################
data.ext <- gen_data.depa(data.departure,data.flights,data.gates)
obj.Para <- split.dataAP10min

require(doParallel)
numCore <- floor(detectCores()*0.8)
ck <- makeCluster(min(numCore,length(obj.Para)),outfile = '')
registerDoParallel(ck)
r <- foreach(df = obj.Para,.combine = rbind,.verbose = F,.packages = 'reshape2') %dopar% 
  add_ftr_ap(data.ext,df)
stopCluster(ck)
save(r,file = file.path(dir_data,'extFeature10Min.Rda'))