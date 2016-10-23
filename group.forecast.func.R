#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: group.forecast.func.R
#
# Description: 
#
# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-10-18 14:41:39
#
# Last   modified: 2016-10-18 14:41:42

seasonal.naive <- function(trainF,testF){
  coltest <- as.POSIXct(names(testF)[-1],tz = 'UTC')
  
  tmp <- (trainF[,as.character(coltest - 3600*24*1)] + 
            trainF[,as.character(coltest - 3600*24*2)] + 
            trainF[,as.character(coltest - 3600*24*3)])/3
  testF[,names(testF[-1])] <- tmp
  testF
}

product <- function(trainF,testF){
  coltest <- as.POSIXct(names(testF)[-1],tz = 'UTC')
  
  tmp <- (trainF[,as.character(coltest - 3600*24*1)] + 
            trainF[,as.character(coltest - 3600*24*2)] + 
            trainF[,as.character(coltest - 3600*24*3)])/3
  
  profile <- rowMeans(tmp)
  levels <- colMeans(tmp)
  pred <-  profile %*% t(levels)
  pred <- pred/mean(profile)
  
  testF[,names(testF[-1])] <- pred
  testF
}

tslm.basic <- function(trainF, testF){
  n <- ifelse(ncol(trainF) > 1000,itv <- 1,itv <- 10)
  ncol.train <- ncol(trainF)
  ncol.test <- ncol(testF)
  for (i in 1:nrow(trainF)){
    s <- ts(as.numeric(trainF[i,2:ncol.train]),frequency = 1440/itv)
    fit <- tslm(s ~ trend + season)
    fc <- forecast(fit,h = (ncol.test - 1))
    testF[i,2:ncol.test] <- as.numeric(fc$mean)
  }
  testF
}

lm.external <- function(trainF,testF,data.ext){
  n <- ifelse(ncol(trainF) > 1000,itv <- 1,itv <- 10)
  ncol.train <- ncol(trainF)
  ncol.test <- ncol(testF)
  for (i in 1:nrow(trainF)){
    s <- ts(as.numeric(trainF[i,2:ncol.train]),frequency = 1440/itv)
    ftr <- subset(ftr.ext,WIFIAPTag == trainF$WIFIAPTag[i])
    
    its.train <- intersect(names(trainF[i,-1]),as.character(ftr$timeStamp))
    ftr.train <- subset(ftr,timeStamp %in% as.p(factor(its.train)))
    ftr.train <- ftr.train[,grepl('X',names(ftr.train))]
    ftr.train$tar <- ts(as.numeric(trainF[i,its.train]),frequency = 1440/itv)
    
    its.test <- intersect(names(testF[i,-1]),as.character(ftr$timeStamp))
    ftr.test <- subset(ftr,timeStamp %in% as.p(factor(its.test)))
    ftr.test <- ftr.test[,grepl('X',names(ftr.test))]
    
    fit <- glm(tar~.,data = ftr.train,family = 'gaussian')
    fc <- predict(fit,ftr.test)
    testF[i,its.test] <- fc
  }
  testF
}

rf.external <- function(trainF,testF,data.ext){
  n <- ifelse(ncol(trainF) > 1000,itv <- 1,itv <- 10)
  ncol.train <- ncol(trainF)
  ncol.test <- ncol(testF)
  for (i in 1:nrow(trainF)){
    s <- ts(as.numeric(trainF[i,2:ncol.train]),frequency = 1440/itv)
    ftr <- subset(ftr.ext,WIFIAPTag == trainF$WIFIAPTag[i])
    
    its.train <- intersect(names(trainF[i,-1]),as.character(ftr$timeStamp))
    ftr.train <- subset(ftr,timeStamp %in% as.p(factor(its.train)))
    ftr.train <- ftr.train[,grepl('X',names(ftr.train))]
    ftr.train$tar <- ts(as.numeric(trainF[i,its.train]),frequency = 1440/itv)
    
    its.test <- intersect(names(testF[i,-1]),as.character(ftr$timeStamp))
    ftr.test <- subset(ftr,timeStamp %in% as.p(factor(its.test)))
    ftr.test <- ftr.test[,grepl('X',names(ftr.test))]
    
    fit <- randomForest(tar~.,data = ftr.train)
    fc <- predict(fit,ftr.test)
    testF[i,its.test] <- fc
  }
  testF
}

# lm.external <- function(trainF,testF,data.ext){
#   n <- ifelse(ncol(trainF) > 1000,itv <- 1,itv <- 10)
#   ncol.train <- ncol(trainF)
#   ncol.test <- ncol(testF)
#   r <- sapply(1:nrow(trainF[1:2,]),ext_all,trainF,testF)
#   testF[1:2,2:ncol.test] <- t(r)
#   
#   
#   for (i in 1:nrow(trainF)){
#     tr.d <- melt(trainF[i,],id.vars = 'WIFIAPTag')
#     names(tr.d) <- c('WIFIAPTag','timeStamp','passengerCount')
#     tr.d$timeStamp <- as.p(tr.d$timeStamp)
#     tr.ftr <- add_ftr_area_ts(data.ext,tr.d)
#     tr.ftr$target <- as.numeric(trainF[i,2:ncol.train])
#     fit <- lm(target ~ .,data = tr.ftr)
#     
#     te.d <- melt(testF[i,],id.vars = 'WIFIAPTag')
#     names(te.d) <- c('WIFIAPTag','timeStamp','passengerCount')
#     te.d$timeStamp <- as.p(te.d$timeStamp)
#     te.ftr <- add_ftr_area_ts(data.ext,te.d)
#     testF[i,2:ncol.test] <- forecast(fit,te.ftr)$mean
#   }
#   testF
# }

stlf.svd <- function(trainF, testF, model.type = 'ets', n.comp = 28){
  horizon <- ncol(testF)
  trainF[is.na(trainF)] <- 0
  z <- svd(trainF[, 2:ncol(trainF)], nu=n.comp, nv=n.comp)
  s <- diag(z$d[1:n.comp])
  trainF[, 2:ncol(trainF)] <- z$u %*% s %*% t(z$v)
  
  for(i in 1:nrow(trainF)){
    s <- ts(as.numeric(trainF[i,2:ncol(trainF)]), frequency=144)
    # 1742W
    if(model.type == 'ets'){
      fc <- stlf(s, 
                 h=horizon-1, 
                 s.window=144, 
                 method='ets',
                 ic='bic', 
                 opt.crit='mae')
    }else if(model.type == 'arima'){
      fc <- stlf(s, 
                 h=horizon-1, 
                 s.window=144, 
                 method='arima',
                 ic='bic')
    }else{
      stop('Model type must be one of ets or arima.')
    }
    testF[i, 2:horizon] <- as.numeric(fc$mean)
  }
  testF
}

lm.external.flight <- function(trainF,testF,ftr.flight,itv = 10,mod.func = 'lr'){
  
  fc <- foreach(i = 1:nrow(trainF),.packages = c('stats','e1071'),
                .verbose = T,.export = 'lm.external.flight.each') %dopar%
    lm.external.flight.each(i,trainF,testF,ftr.flight,itv,mod.func)

  # fc <- lapply(1:nrow(trainF[1:10,]),function(x){
  #   lm.external.flight.each(x,trainF,testF,ftr.flight,itv,mod.func)
  # })
  
  fc <- do.call(rbind,fc)
  fc[fc < 0] <- 0
  testF[,-1] <- fc
  testF
}

lm.external.flight.each <- function(i,trainF,testF,ftr.flight,itv,mod.func){
  # ncol.train <- ncol(trainF)
  # ncol.test <- ncol(testF)
  # s <- ts(as.numeric(trainF[i,2:ncol.train]),frequency = 1440/itv)
  # ftr <- ftr.flight
  
  its.train <- intersect(names(trainF[i,-1]),as.character(ftr.flight$timeStamp))
  ftr.train <- subset(ftr.flight,timeStamp %in% as.POSIXct(its.train,tz = 'UTC'))
  ftr.train <- ftr.train[,grepl('\\#',names(ftr.train))]
  ftr.train$tar <- as.numeric(trainF[i,its.train])
  
  its.test <- intersect(names(testF[i,-1]),as.character(ftr.flight$timeStamp))
  ftr.test <- subset(ftr.flight,timeStamp %in% as.POSIXct(its.test,tz = 'UTC'))
  ftr.test <- ftr.test[,grepl('\\#',names(ftr.test))]
  
  cat(sprintf('[group.forecast.func::lm.external.flight.each::%s] APnum:%d\n',mod.func,i))
  
  if(mod.func == 'dl'){
    data <- mx.symbol.Variable("data")
    fc1 <- mx.symbol.FullyConnected(data, num_hidden=1)
    lro <- mx.symbol.LinearRegressionOutput(fc1)
    mx.set.seed(0)
    train.data <- data.matrix(ftr.train[,1:(ncol(ftr.train) - 1)])
    train.resp <- as.numeric(trainF[i,its.train])
    fit <- mx.model.FeedForward.create(lro,X = train.data, y= train.resp,array.layout = 'rowmajor',
                                       ctx=mx.cpu(),num.round=100, array.batch.size=20,
                                       learning.rate=2e-6, momentum=0.9, eval.metric=mx.metric.rmse)
    fc <- predict(fit,data.matrix(ftr.test),array.layout = 'rowmajor')
  }else if(mod.func == 'lr'){
    fit <- glm(tar~.,data = ftr.train,family = gaussian())
    fc <- predict(fit,ftr.test)
  }else if(mod.func == 'svm'){
    fit <- svm(tar~.,data = ftr.train)
    fc <- predict(fit,ftr.test)
  }
  
  fc
}