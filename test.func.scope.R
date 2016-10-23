plot_ap_data(split.dataAP10min[['E1-1A-3<E1-1-03>']])
dt.tr <- split.dataAP10min[[aptag]]
tmp <- split.dataAP10min[[aptag]]
dt.tr <- subset(tmp,timeStamp < as.p(factor('2016-09-25 12:00:00')))
dt.te <- subset(tmp,timeStamp >= as.p(factor('2016-09-25 12:00:00')))

dt.tr <- merge(dt.tr,ftr.flight,by = 'timeStamp',all.x = T)
dt.te <- merge(dt.te,ftr.flight,by = 'timeStamp',all.x = T)

ftr.flight <- ftr.flight[,order(names(ftr.flight))]
dt.te <- subset(dt.te,1==1,c('timeStamp','passengerCount',names(dt.te)[grepl('E1\\#',names(dt.te))]))
dt.tr.attr <- subset(dt.tr,1==1,c('timeStamp','passengerCount',names(dt.tr)[grepl('E1\\#',names(dt.tr))]))


cor.col <- names(dt.tr)
cor.col <- cor.col[-c(1,2,3)]
cor.pas <- melt(sapply(cor.col,function(x)cor(dt.tr$passengerCount,dt.tr[[x]])))
cor.pas$attr <- row.names(cor.pas)
row.names(cor.pas) <- NULL
cor.pas <- cor.pas[order(cor.pas$value,decreasing = T),]

lag <- 3;len <- nrow(dt.tr.attr)
cor(dt.tr.attr$passengerCount,c(rep(0,lag),dt.tr.attr$`E1#1`[1:(len-lag)]))
