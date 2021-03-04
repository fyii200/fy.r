rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/LVPEI-Variability/")
# library(visualFields)

d <- read.csv('data/power.csv')
d <- d[,-1]

#configure data frame d
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)



spv <- function(dt){
  lv <- dt
  
  pdf(file='spv.pdf', width=25, height=35)
  for(i in unique(lv[,1])) {
    layout(matrix(c(1,1,1,1,
                    2,2,3,3,
                    4:27)
                  ,nrow=8,ncol=4,byrow=TRUE),
           heights=c(1.5,2.5,1.30,1.3,1.3,1.3,1.3,1.3,1.3,1.3)
           , widths=c(3,3,3,3))
    idx <- lv[which(lv[,1]==i),]
    
    par(mar=c(0,8,4,2))
    plot(0, xlim=c(0,1), ylim=c(0,1), axes=FALSE, ann=FALSE, type='n')
    text(0.252,0.75, labels='Serial Visual Fields Visualisation', cex=7, font=2)
    text(0.21,0.5, labels='Static Automated Perimetry with the SITA Standard Strategy', cex=3.4, font=1)
    text(0.209,0.35, labels='Stimulus Size: Size III           SUNY-IU classic NVs for 24-2', cex=3.4, font=1)
    
    par(mar=c(9,0,0,2))
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    text(0.23,0.75, labels='px', col='lightsteelblue', cex=25.5, font=2)
    text(0.24,0.16, labels='idx', col='lightsteelblue', cex=22, font=2)
    legend(0.92,1, legend=c(paste0('ID: ',idx$id[1]), paste0('Age: ',min(idx$age),' to ',max(idx$age)), paste0('Eye: ',idx$eye[1]),
                            paste0('Date From: ',min(idx$date)), paste0('Date To: ',max(idx$date))), cex=3, bty='n', y.intersp=1.3, adj=1)
    
    md <- apply(gettd(idx)[,-c(1:10,36,45)],1,mean)
    legend(0.91, 0.37, legend=c(paste0('FPR Range: ',min(idx$fpr), ' to ', max(idx$fpr)),
                                paste0('FNR Range: ',min(idx$fnr), ' to ', max(idx$fnr)),
                                paste0('FL Range: ',min(idx$fl), ' to ', max(idx$fl)),
                                paste0('MD Range (dB): ',round(min(md),digits=1), ' to ', round(max(md),digits=1)) ),
          cex=3, bty='n', y.intersp=1.3, adj=1)
    
    par(mar=c(15,9,5,6),  mgp=c(3,2,5))
    plot(idx$date[1:nrow(idx)], md, bty='n', pch=19, col='maroon',
         cex=7, cex.axis=2.5, cex.main=2, xlab='', ylab='')
    legend('left',legend='MD', text.col='gray88', cex=18, bty='n')
    if(nrow(idx)>1 & idx$date[1] != idx$date[2] ){
      lines(lowess(idx$date[1:nrow(idx)], md ), lwd=7, lty=1, col='green')
      abline(a=lm(md~idx$date)$coefficients[1],
             b=lm(md~idx$date)$coefficients[2], col='gray', lwd=7, lty=1)
      legend('top',c('LOESS', 'OLSR'), lty=c(1,1), bty='n', col=c('green', 'gray'),cex=2, lwd=8 )} else{print('NA')}
    
    par(mar=c(5,4,0,2))
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(-0.03,1.1, legend=c(idx$date[1:3]), cex=5, text.font=2, bty='n', y.intersp=1.7,
           text.col='lightsteelblue')
    for (i in 1:3) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=1.2)
      legend(ifelse(idx$eye[1]=='OD',-66,60),29.5,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.06,1.07, legend=c(idx$date[4:6]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
           text.col='lightsteelblue')
    for (i in 4:6) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=1.2)
      legend(ifelse(idx$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.06,1.07, legend=c(idx$date[7:9]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
           text.col='lightsteelblue')
    for (i in 7:9) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=1.2)
      legend(ifelse(idx$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.06,1.07, legend=c(idx$date[10:12]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
           text.col='lightsteelblue')
    for (i in 10:12) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=1.2)
      legend(ifelse(idx$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.06,1.07, legend=c(idx$date[13:15]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
           text.col='lightsteelblue')
    for (i in 13:15) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=1.2)
      legend(ifelse(idx$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.06,1.07, legend=c(idx$date[16:18]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
           text.col='lightsteelblue')
    for (i in 16:18) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=1.2)
      legend(ifelse(idx$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.06,1.07, legend=c(idx$date[19:21]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
           text.col='lightsteelblue')
    for (i in 19:21) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=1.2)
      legend(ifelse(idx$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.06,1.07, legend=c(idx$date[22:24]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
           text.col='lightsteelblue')
    for (i in 22:24) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=1.2)
      legend(ifelse(idx$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
    
    plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
    legend(0.06,1.07, legend=c(idx$date[25:27]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
           text.col='lightsteelblue')
    for (i in 25:27) {if(i > nrow(idx)) {break}
      vfplot(idx[i,], type='s', cex=1.2)
      legend(ifelse(idx$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
  }
  dev.off() }
