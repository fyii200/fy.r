rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/LVPEI-Variability/")
# library(visualFields)

poag <- read.csv("data/poag.csv")
poag$type <- "poag"
pacg <- read.csv("data/pacg.csv")
pacg$type <- "pacg"
pacg$Pat.no <- pacg$Pat.no + 509
ntg <- read.csv("data/ntg.csv")
ntg$type <- "ntg"
ntg$Pat.no <- ntg$Pat.no + 1305
lv <- rbind(poag,pacg,ntg)
lv$SNO <- seq(1,nrow(lv),by=1)
lv$Vis.date <- as.Date(lv$Vis.date, "%b%d,%Y")
lv$DOB <- as.Date(lv$DOB, "%b%d,%Y")
lv$DOB <- round((lv$Vis.date - lv$DOB)/365, digits=1)
lv2 <- lv[,-c(1,9:10, 14:18,73)]
lv3 <- data.frame(id=lv2[,1], eye=lv2[,2], date=lv2[,6], time=lv2[,3], age=lv2[,5], type=lv2[,7], fpr=lv2[,9], fnr=lv2[,10], fl=lv2[,8], sduration=lv2[,4])
lv2 <- subset(lv, select=c(L1:L54))
lv2 <- cbind(lv3,lv2)
lv2$age <- as.numeric(lv2$age)


# pdf(file='vfplot.pdf',width=12, height=16)
# for(i in 1:max(lv$id)) {
#   idx <- which(lv$id==i)
#   d <- lv[idx,]
#   par(mfrow=c(9,3), omi=c(1,0.5,0.5,1), oma=c(1,1,1.5,1))
#   plot(0,axes=FALSE,ann=FALSE,type='n')
#   legend('center',legend=c(paste0('Patient ID: ',i), paste0('Eye: ', d$eye[1]), paste0('Age: ', d$age[1]) ), cex=1.3,
#          pch=19, col='maroon', bty='n')
#   for (i in 1:nrow(d)) {vfplot(lv[i,], type='s')
#     legend('topleft', legend=paste0('Test: ',i), fill='red', bty='n', cex=1.2) }
# }
# dev.off()


# pdf(file='spv.pdf', width=25, height=40)
# for(i in 1:1) {
#   layout(matrix(c(1,1,1,2,2,3,4:6,7,7,7,8:28),nrow=12,ncol=3,byrow=TRUE), heights=c(0.8,0.8,1.3,0.25), widths=c(4,4,4))
#   idx <- which(lv$id==i)
#   d <- lv[idx,]
# 
#   plot(0,xlim=c(0,1),ylim=c(0,1),axes=FALSE,ann=FALSE,type='n')
#   par(font=2)
#   legend('top',legend='Series Visualisation - Visual Field Progression', bty='n', cex=4)
#   par(font=1)
#   legend(0.5,0.5, legend=c('Static Automated Perimetry with the SITA Standard Strategy', 'Stimulus Size: Size III                    SUNY-IU classic NVs for 24-2'), bty='n', cex=1.5)
#   par(font=1)
#   plot(0, axes=FALSE, ann=FALSE, type='n')
#   legend('top', title=expression(bold('PATIENT INFORMATION')),title.col='maroon' ,
#          legend=c(paste0('ID: ',d$id[1]), paste0('Age: ',d$age[1]), paste0('Eye: ',d$eye[1]), paste0('Diagnosis: '), paste0('From: ',min(d$date)), paste0('To: ',max(d$date))),
#          cex=3, ncol=3, bg='papayawhip', box.lty=0)
#   plot(0, axes=FALSE, ann=FALSE, type='n')
#   legend('topleft', title=expression(bold('     GLOBAL INDICES     ')),title.col='maroon',
#          legend=c(paste0('MS: ',round(mean(apply(d[,11:64],1,mean)),digits=1)), paste0('SD: ',round(mean(apply(d[,11:64],1,sd)), digits=1)),
#                   paste0('Mean FP: ',round(mean(d[,7]), digits=0)),
#                   paste0('Mean FN: ',round(mean(d[,8]), digits=0))), ncol=2,
#          cex=3, bg='papayawhip', box.lty=0)
#   
#   plot(d$date[1:nrow(d)], apply(d[1:nrow(d),11:64],1,mean), bty='n', pch=19, col='blue', main='MS vs Year', 
#        cex=3, cex.lab=2.5, cex.axis=2, cex.main=3, xlab='Year', ylab='MS')
#   lines(lowess(d$date[1:nrow(d)], apply(d[1:nrow(d),11:64],1,mean)))
#   
#   # par(mar=c(1,0,5,2),xpd=TRUE)
#   vfmean <- vfmean(vfselect(d[1:nrow(d),],sel='first',n=nrow(d)),by='eye')
#   vfplot(vfmean, type='tds', cex=1.2)
#   
#   # par(font=2, mar=c(5,11,5,2))
#   hist(stack(d[1:nrow(d),11:64])$values, breaks=seq(min(stack(d[1:nrow(d),11:64])$values), max(stack(d[1:nrow(d),11:64])$values),1),
#        xlab='Sensitivity (dB)', border='white', col='orange', main='Sensitivity Distribution', cex=3, cex.lab=2.5, cex.axis=2, cex.main=3)
#   
#   par(font=1)
#   plot(0, axes=FALSE, ann=FALSE, type='n')
#   
#   for (i in 1:nrow(d)) {vfplot(d[i,], type='s', cex=1.2)
#     legend(ifelse(d$eye[1] =='OS',49,-49), 28, 
#            legend=c(paste0(d$date[i]), paste0('FL: ',d$fl[i]), paste0('FP: ',d$fpr[i]), paste0('FN: ',d$fnr[i]) ), 
#            fill='red', bty='n', cex=1.2) }
# }
# dev.off()



# pdf(file='spv.pdf', width=25, height=40)
# for(i in 1:1) {
#   layout(matrix(c(1,1,1,1,2,2,3,3,4,4,5,5,6,6,6,6,7:28),nrow=10,ncol=4,byrow=TRUE), heights=c(1.7,2,2.2,0.5), widths=c(4,4,4,4))
#   idx <- which(lv2$id==i)
#   d <- lv2[idx,]
#   
#   par(mar=c(0,4,4,2))
#   plot(0, xlim=c(0,1), ylim=c(0,1), axes=FALSE, ann=FALSE, type='n')
#   text(0.29,1, labels='1 Page Report: Series Visualisation', cex=7, font=2)
#   text(0.24,0.97, labels='Static Automated Perimetry with the SITA Standard Strategy', cex=3.5, font=1)
#   text(0.238,0.955, labels='Stimulus Size: Size III           SUNY-IU classic NVs for 24-2', cex=3.5, font=1)
#   
#   par(mar=c(0,0,0,2))
#   # plot(0,xlim=c(0,1), ylim=c(0,1))
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   text(0.24,0.81, labels='px', col='gray88', cex=25, font=2)
#   text(0.25,0.20, labels='idx', col='gray88', cex=22, font=2)
#   legend(0.96,1.03, legend=c(paste0('ID: ',d$id[1]), paste0('Age: ',min(d$age),' to ',max(d$age)), paste0('Eye: ',d$eye[1]),
#                            paste0('Date From: ',min(d$date)), paste0('Date To: ',max(d$date))), cex=3, bty='n', y.intersp=1.3, adj=1)
#   legend(0.95, 0.40, legend=c(paste0('MS: ',round(mean(apply(d[,11:64],1,mean)),digits=1)), 
#                             paste0('MD: ',round(mean(lv[idx,15]), digits=1)), paste0('PSD: ',round(mean(lv[idx,17]), digits=1)), 
#                             paste0('Mean SD: ',round(mean(apply(d[,11:64],1,sd)), digits=1)) ), cex=3, bty='n', y.intersp=1.3, adj=1)
#   
#   
#   par(mar=c(5,0,4,2), font=2)
#   vfmean <- vfmean(vfselect(d[1:nrow(d),],sel='first',n=nrow(d)),by='eye')
#   vfplot(vfmean, type='tds', cex=1.3)
#   # text(-29,20,labels='TD', cex=6, col='gray88')
#   
#   
#   par(mar=c(5,8,5,2))
#   plot(d$date[1:nrow(d)], apply(d[1:nrow(d),11:64],1,mean), bty='n', pch=19, col='blue',
#              cex=2.5, cex.axis=1.5, cex.main=2, xlab='', ylab='')
#   legend('left',legend='MD', text.col='gray88', cex=8, bty='n')
#   lines(lowess(d$date[1:nrow(d)], apply(d[1:nrow(d),11:64],1,mean)))
# 
# 
#   par(mar=c(5,10,5,4))
#   hist(stack(d[1:nrow(d),11:64])$values, breaks=seq(min(stack(d[1:nrow(d),11:64])$values), max(stack(d[1:nrow(d),11:64])$values),1),
#        xlab='', ylab='',main='',border='white', col='orange', cex=3, cex.axis=1.2)
#   legend('left',legend='Dist', text.col='gray88', cex=4.5, bty='n')
# 
#   par(mar=c(2,0,2,0))
#   plot(0, axes=FALSE, ann=FALSE, type='n')
# 
#   for (i in 1:nrow(d)) {vfplot(d[i,], type='s', cex=1.2)
#     par(mar=c(4,0,4,0))
#     legend(ifelse(d$eye[1] =='OS',49,-49), 28,
#            legend=c(paste0(d$date[i])), bty='n', pch=19) }
#   # 
#   # for (i in 1:nrow(d)) {vfplot(d[i,], type='s', cex=1.2)
#   #   legend(ifelse(d$eye[1] =='OS',49,-49), 28,
#   #   legend=c(paste0(d$date[i]), paste0('FL: ',d$fl[i]), paste0('FP: ',d$fpr[i]), paste0('FN: ',d$fnr[i]) ),
#   #        fill='red', bty='n', cex=1.2) }
#   
# }
# dev.off()






# pdf(file='spv.pdf', width=25, height=35)
# for(i in 1:3) {
#   layout(matrix(c(1,1,1,1,
#                   2,2,3,3,
#                   4:27)
#                 ,nrow=8,ncol=4,byrow=TRUE), 
#          heights=c(1.5,2.5,1.3,1.3,1.3,1.3,1.3,1.3,1.3,1.3)
#                    , widths=c(3,3,3,3))
#   idx <- which(lv2$id==i)
#   d <- lv2[idx,]
#   
#   par(mar=c(0,8,4,2))
#   plot(0, xlim=c(0,1), ylim=c(0,1), axes=FALSE, ann=FALSE, type='n')
#   text(0.25,0.75, labels='OnePage VF Series Visualisation', cex=7, font=2)
#   text(0.21,0.5, labels='Static Automated Perimetry with the SITA Standard Strategy', cex=3.4, font=1)
#   text(0.209,0.35, labels='Stimulus Size: Size III           SUNY-IU classic NVs for 24-2', cex=3.4, font=1)
#   
#   par(mar=c(15,0,0,2))
#   # plot(0,xlim=c(0,1), ylim=c(0,1))
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   text(0.23,0.73, labels='px', col='lightsteelblue', cex=25, font=2)
#   text(0.24,0.15, labels='idx', col='lightsteelblue', cex=22, font=2)
#   legend(0.92,1.03, legend=c(paste0('ID: ',d$id[1]), paste0('Age: ',min(d$age),' to ',max(d$age)), paste0('Eye: ',d$eye[1]),
#                              paste0('Date From: ',min(d$date)), paste0('Date To: ',max(d$date))), cex=3, bty='n', y.intersp=1.3, adj=1)
#   legend(0.91, 0.40, legend=c(paste0('MS: ',round(mean(apply(d[,11:64],1,mean)),digits=1)), 
#                               paste0('MD: ',round(mean(lv[idx,15]), digits=1)), paste0('PSD: ',round(mean(lv[idx,17]), digits=1)), 
#                               paste0('Mean SD: ',round(mean(apply(d[,11:64],1,sd)), digits=1)) ), cex=3, bty='n', y.intersp=1.3, adj=1)
#   
#   
#   par(mar=c(15,12,5,6))
#   plot(d$date[1:nrow(d)], apply(d[1:nrow(d),11:64],1,mean), bty='n', pch=19, col='maroon',
#        cex=7, cex.axis=2.5, cex.main=2, xlab='', ylab='')
#   legend('left',legend='MD', text.col='gray88', cex=18, bty='n')
#   lines(lowess(d$date[1:nrow(d)], apply(d[1:nrow(d),11:64],1,mean)), lwd=7)
#   
#   # par(mar=c(2,0,2,0))
#   # plot(0, axes=FALSE, ann=FALSE, type='n')
#   
#   par(mar=c(5,4,0,2))
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   legend(-0.03,1.13, legend=c(d$date[1:3]), cex=5, text.font=2, bty='n', y.intersp=1.7, 
#          text.col='lightsteelblue1')
#   for (i in 1:3) {vfplot(d[i,], type='s', cex=1.2)
#     legend(-37,25,paste0(i), bty='n', text.col='gray88', cex=2, text.font=2)}
#    
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   legend(0.06,1.07, legend=c(d$date[4:6]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
#          text.col='lightsteelblue1')
#   for (i in 4:6) {vfplot(d[i,], type='s', cex=1.2)
#     legend(-37,22,paste0(i), bty='n', text.col='gray88', cex=2, text.font=2)}
#   
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   legend(0.06,1.07, legend=c(d$date[7:9]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
#          text.col='lightsteelblue1')
#   for (i in 7:9) {vfplot(d[i,], type='s', cex=1.2)
#     legend(-37,22,paste0(i), bty='n', text.col='gray88', cex=2, text.font=2)}
#   
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   legend(0.06,1.07, legend=c(d$date[10:12]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
#          text.col='lightsteelblue1')
#   for (i in 10:12) {vfplot(d[i,], type='s', cex=1.2)
#     legend(-37,22,paste0(i), bty='n', text.col='gray88', cex=2, text.font=2) }
#   
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   legend(0.06,1.07, legend=c(d$date[13:15]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
#          text.col='lightsteelblue1')
#   for (i in 13:15) {vfplot(d[i,], type='s', cex=1.2)
#     legend(-37,22,paste0(i), bty='n', text.col='gray88', cex=2, text.font=2) }
#   
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   legend(0.06,1.07, legend=c(d$date[16:18]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
#          text.col='lightsteelblue1')
#   for (i in 16:18) {vfplot(d[i,], type='s', cex=1.2)
#     legend(-37,22,paste0(i), bty='n', text.col='gray88', cex=2, text.font=2) }
#   
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   legend(0.06,1.07, legend=c(d$date[19:21]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
#          text.col='lightsteelblue1')
#   for (i in 19:21) {vfplot(d[i,], type='s', cex=1.2)
#     legend(-37,22,paste0(i), bty='n', text.col='gray88', cex=2, text.font=2) }
#   
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   legend(0.06,1.07, legend=c(d$date[22:24]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
#          text.col='lightsteelblue1')
#   for (i in 22:24) {vfplot(d[i,], type='s', cex=1.2)
#     legend(-37,22,paste0(i), bty='n', text.col='gray88', cex=2, text.font=2) }
#   
#   plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
#   legend(0.06,1.07, legend=c(d$date[25:27]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
#          text.col='lightsteelblue1')
#   for (i in 25:27) {vfplot(d[i,], type='s', cex=1.2)
#     legend(-37,22,paste0(i), bty='n', text.col='gray88', cex=2, text.font=2) }
# }
# dev.off()


pdf(file='spv.pdf', width=25, height=35)
# for(i in slopedif[which(slopedif$diff > 0.5),1] ) {
for(i in unique(lv2$id)) {
  layout(matrix(c(1,1,1,1,
                  2,2,3,3,
                  4:27)
                ,nrow=8,ncol=4,byrow=TRUE), 
         heights=c(1.5,2.5,1.30,1.3,1.3,1.3,1.3,1.3,1.3,1.3)
         , widths=c(3,3,3,3))
  idx <- which(lv2$id==i)
  d <- lv2[idx,]
  
  par(mar=c(0,8,4,2))
  plot(0, xlim=c(0,1), ylim=c(0,1), axes=FALSE, ann=FALSE, type='n')
  text(0.252,0.75, labels='Serial Visual Fields Visualisation', cex=7, font=2)
  text(0.21,0.5, labels='Static Automated Perimetry with the SITA Standard Strategy', cex=3.4, font=1)
  text(0.209,0.35, labels='Stimulus Size: Size III           SUNY-IU classic NVs for 24-2', cex=3.4, font=1)
  
  par(mar=c(9,0,0,2))
  # plot(0,xlim=c(0,1), ylim=c(0,1))
  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  text(0.23,0.75, labels='px', col='lightsteelblue', cex=25.5, font=2)
  text(0.24,0.16, labels='idx', col='lightsteelblue', cex=22, font=2)
  legend(0.92,1, legend=c(paste0('ID: ',d$id[1]), paste0('Age: ',min(d$age),' to ',max(d$age)), paste0('Eye: ',d$eye[1]),
                             paste0('Date From: ',min(d$date)), paste0('Date To: ',max(d$date))), cex=3, bty='n', y.intersp=1.3, adj=1)
  legend(0.91, 0.39, legend=c(paste0('MS: ',round(mean(apply(d[,11:64],1,mean)),digits=1)), 
                              paste0('MD: ',round(mean(lv[idx,15]), digits=1)), paste0('PSD: ',round(mean(lv[idx,17]), digits=1)), 
                              paste0('Mean SD: ',round(mean(apply(d[,11:64],1,sd)), digits=1)) ), cex=3, bty='n', y.intersp=1.3, adj=1)
  
  
  par(mar=c(15,9,5,6),  mgp=c(3,2,5))
  plot(d$date[1:nrow(d)], lv[which(lv$Pat.no==i),15], bty='n', pch=19, col='maroon',
       cex=7, cex.axis=2.5, cex.main=2, xlab='', ylab='')
  legend('left',legend='MD', text.col='gray88', cex=18, bty='n')
  if(nrow(d)>1 & d$date[1] != d$date[2] ){
  lines(lowess(d$date[1:nrow(d)], lv[which(lv$Pat.no==i),15] ), lwd=7, lty=1, col='green')
  abline(a=lm(lv[which(lv$Pat.no==i),15]~d$date)$coefficients[1], 
         b=lm(lv[which(lv$Pat.no==i),15]~d$date)$coefficients[2], col='gray', lwd=7, lty=1)
  legend('top',c('LOESS', 'OLSR'), lty=c(1,1), bty='n', col=c('green', 'gray'),cex=2, lwd=8 )} else{print('NA')}
  
  # par(mar=c(2,0,2,0))
  # plot(0, axes=FALSE, ann=FALSE, type='n')
  
  par(mar=c(5,4,0,2))
  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  legend(-0.03,1.1, legend=c(d$date[1:3]), cex=5, text.font=2, bty='n', y.intersp=1.7, 
         text.col='lightsteelblue')
  for (i in 1:3) {if(i > nrow(d)) {break} 
    vfplot(d[i,], type='s', cex=1.2)
    legend(ifelse(d$eye[1]=='OD',-66,60),29.5,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
  
  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  legend(0.06,1.07, legend=c(d$date[4:6]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
         text.col='lightsteelblue')
  for (i in 4:6) {if(i > nrow(d)) {break} 
    vfplot(d[i,], type='s', cex=1.2)
    legend(ifelse(d$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
  
  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  legend(0.06,1.07, legend=c(d$date[7:9]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
         text.col='lightsteelblue')
  for (i in 7:9) {if(i > nrow(d)) {break} 
    vfplot(d[i,], type='s', cex=1.2)
    legend(ifelse(d$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }

  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  legend(0.06,1.07, legend=c(d$date[10:12]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
         text.col='lightsteelblue')
  for (i in 10:12) {if(i > nrow(d)) {break} 
    vfplot(d[i,], type='s', cex=1.2)
    legend(ifelse(d$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }

  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  legend(0.06,1.07, legend=c(d$date[13:15]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
         text.col='lightsteelblue')
  for (i in 13:15) {if(i > nrow(d)) {break} 
    vfplot(d[i,], type='s', cex=1.2)
    legend(ifelse(d$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }

  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  legend(0.06,1.07, legend=c(d$date[16:18]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
         text.col='lightsteelblue')
  for (i in 16:18) {if(i > nrow(d)) {break} 
    vfplot(d[i,], type='s', cex=1.2)
    legend(ifelse(d$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }

  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  legend(0.06,1.07, legend=c(d$date[19:21]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
         text.col='lightsteelblue')
  for (i in 19:21) {if(i > nrow(d)) {break} 
    vfplot(d[i,], type='s', cex=1.2)
    legend(ifelse(d$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }

  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  legend(0.06,1.07, legend=c(d$date[22:24]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
         text.col='lightsteelblue')
  for (i in 22:24) {if(i > nrow(d)) {break} 
    vfplot(d[i,], type='s', cex=1.2)
    legend(ifelse(d$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }

  plot(0, xlim=c(0,1), ylim=c(0,1), ann=FALSE, axes=FALSE, type='n')
  legend(0.06,1.07, legend=c(d$date[25:27]), cex=2.8, text.font=2, bty='n', y.intersp=1.7,
         text.col='lightsteelblue')
  for (i in 25:27) {if(i > nrow(d)) {break} 
    vfplot(d[i,], type='s', cex=1.2)
    legend(ifelse(d$eye[1]=='OD',-64.5,60),28,c('V','F',paste0(i)), bty='n', text.col='gray88', cex=4.3, text.font=2, y.intersp=1.05) }
}
dev.off()

## MD Dist of this sample (every single VF included) ##
pdf(file='MDdist.pdf', width=6, height=6)
hist(lv$MD, breaks=100, border=F, col='maroon', xlab='MD (dB)', main='Distribution of MD')
abline(v=quantile(lv$MD, c(0.05,0.5,0.95)), col=c('green','yellow','black'))
legend("top", c('5%: -29.4','50%: -8.6','90%: -0.7'), col=c('green','yellow','black'), bty='n', lty=1, cex=0.8)
dev.off()


