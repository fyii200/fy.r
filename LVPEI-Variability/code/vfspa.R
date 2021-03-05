rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/LVPEI-Variability/")
library(visualFields)

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


spv <- function(dt){
  lv2 <- dt
pdf(file='spv.pdf', width=25, height=35)
# for(i in slopedif[which(slopedif$diff > 0.5),1] ) {
for(i in unique(lv2[,1])) {
  layout(matrix(c(1,1,1,1,
                  2,2,3,3,
                  4:27)
                ,nrow=8,ncol=4,byrow=TRUE),
         heights=c(1.5,2.5,1.30,1.3,1.3,1.3,1.3,1.3,1.3,1.3)
         , widths=c(3,3,3,3))
  idx <- which(lv2[,1]==i)
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
dev.off() }



# ## MD Dist of this sample (every single VF included) ##
# pdf(file='MDdist.pdf', width=6, height=6)
# hist(lv$MD, breaks=100, border=F, col='maroon', xlab='MD (dB)', main='Distribution of MD')
# abline(v=quantile(lv$MD, c(0.05,0.5,0.95)), col=c('green','yellow','black'))
# legend("top", c('5%: -29.4','50%: -8.6','90%: -0.7'), col=c('green','yellow','black'), bty='n', lty=1, cex=0.8)
# dev.off()

# 
