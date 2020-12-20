rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/myopia/")
d <- read.csv("data/myopia.csv")
library(RColorBrewer)
library(plotrix)

# d[which(gr$diff>0),][,-13]

# pdf (file = "plots.pdf", width = 12, height = 12)
# plot_res <- function (a,b,c,d,e,f,g,h,i,j) {plot(a,b,xlim=c(100,550),ylim=c(-40,60),bty="n",pch=19,col=h,cex = 0.8, abline(h=0,lwd=0.7,lty=2),
#                                    xlab="Baseline CT (µm)",ylab="Change in CT (µm)",main=c) + abline(a=d,b=e, col="red",lwd=1.5)
#   legend("top", inset=0.1, legend = c(paste0("y = ",round(e,digits=4),"x ","+ ",round(d,digits=4) ), paste0("R² Linear = ", f), paste0("Spearman's r = ",i),paste0("p = ",j) ), bty="n")
#   legend("topleft", legend=g, bty="n", cex=2, text.font=2, text.col="black")}
# 
# exp.bef <- c(d[1:23,3:12]$X1,d[1:23,3:12]$X2,d[1:23,3:12]$X3,d[1:23,3:12]$X4,d[1:23,3:12]$X5,d[1:23,3:12]$X6,d[1:23,3:12]$X7,d[1:23,3:12]$X8,d[1:23,3:12]$X9)
# d2 <- d[24:46,4:12] - d[1:23,4:12]
# exp.chg <- c(d2$X1,d2$X2,d2$X3,d2$X4,d2$X5,d2$X6,d2$X7,d2$X8,d2$X9)
# 
# d3 <- d[47:69,4:12] - d[1:23,4:12]
# exp.chg2 <- c(d3$X1,d3$X2,d3$X3,d3$X4,d3$X5,d3$X6,d3$X7,d3$X8,d3$X9)
# 
# cont.bef <- c(d[70:92,3:12]$X1,d[70:92,3:12]$X2,d[70:92,3:12]$X3,d[70:92,3:12]$X4,d[70:92,3:12]$X5,d[70:92,3:12]$X6,d[70:92,3:12]$X7,d[70:92,3:12]$X8,d[70:92,3:12]$X9)
# d4 <- d[93:115,4:12] - d[70:92,4:12]
# cont.chg <- c(d4$X1,d4$X2,d4$X3,d4$X4,d4$X5,d4$X6,d4$X7,d4$X8,d4$X9)
# 
# d5 <- d[116:138,4:12] - d[70:92,4:12]
# cont.chg2 <- c(d5$X1,d5$X2,d5$X3,d5$X4,d5$X5,d5$X6,d5$X7,d5$X8,d5$X9)
# 
# dat <- rbind(data.frame("eye" = "exp" ,"bef" = exp.bef, "chg" = exp.chg, "chg2" = exp.chg2), data.frame("eye" = "cont","bef" = cont.bef, "chg" = cont.chg, "chg2" = cont.chg2))
# par(mfrow=c(3,2))
# plot_res(dat$bef, dat$chg, "Combined: Immediate Change in CT vs Baseline CT",-5.6487,0.0113,round(summary(lm(chg~bef, data=dat))$r.squared, digits=3),
#          "A", "Blue", 0.092, 0.062)
# plot_res(dat$bef,dat$chg2,"Combined: Change in CT After 5 Minutes vs Baseline CT",-4.1842,0.0108,round(summary(lm(chg2~bef, data=dat))$r.squared, digits=3), 
#          "B", "Blue", 0.025, 0.615)
# plot_res(cont.bef,exp.chg,"Control Eyes: Immediate Change in CT vs Baseline CT",-1.6698,-0.0030,round(summary(lm(chg~bef, data=dat[which(dat$eye=="cont"),] ))$r.squared, digits=3), 
#          "C", "green", -0.061, 0.382)
# plot_res(cont.bef,exp.chg2, "Control Eyes: Change in CT After 5 Minutes vs Baseline CT",-5.4446,0.01547,round(summary(lm(chg2~bef, data=dat[which(dat$eye=="cont"),] ))$r.squared, digits=3), 
#          "D", "green", 0.066, 0.347)
# plot_res(exp.bef,exp.chg,"Experimental Eyes: Immediate Change in CT vs Baseline CT",-9.4795,0.0249,round(summary(lm(chg~bef, data=dat[which(dat$eye=="exp"),] ))$r.squared, digits=3), 
#          "E", "purple", 0.231, 0.001)
# plot_res(exp.bef, exp.chg2, "Experimental Eyes: Change in CT After 5 Minutes vs Baseline CT",-2.9860,0.0064,round(summary(lm(chg2~bef, data=dat[which(dat$eye=="exp"),] ))$r.squared, digits=3), 
#          "F","purple", 0.093, 0.183)
# 
# dev.off()

## Which subjects thickened, which thinned (flashed eyes)? ##
x <- cbind(data.frame(px = d$px[1:23]), data.frame(base=apply(d[1:23,4:12],1,mean)) ,data.frame(ctchg = apply(d[24:46,4:12] - d[1:23, 4:12],1,mean)) )

pdf(file='baseVSchg.pdf', width=7, height=7)
plot(jitter(x$base[c(1:3,6:8,19,23)]), jitter(x$ctchg[c(1:3,6:8,19,23)]), bty='n', pch=16, xlab='Baseline CT (µm)', ylab='Change in CT (µm)', 
     col='red', ylim=c(-25,25), xlim=c(200,500), main='Change in CT vs Baseline CT')
points(jitter(x$base[c(4:5,9:18,20:22)]), jitter(x$ctchg[c(4:5,9:18,20:22)]), col='blue', pch=17)
abline(h=0, lty=2)
legend(200, 15, c('Thickening', 'Thinning'), pch=c(16,17), col=c('red', 'blue'), bty='n', cex=0.9)
dev.off()

## BLAND-ALTMAN PLOT##
ba.sfct <- data.frame(bas = d[1:23,4], chg = (d$X1[24:46] - d$X1[1:23]) )

ba.ct <- data.frame (bas = 1:207, chg = 1:207)
  ba.ct$bas <- c(d[1:23,4],d[1:23,5],d[1:23,6],d[1:23,7],d[1:23,8],d[1:23,9],d[1:23,10],d[1:23,11],d[1:23,12],d[1:23,13],d[1:23,14])
  idx <- d[24:46,4:12] - d[1:23,4:12]
  ba.ct$chg <- c(idx[,1],idx[,2],idx[,3],idx[,4],idx[,5],idx[,6],idx[,7],idx[,8],idx[,9])  
  
ba.fun <- function (a,b) {plot(a$bas, a$chg, bty="n", pch=19,col="orange",
     xlab=paste0("Baseline ", b, " (µm)"), ylab=paste0("Immediate Change in ", b, " (µm)"), cex=0.7,
     ylim=c(-40,40), xlim=c(100,550), main = paste0("Bland-Altman Plot: Change in ", b, " vs Baseline ", b))
ml <- mean(a$chg)
ul <- ml + 1.96*sd(a$chg)
ll <- ml - 1.96*sd(a$chg)
abline (h=ml, lty=5)
abline(h=c(ul, ll), col=c("red", "blue"), lwd=1.4 )
loa <- data.frame(r = round(c(ul,ml,ll), digits=1) )
text (150, c(ul+1.5,ml+1.5,ll+1.5), labels= c(loa[1,], loa[2,], loa[3,] ) )
legend ("topleft", lty=c(1,5,1), col=c("red", "black", "blue"), 
          legend=c("Upper LoA", "Mean difference", "Lower LoA"), bty="n", lwd=2 ) }
pdf (file="plot3.pdf", width=14, height=7)
par(mfrow=c(1,2))                                                           
ba.fun(ba.sfct,"SFCT")
ba.fun(ba.ct, "Overall CT")
dev.off()

####################################################################################################################
ctchg <- data.frame(px=1:23, eye='exp', chg=0)

idx <- cbind(d[1:46,1:3], data.frame(ct=apply(d[1:46,4:12],1,mean)) )
ctchg$chg <- idx[24:46,4] - idx[1:23,4]

d.thick3 <- d[c(1:3,6:8,19,23,24:26,29:31,42,46,70:72,75:77,88,92,93:95,98:100,111,115),]
d.thin3 <- d[c(which(ctchg$chg < 0),27:28,32:41,43:45,73:74,78:87,89:91,96:97,101:110,112:114),]


# pdf(file='thickeningCTgroup', width=16, height=12)
# par(mfrow=c(3,3))
# for(i in 4:12){
#   plot(jitter(d.thick3[which(d.thick3$eye =='exp' & d.thick3$time == 'bef'),i]),
#        jitter(d.thick3[which(d.thick3$eye =='exp' & d.thick3$time == 'fl'),i] - d.thick3[which(d.thick3$eye =='exp' & d.thick3$time == 'bef'),i]),
#        ylab='Change in CT (µm)',xlab='Baseline CT (µm)', main= paste0('Subfield ',i-3), pch=19, col='lawngreen', bty='n', ylim=c(-40,60), xlim=c(150,550), cex=1.3)
#   
#   points(jitter(d.thick3[which(d.thick3$eye =='cont' & d.thick3$time == 'bef'),i]),
#          jitter(d.thick3[which(d.thick3$eye =='cont' & d.thick3$time == 'fl'),i] - d.thick3[which(d.thick3$eye =='cont' & d.thick3$time == 'bef'),i]),
#          col='maroon', pch=19, cex=1.3)
#   
#   legend('topleft',c('Flashed', 'Non-flashed'), col=c('lawngreen', 'maroon'), pch=19, bty='n' )
#   
#   abline(h=0, lty=2)}
# dev.off()



# pdf(file='thinningCTgroup', width=16, height=12)
# par(mfrow=c(3,3))
# for(i in 4:12){
#   plot(jitter(d.thin3[which(d.thin3$eye =='exp' & d.thin3$time == 'bef'),i]),
#        jitter(d.thin3[which(d.thin3$eye =='exp' & d.thin3$time == 'fl'),i] - d.thin3[which(d.thin3$eye =='exp' & d.thin3$time == 'bef'),i]),
#        ylab='Change in CT (µm)',xlab='Baseline CT (µm)', main= paste0('Subfield ',i-3), pch=19,
#        col='lawngreen', bty='n', ylim=c(-40,60), xlim=c(150,550), cex=1.3)
#   
#   points(jitter(d.thin3[which(d.thin3$eye =='cont' & d.thin3$time == 'bef'),i]),
#          jitter(d.thin3[which(d.thin3$eye =='cont' & d.thin3$time == 'fl'),i] - d.thin3[which(d.thin3$eye =='cont' & d.thin3$time == 'bef'),i]),
#          col='maroon', pch=19, cex=1.3)
#   
#   legend('topleft',c('Flashed', 'Non-flashed'), col=c('lawngreen', 'maroon'), pch=19, bty='n' )
#   
#   abline(h=0, lty=2)}
# dev.off()

######################################################################################################################

ctchg <- data.frame(px=1:23, eye='exp', chg=0)

idx <- cbind(d[1:46,1:3], data.frame(ct=apply(d[1:46,4:12],1,mean)) )
ctchg$chg <- idx[24:46,4] - idx[1:23,4]

d.thick3 <- d[c(1:3,6:8,19,23,24:26,29:31,42,46,70:72,75:77,88,92,93:95,98:100,111,115),]
d.thin3 <- d[c(which(ctchg$chg < 0),27:28,32:41,43:45,73:74,78:87,89:91,96:97,101:110,112:114),]

# pdf(file='baselineVSchg', width=14, height=12)
# par(mfrow=c(3,3))
# for(i in 4:12){
#   plot(jitter(d.thin3[which(d.thin3$eye =='exp' & d.thin3$time == 'bef'),i]),
#        jitter(d.thin3[which(d.thin3$eye =='exp' & d.thin3$time == 'fl'),i] - d.thin3[which(d.thin3$eye =='exp' & d.thin3$time == 'bef'),i]),
#        ylab='Change in CT (µm)',xlab='Baseline CT (µm)', main= paste0('Subfield ',i-3), pch=19, col='lawngreen', bty='n', ylim=c(-40,60), xlim=c(150,550), cex=1.1)
#   
#   points(jitter(d.thin3[which(d.thin3$eye =='cont' & d.thin3$time == 'bef'),i]),
#          jitter(d.thin3[which(d.thin3$eye =='cont' & d.thin3$time == 'fl'),i] - d.thin3[which(d.thin3$eye =='cont' & d.thin3$time == 'bef'),i]),
#          col='maroon', pch=19, cex=1.1)
#   
#   points(jitter(d.thick3[which(d.thick3$eye =='exp' & d.thick3$time == 'bef'),i]),
#          jitter(d.thick3[which(d.thick3$eye =='exp' & d.thick3$time == 'fl'),i] - d.thick3[which(d.thick3$eye =='exp' & d.thick3$time == 'bef'),i]),
#          col='purple', pch=17, cex=1.1)
#   
#   points(jitter(d.thick3[which(d.thick3$eye =='cont' & d.thick3$time == 'bef'),i]),
#          jitter(d.thick3[which(d.thick3$eye =='cont' & d.thick3$time == 'fl'),i] - d.thick3[which(d.thick3$eye =='cont' & d.thick3$time == 'bef'),i]),
#          col='orange', pch=17, cex=1.1)
#   
#   legend('topleft',c('Flashed', 'Non-flashed'), col=c('lawngreen', 'maroon'), pch=19, bty='n', title='Thinning', title.adj=0.15, title.col='gray38')
#   legend(134, 43, c('Flashed', 'Non-flashed'), col=c('purple', 'orange'), pch=17, bty='n', title='Thickening', title.adj=0.2, title.col='gray38')
#   
#   abline(h=0, lty=2)}
# dev.off()

########################################################################################################

# #adjusted CT change post-flash
# pdf(file='adjusted', width=14, height=12)
# par(mfrow=c(3,3))
# for(i in 4:12){
#   plot(jitter(d.thick3[which(d.thick3$eye =='exp' & d.thick3$time == 'bef'),i]),
#        jitter( (d.thick3[which(d.thick3$eye =='exp' & d.thick3$time == 'fl'),i] - d.thick3[which(d.thick3$eye =='exp' & d.thick3$time == 'bef'),i]) -
#          (d.thick3[which(d.thick3$eye =='cont' & d.thick3$time == 'fl'),i] - d.thick3[which(d.thick3$eye =='cont' & d.thick3$time == 'bef'),i]) ),
#        ylab='Change in CT (µm)',xlab='Baseline CT (µm)', main= paste0('Subfield ',i-3), pch=17, col='lawngreen', bty='n', ylim=c(-60,60), xlim=c(150,550), cex=1.1)
#   
#   points(jitter(d.thin3[which(d.thin3$eye =='cont' & d.thin3$time == 'bef'),i]),
#          jitter( (d.thin3[which(d.thin3$eye =='exp' & d.thin3$time == 'fl'),i] - d.thin3[which(d.thin3$eye =='exp' & d.thin3$time == 'bef'),i]) -
#            (d.thin3[which(d.thin3$eye =='cont' & d.thin3$time == 'fl'),i] - d.thin3[which(d.thin3$eye =='cont' & d.thin3$time == 'bef'),i]) ),
#          col='maroon', pch=19, cex=1.1)
#   
#   legend('topleft',c('Thickening', 'Thinning'), col=c('lawngreen', 'maroon'), pch=c(17,19), bty='n')
#   abline(h=0, lty=2) }
# 
#   dev.off()
  
  ##############################################################################
d.thick <- rbind(d.thick3[1:16,], d[c(47:49,52:54,65,69),], d.thick3[17:32,], d[c(116:118,121:123,134,138),])
d.thin <- rbind(d.thin3[1:30,], d[c(50,51,55:64,66:68),], d.thin3[31:60,], d[c(119:120,124:133,135:137),])  
  
# ##### chg in CT over time: thickening group #####
# 
# plot_res <- data.frame(time=1:12, s1=0, s2=0, s3=0, s4=0, s5=0, s6=0, s7=0, s8=0, s9=0)
# for(i in 4:12){
# plot_res[1,i-2] <- mean(d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),i] - d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),i])
# plot_res[2,i-2] <- mean(d.thick[which(d.thick$eye=='exp' & d.thick$time=='fl'),i] - d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),i])
# plot_res[3,i-2] <- mean(d.thick[which(d.thick$eye=='exp' & d.thick$time=='fu'),i] - d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),i])
# 
# plot_res[4,i-2] <- mean(d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),i] - d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),i])
# plot_res[5,i-2] <- mean(d.thick[which(d.thick$eye=='cont' & d.thick$time=='fl'),i] - d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),i])
# plot_res[6,i-2] <- mean(d.thick[which(d.thick$eye=='cont' & d.thick$time=='fu'),i] - d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),i])
# 
# plot_res[7,i-2] <- mean(d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),i] - d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),i])
# plot_res[8,i-2] <- mean(d.thin[which(d.thin$eye=='exp' & d.thin$time=='fl'),i] - d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),i])
# plot_res[9,i-2] <- mean(d.thin[which(d.thin$eye=='exp' & d.thin$time=='fu'),i] - d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),i])
# 
# plot_res[10,i-2] <- mean(d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),i] - d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),i])
# plot_res[11,i-2] <- mean(d.thin[which(d.thin$eye=='cont' & d.thin$time=='fl'),i] - d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),i])
# plot_res[12,i-2] <- mean(d.thin[which(d.thin$eye=='cont' & d.thin$time=='fu'),i] - d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),i])
# }
# 
# 
# pdf(file='CTchg', width=14, height=14)
# par(mfrow=c(3,3), mar=c(0,6,2,1))
# 
# ## subfield 1 ##
# plot(1:3, plot_res$s1[1:3], bty='n', xaxt='n', xlab='', ylab='Mean CT Change (µm)', main='', pch=c(1,0,2), ylim=c(-15,15), cex=1.3 )
# abline(h=0, col='gray')
# lines(1:3, plot_res$s1[1:3], col='red', lwd=2)
# 
# points(1:3, plot_res$s1[4:6], pch=c(1,0,2), cex=1.3)
# lines(1:3, plot_res$s1[4:6], lty=2, col='red', lwd=2)
# 
# points(1:3, plot_res$s1[7:9], pch=c(1,0,2), cex=1.3)
# lines(1:3, plot_res$s1[7:9], col='green', lwd=2)
# 
# points(1:3, plot_res$s1[10:12], pch=c(1,0,2), cex=1.3)
# lines(1:3, plot_res$s1[10:12], lty=2, col='green', lwd=2)
# 
# legend('top','Subfield 1', bty='n', cex=1.5)
# 
# legend(1,13, c('Baseline','Immediately post', '5 min post'), pch=c(1,0,2), bty='n')
# legend(1.7,13,c('Flashed','Not flashed'), lty=c(1,2), col='red', title='Thickening', bty='n', lwd=2)
# legend(2.5,13,c('Flashed','Not flashed'), lty=c(1,2), col='green', title='Thinning', bty='n', lwd=2)
# 
# ## subfield 2:3 ##
# for (i in 3:4){
#   plot(1:3, plot_res[1:3,i], bty='n', pch=c(1,0,2), ylim=c(-15,15), cex=1.3, axes=FALSE, ylab='', xlab='' )
#   abline(h=0, col='gray')
#   lines(1:3, plot_res[1:3,i], col='red', lwd=2)
#   
#   points(1:3, plot_res[4:6,i], pch=c(1,0,2), cex=1.3)
#   lines(1:3, plot_res[4:6,i], lty=2, col='red', lwd=2)
#   
#   points(1:3, plot_res[7:9,i], pch=c(1,0,2), cex=1.3)
#   lines(1:3, plot_res[7:9,i], col='green', lwd=2)
#   
#   points(1:3, plot_res[10:12,i], pch=c(1,0,2), cex=1.3)
#   lines(1:3, plot_res[10:12,i], lty=2, col='green', lwd=2)
#   
#   legend('top',paste0('Subfield ',i-1), bty='n', cex=1.5)
#   
#   legend(1,13, c('Baseline','Immediately post', '5 min post'), pch=c(1,0,2), bty='n')
#   legend(1.7,13,c('Flashed','Not flashed'), lty=c(1,2), col='red', title='Thickening', bty='n', lwd=2)
#   legend(2.5,13,c('Flashed','Not flashed'), lty=c(1,2), col='green', title='Thinning', bty='n', lwd=2)
# }
# 
# par(mar=c(0,6,0,1))
# ## Subfield 4 ##  
# plot(1:3, plot_res$s4[1:3], bty='n', xaxt='n', xlab='', ylab='Mean CT Change (µm)', pch=c(1,0,2), ylim=c(-15,15), cex=1.3 )
# abline(h=0, col='gray')
# lines(1:3, plot_res$s4[1:3], col='red', lwd=2)
# 
# points(1:3, plot_res$s4[4:6], pch=c(1,0,2), cex=1.3)
# lines(1:3, plot_res$s4[4:6], lty=2, col='red', lwd=2)
# 
# points(1:3, plot_res$s4[7:9], pch=c(1,0,2), cex=1.3)
# lines(1:3, plot_res$s4[7:9], col='green', lwd=2)
# 
# points(1:3, plot_res$s4[10:12], pch=c(1,0,2), cex=1.3)
# lines(1:3, plot_res$s4[10:12], lty=2, col='green', lwd=2) 
# 
# legend('top',paste0('Subfield ',4), bty='n', cex=1.5)
# 
# legend(1,13, c('Baseline','Immediately post', '5 min post'), pch=c(1,0,2), bty='n')
# legend(1.7,13,c('Flashed','Not flashed'), lty=c(1,2), col='red', title='Thickening', bty='n', lwd=2)
# legend(2.5,13,c('Flashed','Not flashed'), lty=c(1,2), col='green', title='Thinning', bty='n', lwd=2)
#   
# ## Subfield 5:6 ##
# for (i in 6:7){
#   plot(1:3, plot_res[1:3,i], bty='n', pch=c(1,0,2), ylim=c(-15,15), cex=1.3, axes=FALSE, ylab='', xlab='' )
#   abline(h=0, col='gray')
#   lines(1:3, plot_res[1:3,i], col='red', lwd=2)
#   
#   points(1:3, plot_res[4:6,i], pch=c(1,0,2), cex=1.3)
#   lines(1:3, plot_res[4:6,i], lty=2, col='red', lwd=2)
#   
#   points(1:3, plot_res[7:9,i], pch=c(1,0,2), cex=1.3)
#   lines(1:3, plot_res[7:9,i], col='green', lwd=2)
#   
#   points(1:3, plot_res[10:12,i], pch=c(1,0,2), cex=1.3)
#   lines(1:3, plot_res[10:12,i], lty=2, col='green', lwd=2)
#   
#   legend('top',paste0('Subfield ',i-1), bty='n', cex=1.5)
#   
#   legend(1,13, c('Baseline','Immediately post', '5 min post'), pch=c(1,0,2), bty='n')
#   legend(1.7,13,c('Flashed','Not flashed'), lty=c(1,2), col='red', title='Thickening', bty='n', lwd=2)
#   legend(2.5,13,c('Flashed','Not flashed'), lty=c(1,2), col='green', title='Thinning', bty='n', lwd=2)
# }
# 
# par(mar=c(2,6,0,1))  
# ## Subfield 7 ##  
# plot(1:3, plot_res$s7[1:3], bty='n', xaxt='n', xlab='', ylab='Mean CT Change (µm)', pch=c(1,0,2), ylim=c(-15,15), cex=1.3 )
# abline(h=0, col='gray')
# lines(1:3, plot_res$s7[1:3], col='red', lwd=2)
# 
# points(1:3, plot_res$s7[4:6], pch=c(1,0,2), cex=1.3)
# lines(1:3, plot_res$s7[4:6], lty=2, col='red', lwd=2)
# 
# points(1:3, plot_res$s7[7:9], pch=c(1,0,2), cex=1.3)
# lines(1:3, plot_res$s7[7:9], col='green', lwd=2)
# 
# points(1:3, plot_res$s7[10:12], pch=c(1,0,2), cex=1.3)
# lines(1:3, plot_res$s7[10:12], lty=2, col='green', lwd=2) 
# 
# legend('top',paste0('Subfield ',7), bty='n', cex=1.5)
# 
# legend(1,13, c('Baseline','Immediately post', '5 min post'), pch=c(1,0,2), bty='n')
# legend(1.7,13,c('Flashed','Not flashed'), lty=c(1,2), col='red', title='Thickening', bty='n', lwd=2)
# legend(2.5,13,c('Flashed','Not flashed'), lty=c(1,2), col='green', title='Thinning', bty='n', lwd=2)
# 
# ## Subfield 8:9 ##
# for (i in 9:10){
#   plot(1:3, plot_res[1:3,i], bty='n', pch=c(1,0,2), ylim=c(-15,15), cex=1.3, axes=FALSE, ylab='', xlab='' )
#   abline(h=0, col='gray')
#   lines(1:3, plot_res[1:3,i], col='red', lwd=2)
#   
#   points(1:3, plot_res[4:6,i], pch=c(1,0,2), cex=1.3)
#   lines(1:3, plot_res[4:6,i], lty=2, col='red', lwd=2)
#   
#   points(1:3, plot_res[7:9,i], pch=c(1,0,2), cex=1.3)
#   lines(1:3, plot_res[7:9,i], col='green', lwd=2)
#   
#   points(1:3, plot_res[10:12,i], pch=c(1,0,2), cex=1.3)
#   lines(1:3, plot_res[10:12,i], lty=2, col='green', lwd=2)
#   
#   legend('top',paste0('Subfield ',i-1), bty='n', cex=1.5)
#   
#   legend(1,13, c('Baseline','Immediately post', '5 min post'), pch=c(1,0,2), bty='n')
#   legend(1.7,13,c('Flashed','Not flashed'), lty=c(1,2), col='red', title='Thickening', bty='n', lwd=2)
#   legend(2.5,13,c('Flashed','Not flashed'), lty=c(1,2), col='green', title='Thinning', bty='n', lwd=2)
# }
# 
# dev.off()

  
## CT change over time (combined 9 ETDRS) ##

plotthick <- data.frame(chg = c(mean(apply(d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),4:12] - 
                                             d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thick[which(d.thick$eye=='exp' & d.thick$time=='fl'),4:12] - 
                                             d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thick[which(d.thick$eye=='exp' & d.thick$time=='fu'),4:12] - 
                                             d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),4:12] - 
                                             d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thick[which(d.thick$eye=='cont' & d.thick$time=='fl'),4:12] - 
                                             d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thick[which(d.thick$eye=='cont' & d.thick$time=='fu'),4:12] - 
                                             d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),4:12], 1, mean) )
                                )) 


plotthin <- data.frame(chg = c(mean(apply(d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),4:12] - 
                                             d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thin[which(d.thin$eye=='exp' & d.thin$time=='fl'),4:12] - 
                                             d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thin[which(d.thin$eye=='exp' & d.thin$time=='fu'),4:12] - 
                                             d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),4:12] - 
                                             d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thin[which(d.thin$eye=='cont' & d.thin$time=='fl'),4:12] - 
                                             d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),4:12], 1, mean) ),
                                mean(apply(d.thin[which(d.thin$eye=='cont' & d.thin$time=='fu'),4:12] - 
                                             d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),4:12], 1, mean) )
)) 


pdf(file='thickening_chg_time.pdf', width=13, height=7)
par(mfrow=c(1,2), oma=c(0,0,0,0))

plot(c(1:3), plotthick[1:3,], bty='n', pch=c(19,17,15), ylab='Mean CT Change (µm)', xaxt='n', 
     xlab='', ylim=c(-20,35), main='Thickening Subgroup: Change in CT Over Time', type='b', col=c('black','red', 'blue'),cex=1.5 )
arrows(x0=2, y0=11.1, x1=2, y1=0.5, code=3, angle=90, col='gray')
arrows(x0=3, y0=6, x1=3, y1=1, code=3, angle=90, col='gray')
abline(h=0, lwd=0.8, col='gray88')
points(c(1,1.85,2.85), plotthick[4:6,], pch=c(19,17,15), type='b', col=c('black','red', 'blue'), lty=2, cex=1.5 )
arrows(x0=1.85, y0=3.4, x1=1.85, y1=-15, code=3, angle=90, col='gray', lty=2)
arrows(x0=2.85, y0=9.5, x1=2.85, y1=-9.9, code=3, angle=90, col='gray', lty=2)
text(c(2.05,1.92,3.06), c(5.1,-6.35,3.5) ,c('*','*','*'), cex=2)
legend('top', c('Flashed','Not Flashed'), bty='n', lty=c(1,2), cex=0.8)
legend('topleft',c('Baseline','Immediate', '5 Min'), bty='n', pch=c(19,17,15), col=c('black','red', 'blue'),cex=0.8)
#Add individual data points (thickening)
thick.ind <- rbind(data.frame(time=2,chg=c(apply(d.thick[which(d.thick$eye=='exp' & d.thick$time=='fl'),4:12] - d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),4:12], 1, mean),
  apply(d.thick[which(d.thick$eye=='cont' & d.thick$time=='fl'),4:12] - d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),4:12], 1, mean) ) ),
  data.frame(time=3, chg=c(apply(d.thick[which(d.thick$eye=='exp' & d.thick$time=='fu'),4:12] - d.thick[which(d.thick$eye=='exp' & d.thick$time=='bef'),4:12], 1, mean),
  apply(d.thick[which(d.thick$eye=='cont' & d.thick$time=='fu'),4:12] - d.thick[which(d.thick$eye=='cont' & d.thick$time=='bef'),4:12], 1, mean) ) )  )

points(thick.ind$time[1:8], thick.ind$chg[1:8], pch=17, cex=0.5, col='red') #flashed eyes immediately post-flash
points(thick.ind$time[9:16]-0.15, thick.ind$chg[9:16], pch=17, cex=0.5, col='red') #non-flashed eyes immediately post-flash
points(thick.ind$time[17:24], thick.ind$chg[17:24], pch=15, cex=0.5, col='blue') #flashed eyes 5 min post-flash
points(thick.ind$time[25:32]-0.15, thick.ind$chg[25:32], pch=15, cex=0.5, col='blue') #non-flashed eyes 5 min post-flash


plot(c(1:3), plotthin[1:3,], bty='n', pch=c(19,17,15), ylab='Mean CT Change (µm)', xaxt='n', 
     xlab='', ylim=c(-20,35), main='Thinning Subgroup: Change in CT Over Time', type='b', col=c('black','red', 'blue'), cex=1.5 )
arrows(x0=2, y0=-1.5, x1=2, y1=-7.5, code=3, angle=90, col='gray')
arrows(x0=3, y0=-0.6, x1=3, y1=-5.6, code=3, angle=90, col='gray')
abline(h=0, lwd=0.8, col='gray88')
points(c(1,1.85,2.85), plotthin[4:6,], pch=c(19,17,15), type='b', col=c('black','red', 'blue'), lty=2, cex=1.5 )
arrows(x0=1.85, y0=3.2, x1=1.85, y1=-5.2, code=3, angle=90, col='gray', lty=2)
arrows(x0=2.85, y0=4.8, x1=2.85, y1=-5.2, code=3, angle=90, col='gray', lty=2)
text(c(2.05,3.05), c(-5.5,-3.75) ,c('*','*'), cex=2)
legend('top', c('Flashed','Not Flashed'), bty='n', lty=c(1,2), cex=0.8)
legend('topleft',c('Baseline','Immediate', '5 Min'), bty='n', pch=c(19,17,15), col=c('black','red', 'blue'),cex=0.8)

#Add individual points (thinning)
thin.ind <- rbind(data.frame(time=2, chg=c(apply(d.thin[which(d.thin$eye=='exp' & d.thin$time=='fl'),4:12] - d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),4:12], 1, mean), 
                                           apply(d.thin[which(d.thin$eye=='cont' & d.thin$time=='fl'),4:12] - d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),4:12], 1, mean) ) ),
                  data.frame(time=3, chg=c(apply(d.thin[which(d.thin$eye=='exp' & d.thin$time=='fu'),4:12] - d.thin[which(d.thin$eye=='exp' & d.thin$time=='bef'),4:12], 1, mean),
                                           apply(d.thin[which(d.thin$eye=='cont' & d.thin$time=='fu'),4:12] - d.thin[which(d.thin$eye=='cont' & d.thin$time=='bef'),4:12], 1, mean) ) )  )

points(thin.ind$time[1:15], thin.ind$chg[1:15], pch=17, cex=0.5, col='red') # flashed eyes immediately post-flash
points(thin.ind$time[16:30]-0.15, thin.ind$chg[16:30], pch=17, cex=0.5, col='red') #non-flashed eyes immediately post-flash
points(thin.ind$time[31:45], thin.ind$chg[31:45], pch=15, cex=0.5, col='blue') #flashed eyes 5 min post-flash
points(thin.ind$time[46:60]-0.15, thin.ind$chg[46:60], pch=15, cex=0.5, col='blue') #non flashed eyes 5 min post-flash
dev.off()








