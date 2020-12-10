#variability vs mean sensitivity
#Faban Yii, Paul Artes

rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/LVPEI-Variability/")
# library(visualFields)
# library(eeptools)
library(boot)
# setwd("c:/Users/paul_/Google Drive/People_2/Fabian Yii/fy.r/LVPEI-Variability/")

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

# r <- stack(lv[,19:72])
# r <- r[which(r$values <= 0),]
# r$ind <- as.numeric(r$ind)
# r <- data.frame(values=r$values,ind=r$ind)
# pdf(file='blindspot.pdf',width=6, height=6)
# hist(r$ind, ylim=c(0,7000), breaks=(seq(0,54,1)),col='purple', border='white',
#      main='Distribution of 0 or Lower Sensitivity', xlab='VF Location')
# text(28,7100,labels='L29')
# text(34,6700,labels='L35')
# dev.off()

lv$L29 <- NA
lv$L35 <- NA

# #sensitivity distribution
# lv.h <- stack(lv[,19:72])
# lv.h$values[lv.h$values<0] <- -1
# 
# pdf (file="s.dist.pdf", width=6, height=6)
# hist(lv.h$values, xlab = "Sensitivity (dB)", main = "Sensitivity Distribution",
#      breaks=seq(from=-1, to=50), col='blue', border='white', xlim=c(0,40))
# dev.off()

# plot_res <- data.frame(id=unique(lv$Pat.no), variab=0, mean.s=0)
# 
# for (i in 1:length(plot_res$id)) {
#   idx <- plot_res$id[i]
#   d <- subset(lv, Pat.no==idx, select=c(L1:L54))
# 
#   d[d<=0] <- NA
#   d[,which(as.character(apply(is.na(d), 2, which)) != 'integer(0)')] <- NA
#   
#   plot_res$mean.s[i] <- mean(apply (d, 2, mean), na.rm=TRUE)
#   plot_res$variab[i] <- sd(apply(d,2,sd), na.rm=TRUE)
#   plot_res$variabt[i] <- sd(apply(16.7764*(exp(0.078*d)),2,sd), na.rm=TRUE)
#   # plot_res$variablog[i] <- sd(apply(apply(d,2,log),2,sd), na.rm=TRUE)
#   
#   # plot_res$variab.mad[i] <- mad(apply(d,2,mad), na.rm=TRUE)
#   # plot_res$variabt.mad[i] <- mad(apply(16.7764*(exp(0.078*d)),2,mad), na.rm=TRUE)
#   
#   lv[which(lv$Pat.no==plot_res$id[i]),19:72] <- d
# }
# 
# plot_res <- plot_res [which (plot_res$variab > 0.01 & plot_res$variabt > 0.01),]
# 
# plfunc <- function(a, b, c, d, e) {scatter.smooth (plot_res$mean.s, a, span=0.75, degree=2, main = paste0(b," Variability vs Mean Sensitivity"), 
#       bty="n", pch=19, xlab="Mean Sensitivity (dB)", ylab= paste0(b, " Variability (dB)"),
#       xlim = c(0,35), ylim = c, cex=0.4,  col=d)
#   legend('topleft', paste0("Spearman's rho = ", e), bty='n', cex=0.7 )
#   }

# plot_res <- plot_res[complete.cases(plot_res),]
# 
# co.func <- function(a,b) {
#   dt <- a[b,]
#   cor(dt[,3], dt[,2], method='spearman')}
# 
# myboot <- boot(plot_res,co.func, R=1000)
# plot(myboot, col='purple', border='white')
# pdf(file='95CIPre.pdf', width=6, height=6)
# hist(myboot$t, col='blue', border='white', bty='n', breaks=(seq(-0.50,-0.25,0.01)), main='95% CI: Pre-Transformed')
# abline(v=c(-0.42,-0.34), lty=2)
# dev.off()
# boot.ci(myboot,type='basic')
# 
# co.func <- function(a,b) {
#   dt <- a[b,]
#   cor(dt[,3], dt[,4], method='spearman')}
# myboot <- boot(plot_res,co.func, R=1000)
# plot(myboot, col='purple', border='white')
# pdf(file='95CIPost.pdf', width=6, height=6)
# hist(myboot$t, col='red', border='white', bty='n', breaks=(seq(-0.15,0.1,0.01)), main='95% CI: Transformed')
# abline(v=c(-0.06,0.03), lty=2)
# dev.off()
# boot.ci(myboot,type='basic')

  
# cor.test(plot_res$mean.s, plot_res$variab, alternative='two.sided', method='spearman', conf.level=0.95)
# cor.test(plot_res$mean.s, plot_res$variabt, alternative='two.sided', method='spearman', conf.level=0.95)
# #Variability vs MS (Overall VF = 54 test locations)
# pdf (file="variab.v.ms.pdf", width=12, height=6)
# par(mfrow=c(1,2))
# plfunc(plot_res$variab, "", c(0,6), 'blue', paste0('-0.39: 95% CI [-0.42, -0.34 ]' ) )
# plfunc(plot_res$variabt, "Transformed", c(0,45), 'red', paste0('-0.02: 95% CI [-0.07, 0.03 ]' ))
# dev.off()

# #Robust statistics (MAD rather than SD)
# pdf (file="variab.mad.pdf", width=12, height=6)
# par(mfrow=c(1,2))
# plfunc(plot_res$variab.mad, "MAD -", c(0,6), 'blue')
# plfunc(plot_res$variabt.mad, "MAD - Transformed", c(0,45), 'red')
# dev.off()
# 
# 
# #pointwise variability L1:L54
# lv.pw <- lv[which(lv$No.of.vis>4) ,c(2,19:72)]
# 
# plot_resm <- aggregate(lv.pw, by=list(lv.pw$Pat.no), mean, na.rm=TRUE)
# plot_resv <- aggregate(lv.pw, by=list(lv.pw$Pat.no), sd, na.rm=TRUE)
# plot_resv$Pat.no <- plot_resm$Pat.no
# plot_resm <- plot_resm[,-1]
# plot_resv <- plot_resv[,-1]
# 
# pw <- function(a,b,c,d) {plot (plot_resm[,a], plot_resv[,a], 
#                                    col="blue", bty="n", pch=19, xlab="Mean Sensitivity (dB)", ylab= paste0("Variability (dB)"),
#                                    xlim = c(0,35), ylim = c(0,12), cex=0.3, axes=b, xaxt=c, yaxt=d, font.axis=2 )
#   legend('topleft',paste0('L',a-1),bty='n', cex=1.5)}
# 
# pdf (file="pointwise.pdf", width=24, height=12)
# par(mfrow=c(6,9), mar=c(1.5,1.5,0,0), oma = c(6,6,0.5,0.5), mgp = c(8,0,1))
# 
# pw(2, "TRUE", 'n', 's')
# for(i in 3:10) {
#   pw(i, "FALSE", 's', 'n')}
# pw(11, "TRUE", 'n', 's')
# for(i in 12:19) {
#   pw(i, "FALSE", 's', 'n')}
# pw(20, "TRUE", 'n', 's')
# for(i in 21:28) {
#   pw(i, "FALSE", 's', 'n')}
# pw(29, "TRUE", 'n', 's')
# for(i in 30:37) {
#   pw(i, "FALSE", 's', 'n')}
# pw(38, "TRUE", 'n', 's')
# for(i in 39:46) {
#   pw(i, "FALSE", 'n', 's')}
# pw(47, "TRUE", 's', 's')
# for(i in 48:55) {
#   pw(i, "TRUE", 's', 'n')}
# mtext("MS (dB)", side=1, outer=TRUE, line=2.2, cex=1.5)
# mtext("Variability (dB)", side=2, outer=TRUE, line=2.2, cex=1.5)
# dev.off()
# 
# #variability defined as rms of diff b/w every two test-retest vfs in terms of percentiles
# lvper <- lv[which(lv$No.of.vis>1) ,c(19:72)]
# lvper$ms <- apply(lvper,1,mean,na.rm=TRUE)
# x <- 1
# y <- 2
# repeat {
#   lvper$diff[x] <- sqrt(apply((lvper[x,1:54] - lvper[y,1:54])^2,1,mean,na.rm=TRUE))
#   x = x + 1
#   y = y + 1
#   if (x==13951) {break}
#   if (y==13951) {break}
# }
# lvper <- subset(lvper, select=c(diff, ms) )
# lvper <- data.frame(diff = lvper[-nrow(lvper),])
# names(lvper)[which(names(lvper)=='diff.diff')] <- 'diff'
# names(lvper)[which(names(lvper)=='diff.ms')] <- 'ms'
# 
# lvper$ms <- round(lvper$ms, digits=0)
# lvper <- lvper[order(lvper$ms),]
# lvper <- lvper[-which(lvper$diff=="NaN"),]
# 
# for(i in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)) {
# lvper[which(lvper$ms==i | lvper$ms==i+1),]$ms <- i
# }
# 
# 
# q.05 <- as.data.frame(aggregate(lvper, by=list(lvper$ms),FUN ='quantile', probs=0.05))
# q.5 <- as.data.frame(aggregate(lvper, by=list(lvper$ms),FUN ='quantile', probs=0.5))
# q.95 <- as.data.frame(aggregate(lvper, by=list(lvper$ms),FUN ='quantile', probs=0.95))
# plot_qt <- data.frame(ms=q.05$ms, diff.05=q.05$diff, diff.5=q.5$diff, diff.95=q.95$diff)
# 
# pdf (file="rmspercentiles.pdf", width=6, height=6)
# plot(plot_qt$ms, plot_qt$diff.05, ylim=c(0,30), xlim=c(0,35),pch=19, bty='n', 
#      xlab='MS (dB)', ylab='RMS Variability: percentiles (dB)', main='RMS Variability vs Mean Sensitivity', 
#      col='purple') 
# legend('topright', paste0(c('95%','50%','5%')), pch=c(15,17,19), col=c('red','black','purple'), bty='n')
# points(plot_qt$ms, plot_qt$diff.5, pch=17)
# points (plot_qt$ms, plot_qt$diff.95, pch=15, col='red')
# dev.off()
# 
# 
# #pointwise percentiles
# lvper <- lv[which(lv$No.of.vis>1) ,c(19:72)]
# x <- 1
# y <- 2
# repeat {
#   lvper[x,] <- lvper[x,1:54] - lvper[y,1:54]
#   x = x + 1
#   y = y + 1
#   if (x==13951) {break}
#   if (y==13951) {break}
# }
# lvper <- lvper[-nrow(lvper),]
# a <- as.data.frame(stack(lvper))
# a <- data.frame(loc=a[,2], diff=a[,1])
# b <- lv[which(lv$No.of.vis>1) ,c(19:72)]
# b <- b[-13949,]
# b <- as.data.frame(stack(b))
# a$sens <- b$values
# a$diff <- 0-a$diff
# a <- a[complete.cases(a),]
# a <- a[order(a$sens),]
# a.5 <- as.data.frame(aggregate(a$diff, by=list(a$sens),FUN ='quantile', probs=0.05))
# a.50 <- as.data.frame(aggregate(a$diff, by=list(a$sens),FUN ='quantile', probs=0.5))
# a.95 <- as.data.frame(aggregate(a$diff, by=list(a$sens),FUN ='quantile', probs=0.95))
# plot_a <- data.frame(ms=a.5$Group.1, diff.5=a.5$x, diff.50=a.50$x, diff.95=a.95$x)

# #Transformed
# ta.5 <- as.data.frame(aggregate(16.7764*(exp(0.078*a$diff)), by=list(a$sens),FUN ='quantile', probs=0.05))
# ta.50 <- as.data.frame(aggregate(16.7764*(exp(0.078*a$diff)), by=list(a$sens),FUN ='quantile', probs=0.5))
# ta.95 <- as.data.frame(aggregate(16.7764*(exp(0.078*a$diff)), by=list(a$sens),FUN ='quantile', probs=0.95))
# plot_ta <- data.frame(ms=ta.5$Group.1, diff.5=ta.5$x, diff.50=ta.50$x, diff.95=ta.95$x)

# pdf (file="pointw.percentiles.pdf", width=6, height=6)
# plot(plot_a$ms, plot_a$diff.5, ylim=c(-20,35), xlim=c(0,35),pch=19, bty='n', 
#      xlab='Sensitivity (dB)', ylab='Variability: percentiles (dB)', main='Pointwise Variability vs Sensitivity', 
#      col='purple') 
# legend('topright', paste0(c('95%','50%','5%')), pch=c(15,17,19), col=c('red','black','purple'), bty='n')
# points(plot_a$ms, plot_a$diff.50, pch=17)
# points (plot_a$ms, plot_a$diff.95, pch=15, col='red')
# dev.off()

lv <- lv[-which(lv$No.of.vis<3),]
plot_res <- data.frame(id=unique(lv$Pat.no), variab=0, mean.s=0)
for (i in 1:length(plot_res$id)) {
  idx <- plot_res$id[i]
  d <- subset(lv, Pat.no==idx, select=c(L1:L54))
  
  d[d<=0] <- NA
  d[,which(as.character(apply(is.na(d), 2, which)) != 'integer(0)')] <- NA
  
  plot_res$mean.s[i] <- mean(apply (d, 2, mean), na.rm=TRUE)
  plot_res$variab[i] <- sd(apply(d,2,sd), na.rm=TRUE)
  # plot_res$variabt[i] <- sd(apply(16.7764*(exp(0.078*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt2[i] <- sd(apply(16.7764*(exp(0.1*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt3[i] <- sd(apply(16.7764*(exp(0.2*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt4[i] <- sd(apply(16.7764*(exp(0.3*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt5[i] <- sd(apply(16.7764*(exp(0.4*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt6[i] <- sd(apply(16.7764*(exp(0.5*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt7[i] <- sd(apply(16.7764*(exp(1*d)),2,sd), na.rm=TRUE)
  # 
  # plot_res$variabt8[i] <- sd(apply(16.7764*(exp(-0.078*d)),2,sd), na.rm=TRUE)
  # 
  # plot_res$variabt9[i] <- sd(apply(16.7764*(exp(0.04*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt10[i] <- sd(apply(16.7764*(exp(0.02*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt11[i] <- sd(apply(16.7764*(exp(0.001*d)),2,sd), na.rm=TRUE)
  # 
  # plot_res$variabt12[i] <- sd(apply(20*(exp(0.078*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt13[i] <- sd(apply(30*(exp(0.078*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt14[i] <- sd(apply(40*(exp(0.078*d)),2,sd), na.rm=TRUE)
  # 
  # plot_res$variabt15[i] <- sd(apply(10*(exp(0.078*d)),2,sd), na.rm=TRUE)
  # plot_res$variabt16[i] <- sd(apply(1*(exp(0.078*d)),2,sd), na.rm=TRUE)
  plot_res$variabt17[i] <- sd(apply(2.1*(exp(0.078*d)),2,sd), na.rm=TRUE)

  lv[which(lv$Pat.no==plot_res$id[i]),19:72] <- d
}

plot_res <- plot_res [which (plot_res$variab > 0.01 & plot_res$variabt > 0.01),]

for(i in 1:length(unique(plot_res$id))){
idx2 <- unique(plot_res$id[i])
d2 <- subset(lv, Pat.no==idx2, select=c(L1:L54))

lr <- data.frame(time=1:nrow(d2) ,vf=apply(d2, 1, mean, na.rm=TRUE), vft=apply(1.21*(exp(0.078*d2)), 1, mean, na.rm=TRUE))
plot_res$lin[i] <- sd(summary(lm(vf~time, lr))$residuals, na.rm=TRUE)

plot_res$lint[i] <- sd(summary(lm(vft~time, lr))$residuals, na.rm=TRUE)
}
plot_res <- plot_res[-which(plot_res$lin==0),]


plfunc <- function(a, b, c, d, e) {scatter.smooth (plot_res$mean.s, a, span=0.75, degree=2, main = paste0(b," Variability vs Mean Sensitivity"), 
                                                   bty="n", pch=19, xlab="Mean Sensitivity (dB)", ylab= paste0(b, " Variability (dB)"),
                                                   xlim = c(0,35), cex=0.35, log=c, col=e) 
  legend('topleft',d, bty='n', cex=1.3 ) }

# pdf (file="variab.v.ms.pdf", width=15, height=25)
# par(mfrow=c(6,3))
# plfunc(plot_res$variab, '', '','', 'blue')
# plfunc(plot_res$variab, 'Log Scale:', 'y', '', 'blue')
# plfunc(plot_res$variabt, 'Transformed', '', '16.7764 X e0.078*s', 'red')
# plfunc(plot_res$variabt2, 'Transformed 2', '', '16.7764 X e0.1*s', 'green')
# plfunc(plot_res$variabt3, 'Transformed 3', '', '16.7764 X e0.2*s', 'green')
# plfunc(plot_res$variabt4, 'Transformed 4', '', '16.7764 X e0.0.3*s', 'green')
# plfunc(plot_res$variabt5, 'Transformed 5', '', '16.7764 X e0.4*s', 'green')
# plfunc(plot_res$variabt5, 'Log Scale: Transformed 5', 'y', '16.7764 X e0.4*s', 'green')
# plfunc(plot_res$variabt8,'Transformed 8:', '', '16.7764 X e-0.078*s', 'maroon')
# plfunc(plot_res$variabt9,'Transformed 9:', '', '16.7764 X e0.04*s', 'purple')
# plfunc(plot_res$variabt10,'Transformed 10:', '', '16.7764 X e0.02*s', 'purple')
# plfunc(plot_res$variabt11,'Transformed 11:', '', '16.7764 X e0.001*s', 'purple')
# plfunc(plot_res$variabt12,'Transformed 12:', '', '20 X e0.078*s', 'orange')
# plfunc(plot_res$variabt13,'Transformed 13:', '', '30 X e0.078*s', 'orange')
# plfunc(plot_res$variabt14,'Transformed 14:', '', '40 X e0.078*s', 'orange')
# plfunc(plot_res$variabt15,'Transformed 15:', '', '10 X e0.078*s', 'orange')
# plfunc(plot_res$variabt16,'Transformed 16:', '', '1 X e0.078*s', 'orange')
# plfunc(plot_res$variabt17,'Transformed 17:', '', '0.1 X e0.078*s', 'orange')
# 
# dev.off()

pdf(file='calibratedTransf.pdf', width=12, height=12)
par(mfrow=c(2,2))
plfunc(plot_res$variab, '', '','', 'blue')
plfunc(plot_res$variabt17, 'Transformed', '', '2.1 X e0.078*s', 'red')
abline(h=1, col='green')

plfunc(plot_res$lin, '', '','SD of Residuals', 'blue')
plfunc(plot_res$lint, 'Transformed', '', '1.21 X e0.078*s', 'red')
abline(h=1, col='green')
dev.off()

