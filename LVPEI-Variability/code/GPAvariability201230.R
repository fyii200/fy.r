## To show that poor specificity of Guided Progression Analysis in some individuals can be explained by high variability ###
rm(list=ls())
setwd('/Users/fabianyii/Desktop/fy.r/LVPEI-Variability')

install.packages('visualFields')
library("visualFields")

data('vfArtes2014')
d <- vfArtes2014

# #compute total deviation for each test location
d[,17:70] <- tdval(d)[17:70]

#configure data frame d
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')

#remove blind spots (L26 and L35)
d <- d[,-c(36,45)]


# plot SD of MD (variability) against MD (MD simply taken as mean of TDs) PRE TRANSFORMED
plot_res <- data.frame(id=unique(d$id), var=0, md=0)
for (i in unique(d$id) ) {
  dat <- d[which(d$id == i),]
  dat$md[1:nrow(dat)] <- apply(dat[,11:62], 1, mean)
  plot_res[which(plot_res$id == i),]$md <- mean(dat$md) #mean of MDs in each series (specific to each px)
  plot_res[which(plot_res$id == i),]$var <- sd(dat$md) }

#plot SD of MD vs MD (PRE TRANSFORMED)
pdf(file='GPAvarMD', height=6, width=12)
par(mfrow=c(1,2))
plot_res$id <- c(1,2,4,5,6,11,13,17,18,23,26,27,103,107,108,109,110,112,114,115,116,119,120,121,122,124,125,128,129,130)
plot(plot_res$md, plot_res$var, pch=15, bty='n', ylab='SD of MD (dB)', xlab='MD (dB)', main='Variability vs MD (Pre-transformed)',
     ylim=c(0.2,1), xlim=c(-12,2), cex=2, col='gray', lty=3 )
text(plot_res$md, plot_res$var, labels=plot_res$id, col='maroon', cex=0.8, pos=2)

for (i in c(125,13,103,129) ){points(plot_res[which(plot_res$id==i),]$md, plot_res[which(plot_res$id==i),]$var, cex=2.5,col='yellow', pch=19) }
for (i in c(6,114,108) ){points(plot_res[which(plot_res$id==i),]$md, plot_res[which(plot_res$id==i),]$var, cex=2.5,col='maroon', pch=19) }
for (i in c(6,122,23,26,2,124) ){points(plot_res[which(plot_res$id==i),]$md, plot_res[which(plot_res$id==i),]$var, cex=2.5,col='blue', pch=19) }
legend('topright', c('likely + possible','likely', 'possible') ,title='Above-average FPR of GPA', pch=19, col=c('yellow', 'maroon', 'blue'), bty='n', cex=0.7, pt.cex=1.2)


#transformed data to equalize variance
for(i in 11:62){ d[,i] <- 40.5*(exp(0.078*d[,i] )) }

# plot SD of MD (variability) against MD (MD simply taken as mean of TDs) TRANSFORMED
plot_res.t <- data.frame(id=unique(d$id), var=0, md=plot_res$md)
for (i in unique(d$id) ) {
  dat <- d[which(d$id == i),]
  dat$md[1:nrow(dat)] <- apply(dat[,11:62], 1, mean)
  plot_res.t[which(plot_res.t$id == i),]$var <- sd(dat$md) }

#plot SD of MD vs MD (TRANSFORMED)
plot_res.t$id <- c(1,2,4,5,6,11,13,17,18,23,26,27,103,107,108,109,110,112,114,115,116,119,120,121,122,124,125,128,129,130)
plot(plot_res.t$md, plot_res.t$var, pch=15, bty='n', ylab='SD of MD (dB)', xlab='MD (dB)', main='Variability vs MD (Transformed)',
     cex=2, col='gray', lty=3, ylim=c(0,2.5) )
abline(h=1, lty=3)
text(plot_res.t$md, plot_res.t$var, labels=plot_res.t$id, col='maroon', cex=0.8, pos=2)

for (i in c(125,13,103,129) ){points(plot_res.t[which(plot_res.t$id==i),]$md, plot_res.t[which(plot_res.t$id==i),]$var, cex=2.5,col='yellow', pch=19) }
for (i in c(6,114,108) ){points(plot_res.t[which(plot_res.t$id==i),]$md, plot_res.t[which(plot_res.t$id==i),]$var, cex=2.5,col='maroon', pch=19) }
for (i in c(6,122,23,26,2,124) ){points(plot_res.t[which(plot_res.t$id==i),]$md, plot_res.t[which(plot_res.t$id==i),]$var, cex=2.5,col='blue', pch=19) }
legend('topright', c('likely + possible','likely', 'possible') ,title='Above-average FPR of GPA', pch=19, col=c('yellow', 'maroon', 'blue'), bty='n', cex=0.7, pt.cex=1.2)

dev.off()


# plot false positive rate RANK vs transformed variability #
plot_res.t$fpr <- 0
# data.frame(fpr=1:30; id=)
plot_res.t$fpr[1:30] <- c(8,14,15,18,2,18,3,18,18,13,9,18,4,18,7,18,18,12,6,18,11,18,18,18,10,17,1,16,5,18)

#bootsrap to find 95% CI for correlation coefficient
bootci <- data.frame(no=1:1000, r=0) #create data frame to record results of 1000 bootstrapping
set.seed(242725)
for (i in 1:1000) {
bs <- sample(1:nrow(plot_res.t), nrow(plot_res.t), replace=TRUE) #random resampling with replacement
bootci[i,2] <- as.numeric(cor.test( plot_res.t[bs,2], plot_res.t[bs,4], alternative='two.sided', 'spearman', exact=FALSE )$estimate)
}

#plot ranked FNR vs transformed variability
pdf(file='RankedFPRvsVar.pdf', width=12, height=6)

par(mfrow=c(1,2))
plot(plot_res.t$var,plot_res.t$fpr, bty='n', pch=19, ylab='FPR (rank)', main='Ranked FPR vs Transformed Variability', 
     xlab='Transformed Variability', col='green', xlim=c(0.5,2), ylim=c(0,20), cex=0.8)
abline(h=c(17.6,5.3), lty=2, col=c('red','blue') )
text(1,19,labels='Lowest FPR (rank 18)', col='gray')
text(1.15,4.5, labels='Top 5 Highest FPR (Rank 1-5)', col='gray')
abline(lm(fpr~var, plot_res.t), col='gray')
legend('center', "Spearman's r = -0.37 (p=0.04)", bty='n', cex=0.7 )


#formula: sample mean ± 1.96*SD / sqrt(sample size)
mean(bootci$r) + 1.96*sd(bootci$r)
mean(bootci$r) - 1.96*sd(bootci$r)
#Distribution of spearman's r from 1000 resampled data
hist(bootci$r, breaks=50, border=F, xlab="Spearman's r", main="Distribution of Spearman's r (1000 Resamples) ")
abline(v= c( mean(bootci$r) + 1.96*sd(bootci$r), mean(bootci$r) - 1.96*sd(bootci$r)), lty=2, col='maroon')
legend('topright', '-0.03 to -0.73', title='95% CI' ,bty='n', cex=1.2, text.col='gray')

dev.off()

# #install latest version of visualFields
# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")  # see RTools WS
# 
# install.packages("devtools")
# library(devtools)
# install_github("imarinfr/vf1/source")
# library("visualFields")
# 
# # creating a new dataframe 'd.s' containing threshold sensitivity values rather than TD
# data('vfArtes2014')
# d.s <- vfArtes2014
# 
# #configure data frame d.s
# d.s <- d.s[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
# names(d.s)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
# 
# #exclude id 4013 from dataset (test intervals too large); entry error?
# d.s <- d.s[-which(d.s$id==4013),]
# # plot_res.t <- plot_res.t[-which(plot_res.t$id==13),]
# 
# # compute days of VF test from test dates
# for(i in 1:length(unique(d.s$id))) {
#   a <- d.s[which(d.s$id == unique(d.s$id)[i]),]
#   for(i in 1:12) { d.s$day[which(d.s$id==a$id)][i] <- as.numeric(a$date[i]-a$date[1]) }
# }
# 
# # scale days to years (8 years max)
# d.s$day <- (d.s$day/105)*8
# 
# 
# #generate VF serial progression analysis (SPA) plots for each px
# for(i in unique(d.s$id)) {
# vfspa(d.s[which(d.s$id==i),1:64], file= paste0('spa',i,'.pdf') ) }
# 
# 
# 
# 
# ### SIMULATIONS ###
# x=1
# 
# # y = mx + c : sensitivity = m (day) + y-intercept (assuming L10 progresses by -0.25dB/y)
# for(i in 1:length(unique(d.s$id))) {
#   a <- d.s[which(d.s$id == unique(d.s$id)[i]),]
#   c <- as.numeric(lm(a$L10[1:12] ~ a$day[1:12])$coefficients[1])
#   m <- -x
#   y <- m*(d.s[1:12,]$day) + c #sensitivity value (y) at L10  if progressed by -0.5dB/y
#   d.s[which(d.s$id==a$id),]$L10[1:12] <- y #change sensitivity to y (progressed sens.)
# }
# 
# # y = mx + c : sensitivity = m (day) + y-intercept (assuming L11 progresses by -0.25dB/y)
# for(i in 1:length(unique(d.s$id))) {
#   a <- d.s[which(d.s$id == unique(d.s$id)[i]),]
#   c <- as.numeric(lm(a$L11[1:12] ~ a$day[1:12])$coefficients[1])
#   m <- -x
#   y <- m*(d.s[1:12,]$day) + c #sensitivity value (y) at L10 if progressed by -0.5dB/y
#   d.s[which(d.s$id==a$id),]$L11[1:12] <- y #change sensitivity at L10 to y (progressed sens.)
# }
# 
# #generate poplr p-value for each px's series
# prog <- data.frame(id=unique(d.s$id), pval=0)
# for (i in 1:length(unique(d.s$id))) {prog$pval[i] <- poplr(d.s[which(d.s$id==unique(d.s$id)[i]),1:64])$cslp}
# prog$pval <- prog$pval/100
# 
# #P-value vs Transformed Variability
# pdf(file=paste0('-', x, '.pdf'), width=6, height=6)
# plot(plot_res.t$var, prog$pval, pch=19, bty='n', xlab='Transformed SD of MD (dB)', ylab='P-value',
#      main=paste0('Variability vs P-value (-', x, 'dB/y at L10 & L11)'), xlim=c(0.5,2))
# abline(h=0.05, v=1, col=c('maroon','blue') )
# # text(plot_res.t[c(14,19),2] ,prog[c(14,19),2], labels=c(paste0('px 108: p=',prog$pval[14]),paste0('px 115: p=',prog$pval[19]) ), col='maroon',pos=2)
# dev.off()
# 
# ### SIMULATIONS ###
# 
# 
# 
# 
# 
# 
# fpr_poplr <- data.frame(id=unique(d.s$id), sig=0)
# set.seed(2254)
# for(i in 1:length(unique(d.s$id))) {
# a <- d.s[which(d.s$id == unique(d.s$id)[i]),]
# re <- sample(1:12,12,replace=FALSE)
# 
# a <- a[re,c(1:2,4:64)]
# a$date <- d.s[which(d.s$id == unique(d.s$id)[i]),]$date
# a <- cbind(a[c('id','eye','date')], a[,3:63])
# 
# fpr_poplr$sig[i] <- poplr(a[re,1:64])$cslp/100
# }
# 
# ## plot p-value from reordered series (if smaller than 0.05 then false positive) against transformed variability
# pdf(file='FPvsVar.pdf', width=6,height=6)
# plot(plot_res.t$var, fpr_poplr$sig, pch=19, bty='n', xlab='Transformed Variability (SD of MD, dB)', 
#      ylab='P-value (Reordered Series)', col='maroon', main='P-value vs Variability')
# abline(h=0.05, v=1 ,lty=c(1,2), col=c('blue','green'))
# text(0.8,0.01, labels='FP', col='blue', cex=2)
# text(0.8,0.7, labels='TN', col='blue', cex=2)
# legend(1.1,0.6 ,'P<0.05 = False Positive', lty=1, col='blue', bty='n', lwd=2, text.col='gray', cex=1.2)
# text(plot_res.t[which(plot_res.t$id==120),]$var-0.04, fpr_poplr[which(fpr_poplr$id==4120),]$sig, labels='120', cex=0.7 )
# dev.off()
# 
# 
# ## assess fpr (and specificity) to compare with that of GPA in individual patients (px 4125 = highest FPR of GPA vs px 4130 = lowest FPR of GPA)
# set.seed(12345)
# perm <- data.frame(id=unique(d.s$id),no=1:2900, sig=0)
# perm <- perm[order(perm$id),]
# perm$no <- 1:100
# 
# 
# for (z in 1:100) {
# a <-  d.s[which(d.s$id==unique(d.s$id)[z]),]
#  for(i in 1:100){
# re <- sample(1:12,12,replace=FALSE)
# 
# a <- a[re,c(1:2,4:64)]
# a$date <- d.s[which(d.s$id == unique(d.s$id)[z]),]$date
# a <- cbind(a[c('id','eye','date')], a[,3:63])
# 
# perm[which(perm$id == a$id[1]),]$sig[i] <- poplr(a[re,1:64])$cslp/100
#  }
# }


# #distribution of p-values (from 100 randomly reordered series) for patient showing FP 
# pdf(file='Pdist.pdf', width=10, height=10)
# par(mfrow=c(2,2))
# hist(perm[ which(perm$id == fpr_poplr[which(fpr_poplr$sig<=0.05),1][1]), 3], breaks=20, border=F, 
#      xlab='P-values', main='Px 2: P-value Distribution from 100 Reordered Series')
# 
# hist(perm[ which(perm$id == fpr_poplr[which(fpr_poplr$sig<=0.05),1][2]), 3], breaks=20, border=F, 
#      xlab='P-values', main='Px 6: P-value Distribution from 100 Reordered Series')
# 
# hist(perm[ which(perm$id == fpr_poplr[which(fpr_poplr$sig<=0.05),1][3]), 3], breaks=20, border=F, 
#      xlab='P-values', main='Px 103: P-value Distribution from 100 Reordered Series')
# 
# hist(perm[ which(perm$id == fpr_poplr[which(fpr_poplr$sig<=0.05),1][4]), 3], breaks=20, border=F, 
#      xlab='P-values', main='Px 128: P-value Distribution from 100 Reordered Series')
# dev.off()

