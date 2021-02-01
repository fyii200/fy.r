rm(list=ls())
setwd('/Users/fabianyii/Desktop/fy.r/LVPEI-Variability')

#install latest version of visualFields
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")  # see RTools WS

# install.packages("devtools")
# library(devtools)
# install_github("imarinfr/vf1/source")
library("visualFields")
library('RColorBrewer')

# read sim results into r
d <- read.csv('data/simdat.csv')
d <- d[,-1]

#create a dataframe containing the average lines for all px
avg <- data.frame(no=1:630 ,id=0, slope=seq(from=0.1, to=4.1, by=0.2), pow=0)
avg$no <- 1:21
for(i in 1:30){ avg[which(avg$no==1)[i]: which(avg$no==21)[i],]$id <- unique(d$id)[i] }

# computing the average lines
for(i in 1:30){
  for(s in 1:21){
    avg[which(avg$id==unique(d$id)[i]),]$pow[s] <-
      mean(d[which(d$id==unique(d$id)[i] & d$slope==unique(d$slope)[s]),]$pow ) }
}

####################### PLOT Power vs Rate of Progression (Individual Plots) ###########################
pdf(file='PowVsSlope.pdf', width=12, height=12)
par(mfrow=c(3,3))

for(i in c(1,6,13,17,19,20,26,27,29,30)){
  plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(0.1,4.1), type='n', ylab='Power (%)',
       xlab='Rate (- dB/y)', main=paste0('Px ',unique(d$id)[i]-4000,': Power vs Rate of Progression') )
  
  for(s in 1:15)
  { idx <- d[row.names(d[which(d$id==unique(d$id)[i] & d$no==1),][s,]) : 
               row.names(d[which(d$id==unique(d$id)[i] & d$no==21),][s,]),]
  # lines(idx$slope, idx$pow, col='gray')
  lines(idx$slope, predict(loess(pow~slope, idx)), col='gray', lwd=0.3) }
  
  lines(avg[which(avg$id==unique(avg$id)[i]),]$slope, avg[which(avg$id==unique(avg$id)[i]),]$pow, lwd=2)
}
dev.off()
################################## Completed ########################################

################### PLOT Power vs Rate of Progression (Combined) ########################
pdf(file='CombPowVsSlope.pdf', width=6, height=6)
plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(0.1,4.1), type='n', ylab='Power (%)',
     xlab='Rate (- dB/y)', main='Power vs Rate of Progression')

col <- c(brewer.pal(12, 'Paired'), brewer.pal(8, 'Dark2'), 
         brewer.pal(8, 'Accent'), brewer.pal(8, 'Set2') )

for(i in c(1,6,13,17,19,20,26,27,29,30)){
  plot_res <- avg[which(avg$id==unique(avg$id)[i]),]
  lines(plot_res$slope, plot_res$pow, col=col[i], lwd=2 )
  # lines(plot_res$slope, predict(loess(pow~slope, plot_res)), col=col[i], lwd=2 ) #loess (smooth)
  
  # for (s in 1:15)
  # { idx <- d[row.names(d[which(d$id==unique(d$id)[i] & d$no==1),][s,]) : 
  #              row.names(d[which(d$id==unique(d$id)[i] & d$no==21),][s,]),] 
  # # lines(idx$slope, idx$pow, col=col[i], lwd=0.2)
  # lines(idx$slope, predict(loess(pow~slope, idx)), col=col[i], lwd=0.2) }
}

i <- c(1,6,13,17,19,20,27,29,30)
legend('topleft', paste0(unique(d$id)[c(1,6,13,17,19,20,26,27,29,30)]-4000), lty=1, lwd=5, 
       col=col[i], bty='n', cex=0.7)
dev.off()
############################## Completed #########################################


########################## VARIABILITY vs MS ####################################
##### pre-transformed variability of each px #####
dat <- read.csv('data/power.csv')

pdf(file='VarIndex.pdf', width=14, height=6)
par(mfrow=c(1,2))

plot(0,0, xlim=c(18,32), ylim=c(0.5,3), bty='n', ylab='SD (Pre-transformed)', xlab='MS')
for (i in 1:30){
  idx <- dat[which(dat$id == unique(dat$id)[i]),]
  points( mean(apply(idx[,18:71], 2, mean)), mean(apply(idx[,18:71], 2, sd)),
          pch=19, col='gray')
  
  text(mean(apply(idx[,18:71], 2, mean))-0.3, mean(apply(idx[,18:71], 2, sd)),
       paste(unique(idx$id)-4000), pch=0.8, cex=0.6, col='blue' )
}

###### Transformed variability #######
plot(0,0, xlim=c(18,32), ylim=c(0,2.5), bty='n', ylab='SD (Transformed)', xlab='MS')
for (i in 1:30){
  idx <- dat[which(dat$id == unique(dat$id)[i]),]
  points( mean(apply(idx[,18:71], 2, mean)), 
          mean(apply(1*(exp(0.078*idx[,18:71])), 2, sd)), pch=19, col='gray')
  
  text(mean(apply(idx[,18:71], 2, mean))-0.3, mean(apply(1*(exp(0.078*idx[,18:71])), 2, sd)),
       paste(unique(idx$id)-4000), pch=0.8, cex=0.6, col='maroon' )
}
abline(h=1, lty=2)
dev.off()
####################################### Completed ##############################################


####################### PLOT Slope of best fit line vs Variability #######################
pdf(file='SlopeVsVar.pdf', width=6, height=6)
co <- data.frame(no=unique(d$id)-4000, slope=0, var=0)

c <- c(1,6,13,17,19,20,26,27,29,30)
for(i in c){
  plot_res <- avg[which(avg$id==unique(avg$id)[i]),]
  idx <- dat[which(dat$id == unique(dat$id)[i]),]
  co$slope[i] <- lm(pow~slope, plot_res[which(plot_res$pow < 0.95),])$coefficients[2]
  co$var[i] <- mean(apply(idx[,18:71], 2, sd)) # pre-transformed var
  # co$var[i] <- mean(apply(1*(exp(0.078*idx[,18:71])), 2, sd)) # transformed var
}

plot(co$var, co$slope, bty='n', pch=19, cex=0.6, ylim=c(0,1), xlim=c(1,3), ylab='Slope (Power / Rate)',
     xlab='Variability (SD)', main='Slope vs Variability')
text(co$var+0.08, co$slope, labels=unique(avg$id)-4000, cex=0.8)
abline(lm(slope~var, co[c,]), col='red')

lab <- as.numeric(cor.test(co[c,]$var, co[c,]$slope, alternative='two.sided', 'pearson')[3:4])
legend('topright', c(paste0('P = ', round(lab[1], digits=2) ), 
                     paste0('r = ', round(lab[2], digits=2) ) ), bty='n', text.col='gray', cex=1.5)

dev.off()
################################### Completed ############################################



