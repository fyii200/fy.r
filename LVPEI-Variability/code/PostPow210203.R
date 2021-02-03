rm(list=ls())
setwd('/Users/fabianyii/Desktop/fy.r/LVPEI-Variability')

c <- c(1:7,13,17,19,20,26,27,29,30)

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

col <- c(brewer.pal(12, 'Paired'), brewer.pal(8, 'Dark2'), 
         brewer.pal(8, 'Accent'), brewer.pal(8, 'Set2') )

for(i in c){
  plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(0.1,4.1), type='n', ylab='Power',
       xlab='Rate (- dB/y)', main=paste0('Px ',unique(d$id)[i]-4000,': Power vs Rate of Progression') )
  
  for(s in 1:15)
  { idx <- d[row.names(d[which(d$id==unique(d$id)[i] & d$no==1),][s,]) : 
               row.names(d[which(d$id==unique(d$id)[i] & d$no==21),][s,]),]

  lines(predict(smooth.spline(idx$slope, idx$pow, df=5)), col='gray') #smooth.spline
  }
  
  lines(predict(smooth.spline(avg[which(avg$id==unique(avg$id)[i]),]$slope, 
                avg[which(avg$id==unique(avg$id)[i]),]$pow, df=5)), lwd=3, col=col[i]) #smooth.spline
}
dev.off()
################################## Completed ########################################

################### PLOT Power vs Rate of Progression (Combined) ########################
pdf(file='CombPowVsSlope.pdf', width=6, height=6)
plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(0.1,4.1), type='n', ylab='Power',
     xlab='Rate (- dB/y)', main='Power vs Rate of Progression')

for(i in c){
  plot_res <- avg[which(avg$id==unique(avg$id)[i]),]
  
  lines(predict(smooth.spline(plot_res$slope, plot_res$pow, df=5)), col=col[i], lwd=2) #smooth.spline
}

i <- c
legend('topleft', paste0(unique(d$id)[c]-4000), lty=1, lwd=5, 
       col=col[i], bty='n', cex=0.7)
dev.off()
############################## Completed #########################################


########################## VARIABILITY vs MS ####################################
##### pre-transformed variability of each px #####
dat <- read.csv('data/power.csv')

pdf(file='VarIndex.pdf', width=14, height=6)
par(mfrow=c(1,2))


plot(0,0, xlim=c(18,32), ylim=c(0.5,3), bty='n', ylab='Variability (SD)', xlab='MS' )
for (i in 1:30){
  idx <- dat[which(dat$id == unique(dat$id)[i]),]

  points( mean(apply(idx[,18:71], 2, mean)), mean(apply(idx[,18:71], 2, sd)), pch=19, col='gray')
  
  text(mean(apply(idx[,18:71], 2, mean))-0.3, mean(apply(idx[,18:71], 2, sd)), paste(unique(idx$id)-4000), 
        pch=0.8, cex=0.6, col='blue' )
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


####################### PLOT Power at a specific rate vs Variability #######################
pdf(file='PowVsVar.pdf', width=12, height=6)
par(mfrow=c(1,2))
co <- data.frame(no=unique(d$id)-4000, pow=0, var=0, tvar=0)

for(i in c){
  co$pow[i] <- avg[which(avg$id==unique(avg$id)[i] & avg$slope==1.1),]$pow
  idx <- dat[which(dat$id == unique(dat$id)[i]),]
  co$var[i] <- mean(apply(idx[,18:71], 2, sd)) # pre-transformed var
  co$tvar[i] <- mean(apply(1*(exp(0.078*idx[,18:71])), 2, sd)) # transformed var
}

f <- function(a,b,t,u){
plot(co[,t], co$pow, bty='n', pch=19, cex=0.6, ylim=c(0,1), xlim=c(0.5, a), ylab='Power',
     xlab='Variability (SD)', main=paste0('Power at -1.1 dB/y vs ', b) )
text(co[,t]+0.08, co$pow, labels=unique(avg$id)-4000, cex=0.8)
abline(lm(u, co[c,]), col='red')

lab <- as.numeric(cor.test(co[c,t], co[c,]$pow, alternative='two.sided', 'pearson')[3:4])
legend('topright', c(paste0('P = ', round(lab[1], digits=2) ), 
                     paste0('r = ', round(lab[2], digits=2) ) ), bty='n', text.col='gray', cex=1.5)
}

f(3, 'Variability', 3, pow~var)
f(2, 'Transformed Variability', 4, pow~tvar)

dev.off()
################################### Completed ############################################

