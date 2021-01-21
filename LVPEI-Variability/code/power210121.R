# Relate power to variability index #
rm(list=ls())
setwd('/Users/fabianyii/Desktop/fy.r/LVPEI-Variability')

# install.packages('visualFields')
# library(visualFields)
# data('vfArtes2014')
# dat <- vfArtes2014
# write.csv(dat, 'power.csv')

d <- read.csv('data/power.csv')
d <- d[,-1]

#install latest version of visualFields
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")  # see RTools WS

# install.packages("devtools")
# library(devtools)
# install_github("imarinfr/vf1/source")
library("visualFields")

# configure data frame d
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)



#GENERATE 5 random VF locations (except L26 & L35 -> blind spots) to be 'injected' with a negative linear trend
pe <- data.frame(no=1:111000, id=0, slope=seq(from=0.4, to=4, by=0.1), 
                 l1=0, l2=0, l3=0, l4=0, l5=0, p=0)
pe$no <- 1:37
pe$idx <- 1:3700
for(i in 1:30) {pe[which(pe$idx==1)[i]:which(pe$idx==3700)[i],]$id <- unique(d$id)[i]}
pe <- pe[,-10]


set.seed(12321)
for (h in 4:5){
  
  for (i in 1:100) {
    sam <- sample(c(11:35,37:44,46:64), 5, replace=FALSE)
    
    dat <- pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[h])[i] : 
                which(pe$no==max(pe$no) & pe$id==unique(d$id)[h])[i],]
    for(l in 1:5) { 
      dat[,l+3] <- sam[l]
      pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[h])[i] : 
           which(pe$no==max(pe$no) & pe$id==unique(d$id)[h])[i], 4:8] <- dat[,4:8] }
    
    
    for(s in 1:nrow(data.frame(unique(pe$slope))) ) {
      
      a <- d[which(d$id==unique(d$id)[h]),]
      for (q in 1:11) {
        a$date[q+1] <- a$date[1] + 180*q }
      
      for (r in 2:12) { 
        a[r, as.numeric(dat[1,4:8])] <- 
          a[r, as.numeric(dat[1,4:8])] - unique(pe$slope)[s]*((r-1)/2) 
      }
      
      dat[s,9] <- poplr(a)$cslp/100
      
    }
    
    pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[h])[i] : 
         which(pe$no==max(pe$no) & pe$id==unique(d$id)[h])[i], 9] <- dat[,9]
  } 
}


# ############## PLOT Power vs Rate of Progression ###############
# pdf(file='PowerVsRate.pdf', width=10, height=13)
# par(mfrow=c(3,2))
avg <- data.frame(slope=unique(pe$slope), p=0)
for(i in 1:30){
  
plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(0.4,4), type='n', ylab='1 - P-value', 
     xlab='Slope (- dB/y)', main=paste0('Px ', unique(d$id)[i]-4000,' : Power vs Annual Rate of Progression') )
  
  for(p in 1:100) { 
  idx <- pe[which(pe$id == unique(d$id)[i] ),]
  plot_res <- idx[which(idx$no == min(idx$no))[p] : which(idx$no == max(idx$no))[p], ]
  lines(plot_res$slope, 1-plot_res$p, col='gray')
  }
  
 for(s in 1:nrow(data.frame(unique(pe$slope))) ) { 
   avg$p[s] <- 1-mean(idx[which(idx$slope==unique(idx$slope)[s]),9]) }
lines(avg$slope, avg$p, lwd=1.8)

abline(h=0.95) 
text(1,0.97, labels='a = 0.05', cex=0.8)

#   for(c in c(96:100)){
#     idx2 <- idx[which(idx$slope==4),][order(idx[which(idx$slope==4),9]),]
#     pow <- idx[ (which(idx$p == idx2$p[c] & idx$no == idx2$no)-36) :
#                    which(idx$p == idx2$p[c] & idx$no == idx2$no), ]
# 
#   col <- c('orchid2', 'green', 'blue', 'yellow1', 'orangered')[c-95]
#   points(pow$slope, 1-pow$p, col=col, pch=19, cex=0.4)
#   lines(pow$slope, 1-pow$p, col=col)
# }
# 
# 
# lo <- paste(t(idx2[96:100,4:8]-10))
# legend(2, 0.5, bty='n', c(paste(lo[1:5], collapse=', '), paste(lo[6:10], collapse=', '),
#                           paste(lo[11:15], collapse=', '), paste(lo[16:20], collapse=', '),
#                           paste(lo[21:25], collapse=', ') ) , col=c('orchid2', 'green', 'blue', 'yellow1', 'red'),
#        lty=1, lwd=4, title='VF Locations', text.col='gray', cex=1.1)

  
}

dev.off()












































