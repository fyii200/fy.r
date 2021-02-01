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

#configure data frame d
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)

#Create data frame (pe) to record results
pe <- data.frame(no=1:12600, id=0, l1=0, l2=0, l3=0, l4=0, l5=0,
                 slope=seq(from=0.1, to=4.1, by=0.2), pow=0)
pe$no <- 1:21
pe$idx <- 1:420
for(i in 1:30) {pe[which(pe$idx==1)[i]:which(pe$idx==420)[i],]$id <- unique(d$id)[i]}
pe <- pe[,-10]

###################### Simulation begins here ##########################
dat <- data.frame(n=1:100, p=0) #'storage' data frame to record p values for the 100 reordered series
set.seed(123243)
for (i in 1:30){
  
 #Empty plot for each px
 plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(0.1,4.1), type='n', ylab='Power',
       xlab='Rate (- dB/y)', main=paste0('Px ', unique(d$id)[i]-4000, ' : Power vs Rate of Progression') )
  
 for(k in 1:20){
   
 neg <- sample(c(11:35, 37:44, 46:64), 5, replace=FALSE ) #random selection of 3 loc (columns)
 
 plot_res <- pe[which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[k] : 
                  which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[k], ]
 
   for(l in 1:5){ plot_res[,l+2] <- neg[l] }
   pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : 
    which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 3:7] <- plot_res[,3:7]
    
   for(s in 1:21 ){
     
     a <- d[which(d$id==unique(d$id)[i]),] #isolate one (original) series
     for (q in 1:11) { a$date[q+1] <- d$date[1] + 180*q } #chg date intervals to 6mths
     for (j in 2:12){ a[j,neg] <- a[j,neg] - unique(pe$slope)[s]*(j-1)/2 }  #inject progression
     
     for(o in 1:100){
       dat$p[o] <- poplr(a)$cslp/100
       
       a <- d[which(d$id==unique(d$id)[i]),]
       a <- a[sample(1:12, 12, replace=FALSE),] #random reordering (rows)
       for (q in 1:11) { a$date[q+1] <- d$date[1] + 180*q } #chg date intervals to 6mths
       for (j in 2:12){ a[j,neg] <- a[j,neg] - unique(pe$slope)[s]*(j-1)/2 }  #inject progression
     }    
    
    plot_res[,9][s] <- length(which(dat$p<0.05))/100 #record power for each slope for each px in pe
    print(length(which(dat$p<0.05))/100) #to delete
   }
  
 pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : 
      which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 9] <- plot_res$pow #record pow in pe
 
 # PLOT Power vs Rate for each px #
 lines(plot_res$slope ,predict(loess(pow~slope, plot_res)), col='gray' )
 # Plot completed #
 
 }
  
}



# ############ PLOT #################################
# pdf(file='PowerVSRate110.pdf', width=6, height=6)
# #data frame for the average line
# avg <- data.frame(id=unique(d$id)[i], slope=seq(from=0.1, to=4.1, by=0.2), pow=0)
# for(s in 1:nrow(data.frame(unique(pe$slope))) ){
#   avg[which(avg$id==unique(d$id)[i] & avg$slope==unique(avg$slope)[s]),3] <-
# mean(pe[which(pe$id==unique(d$id)[i] & pe$slope==unique(pe$slope)[s]),9][1:20]) }
# 
# # PLOT Power vs Rate for each px #
# plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(0.1,4.1), type='n', ylab='Power', col='gray',
#      xlab='Rate (- dB/y)', main=paste0('Px ', unique(d$id)[i]-4000, ' : Power vs Rate of Progression') )
# 
# for(p in 1:20){
# plot_res <- pe[which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[p] :
#                  which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[p], ]
# lines(plot_res$slope ,predict(loess(pow~slope, plot_res)), col='gray' )}
# 
# #ADD the average line
# lines(avg$slope, predict(loess(pow~slope, avg)), lwd=1.5 )
# 
# 
# # ## mark 5 least-powered lines
# # for(m in 1:5){
# #   col=c('red','green','brown','blue','yellow')
# #   idx <- pe[which(pe$id==unique(d$id)[i] & pe$slope==max(unique(pe$slope))),]
# #
# #   plot_res <- pe[(as.integer(row.names(idx[which(idx[,9] == sort(idx[,9][1:50])[m])[1],][3:7]))-20):
# #        as.integer(row.names(idx[which(idx[,9] == sort(idx[,9][1:50])[m])[1],][3:7])),]
# #   lines(plot_res$slope ,predict(loess(pow~slope, plot_res)), col=col[m], lwd=1.5 )
# #
# #   legend(0.3, 1-m/20, paste(plot_res[21,3:7]-10, collapse=', '),
# #          bty='n', lty=1, lwd=3, col=col[m], cex=0.8, text.col='gray' )
# # }
# # ## 5 least-powered lines marked
# dev.off()
# ####################### Plot completed ##########################




