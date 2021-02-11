rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/LVPEI-Variability/")

d <- read.csv('data/power.csv')
d <- d[,-1]

#install latest version of visualFields
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")  # see RTools WS

# install.packages("devtools")
# library(devtools)
# install_github("imarinfr/vf1/source")
# install.packages('RColorBrewer')
library("visualFields")
library('RColorBrewer')

# #create data frame to record results
# pe <- data.frame(no=1:9900, id=0, l1=0, l2=0, l3=0, l4=0, l5=0, slope=c(0, seq(0.1,4.1,0.2)), pow=0)
# 
# pe$no <- 1:22
# pe$idx <- 1:330
# for(i in 1:30) {pe[which(pe$idx==1)[i]:which(pe$idx==330)[i],]$id <- unique(d$id)[i]}
# pe <- pe[,-10]
# #

#read existing simulation results
pe <- read.csv('data/simdat.csv')
pe <- pe[,-1]

#configure data frame d
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)

for(i in 1:30){
  d[which(d$id==unique(d$id)[i]),]$id <- i
  pe[which(pe$id==unique(pe$id)[i]),]$id <- i
}


###################### Simulation begins here ##########################
dat <- data.frame(n=1:100, p=0) #'storage' data frame to record p values for the 100 reordered series
set.seed(181743)

for (i in 22){
  
  #Empty plot for each px
  plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(0.1,4.1), type='n', ylab='Power',
       xlab='Rate (- dB/y)', main=paste0('Px ', unique(d$id)[i], ' : Power vs Rate of Progression') )
  
  for(k in 1:15){
    
    neg <- sample(c(11:35, 37:44, 46:64), 5, replace=FALSE ) #random selection of 3 loc (columns)
    
    plot_res <- pe[which(pe$no==min(pe$no) & pe$id==unique(pe$id)[i])[k] : 
                     which(pe$no==max(pe$no) & pe$id==unique(pe$id)[i])[k], ]
    
    for(l in 1:5){ plot_res[,l+2] <- neg[l] }
    pe[which(pe$no==min(pe$no) & pe$id==unique(d$id)[i])[k] : 
         which(pe$no==max(pe$no) & pe$id==unique(d$id)[i])[k], 3:7] <- plot_res[,3:7]
    
    for(s in 1:22 ){
      
      for(o in 1:100){
        a <- d[which(d$id==unique(d$id)[i]),]
        a <- a[sample(1:12, 12, replace=FALSE),] #random reordering (rows)
        for (q in 1:11) { a$date[q+1] <- d$date[1] + 180*q } #chg date intervals to 6mths
        # for (j in 2:12){ a[j,neg] <- a[j,neg] - unique(pe$slope)[s]*(j-1)/2 }  #inject progression
        for (j in 2:12){ a[j,neg] <- a[j,neg] - unique(pe$slope)[s]*(j-1)/2 }  #inject progression
        dat$p[o] <- poplr(a)$cslp/100
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

# write.csv(pe, 'simdat.csv') #to delete








# for (i in 1:30){
#   
#   for(k in 1:15){
#     
#     plot_res <- pe2[which(pe2$no==min(pe2$no) & pe2$id==unique(pe2$id)[i])[k],] 
# 
#     neg <- as.numeric(plot_res[,3:7])
#       
#       for(o in 1:100){
#         a <- d[which(d$id==unique(d$id)[i]),]
#         a <- a[sample(1:12, 12, replace=FALSE),] #random reordering (rows)
#         for (q in 1:11) { a$date[q+1] <- d$date[1] + 180*q } #chg date intervals to 6mths
#         for (j in 2:12){ a[j,neg] <- a[j,neg] - unique(pe2$slope)[1]*(j-1)/2 }  #inject progression
#         dat$p[o] <- poplr(a)$cslp/100
#       }    
#       
#       plot_res[,9] <- length(which(dat$p<0.05))/100 #record power for each slope for each px in plot_res
#       print(length(which(dat$p<0.05))/100) #to delete
# 
#     
#     pe2[which(pe2$no==min(pe2$no) & pe2$id==unique(d$id)[i])[k],9]  <- plot_res$pow #record pow in pe
#   }
# }








