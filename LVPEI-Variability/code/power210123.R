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


####################### Power vs Year From Baseline ####################

## Create data frame to record results ##
ord <- data.frame(no=1:24000, id=0, y=seq(from=2, to=5.5, by=0.5), 
                  l1=0, l2=0, l3=0, l4=0, l5=0, p=0)
ord$no <- 1:8
ord$idx <- 1:800
for(i in 1:30) {ord[which(ord$idx==1)[i]:which(ord$idx==800)[i],]$id <- unique(d$id)[i]}
ord <- ord[,-10]
## Data frame created ##

####### Randomly 'progress' 5 VF locations then PoPLR then record P #######
set.seed(12321)
for (h in 1:30){
  
  for(i in 1:100){
sam <- sample(c(11:35,37:44,46:64), 5, replace=FALSE)
dat <- ord[which(ord$no==min(ord$no) & ord$id==unique(d$id)[h])[i] : 
            which(ord$no==max(ord$no) & ord$id==unique(d$id)[h])[i],] 

      for(l in 1:5) { 
        dat[,l+3] <- sam[l]
        ord[which(ord$no==min(ord$no) & ord$id==unique(d$id)[h])[i] : 
        which(ord$no==max(ord$no) & ord$id==unique(d$id)[h])[i], 4:8] <- dat[,4:8] }

      a <- d[which(d$id==unique(d$id)[h]),]
      for (q in 1:11) { a$date[q+1] <- a$date[1] + 180*q }

      for (r in 2:12) { a[r,sam] <- a[r,sam] - (r-1)*0.50 }
         
      for(s in 1:nrow(data.frame(unique(ord$y))) ){ 
       dat$p[s]  <- poplr(a[1:(s+4),])$cslp/100
       ord[which(ord$no==min(ord$no) & ord$id==unique(d$id)[h])[i] : 
            which(ord$no==max(ord$no) & ord$id==unique(d$id)[h])[i],9] <- dat$p }
   }
}


## PLOT Power vs Year ##
pdf(file='PowerVsY-1.pdf', width=18, height=24)
par(mfrow=c(6,5))
avg <- data.frame(y=unique(ord$y), p=0)
for(i in 1:30){
  
  plot(0,0,bty='n', pch=19, cex=0.4, ylim=c(0,1), xlim=c(3,5.5), type='n', ylab='1 - P-value', 
       xlab='Year', main=paste0('Px ', unique(d$id)[i]-4000, ' : Power vs Year') )
  
  for(p in 1:100) { 
    idx <- ord[which(ord$id == unique(d$id)[i] ),]
    plot_res <- idx[which(idx$no == 3)[p] : which(idx$no == max(idx$no))[p], ]
    # lines(loess.smooth(plot_res$y, 1-plot_res$p, span=1.1), col='gray')
    lines(plot_res$y, 1-plot_res$p,col='gray')
  }
  
  for(s in 1:nrow(data.frame(unique(ord$y))) ) { 
    avg$p[s] <- 1-mean(idx[which(idx$y==unique(idx$y)[s]),9]) }
  # lines(loess.smooth(avg$y, avg$p, span=1.1), lwd=1.8)
  lines(avg$y, avg$p, lwd=1.8)
  
  abline(h=0.95, lty=c(1,2)) 
  text(3,0.97, labels='a = 0.05', cex=0.8)
  
}
dev.off()



## PLOT Transformed Variability Index vs Power (at 5.5y, what is the % of detected progression for each px) ## 

#Compute MS and transform data to equalise variance
d <- read.csv('data/power.csv')
d <- d[,-1]
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)

plot_res.t <- data.frame(id=unique(d$id), var=0, ms=0, pow=0)
for(i in 1:30){ a <- d[which(d$id==unique(d$id)[i]),-c(1:10,36,45)]
plot_res.t$ms[i] <- mean(apply(a,2,mean)) }

for(i in 11:64){ d[,i] <- 3*(exp(0.078*d[,i] )) } #equalise variance

#compute variability index and power (at 5.5y, what is the % of detected progression) for each px

for(i in 1:30){
a <- d[which(d$id==unique(d$id)[i]),]
a <- a[,-c(36, 45)]
plot_res.t$var[i] <- sd(apply(a[,11:62],2,sd)) #variability

plot_res.t$pow[i] <- 
  length(which(ord[which(ord$y==5.5 & ord$id==unique(d$id)[i]),9] < 0.05))/100 #power
}


# ## Variability index vs MS ##
# pdf(file='VarVsMs.pdf', width=6, height=6)
# plot(plot_res.t$ms, plot_res.t$var, bty='n', pch=19, col='maroon', cex=1.4,
#      main='Variability Index vs MS', xlab='MS (dB)', ylab='Variability Index')
# abline(h=1, lty=2)
# text(plot_res.t$ms, plot_res.t$var+0.05, labels=plot_res.t$id-4000, col='gray')
# dev.off()


## Power vs Variability Index ##
pdf(file='VarVsPower5.5.pdf', width=6, height=6)
plot(plot_res.t$var, plot_res.t$pow, bty='n', pch=19, col='maroon', 
     main='Variability Index vs Power (5.5y)', xlab='Variability Index', ylab='Power (%)')
text(plot_res.t$var, plot_res.t$pow+0.03, labels=plot_res.t$id-4000, cex=0.8)
dev.off()






