
# work on Variability Index
# Fabian Yii, Paul Artes

rm(list=ls())
#setwd("/Users/fabianyii/Desktop/fy.r/LVPEI-Variability/")

setwd("c:/Users/paul_/Google Drive/People_2/Fabian Yii/fy.r/LVPEI-Variability/")


# install.packages("devtools")
# library(devtools)
# install_github("imarinfr/vf1/source")

library( "visualFields" )

lvdat    <- read.csv( "data/LVPFirst100Data_processed.2.csv", stringsAsFactors = FALSE )
lvdat$date <- as.Date(lvdat$date, "%m/%d/%y")

lvdat.h <- stack(lvdat[,12:65])
hist(lvdat.h$values, xlab = "Sensitivity (dB)", main = "Sensitivity Distribution", ylim=c(0,10000), xlim=c(-10,40) )

plot_res <- data.frame( id = unique( lvdat$id ), mean.s = 0, varb.s = 0)

# lvdat[12:65] [lvdat[12:65] <= 0] <- NA   # you were close here!

thresh <- subset (lvdat, select=c(L1:L54))   
thresh [thresh < 0] <- -1                
lvdat[,12:65] <- thresh   # 

hist( as.matrix( thresh ), # this is what I used to do before I learned 'stack' from you :)
      xlab = "Sensitivity (dB)", main = "Sensitivity Distribution", xlim=c(-1,40),
      breaks=seq(from=-1, to=40),   # here, I'm forcing it to plot in steps of 1 dB.
      col='blue', border='white')

d <- lvdat[, c( 2, 12:65)]

d.sd   <- aggregate( . ~ id, d, sd )
d.mean <- aggregate( . ~ id, d, mean )

#  see ?reshape to make the data 'long' -- i.e. columns id, eye, location, mean_s, sd
#
#


for (i in 1:length(plot_res$id)) {
  idx <- plot_res$id[i]
  
  d <- subset (lvdat, id == idx, select = L1:L54 )
  d [d<=0] <- NA
  
  d [,c(which(as.character(apply(is.na(d),2,which)) != "integer(0)"))] <- NA
  
  d.t <- subset (lvdat, id == idx, select = L1:L54 )
  d.t [d.t<=0] <- NA
  d.t [,c(which(as.character(apply(is.na(d.t),2,which)) != "integer(0)"))] <- NA
  d.t<- 16.7764*(exp(0.078*d.t))
  
  plot_res$varb.s[i] <- mean( apply(d.t, 2, sd),na.rm = TRUE)
  plot_res$mean.s[i] <- mean(apply (d, 2, mean),na.rm = TRUE)
}
  
plot_res2 <- plot_res [which (plot_res$varb.s > 0.01),]

pdf (file="plot.pdf", width=6, height=6)
plot (plot_res2$mean.s, plot_res2$varb.s, main = "VF Variability vs Mean Sensitivity", 
      col="blue", bty="n", pch=19, xlab="Mean Sensitivity (dB)", ylab="VF Variability (dB)",
      xlim = c(5,35), ylim = c(0,60) )
dev.off()
