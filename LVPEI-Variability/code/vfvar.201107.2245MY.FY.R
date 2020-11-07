
# work on Variability Index
# Fabian Yii, Paul Artes

rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/LVPEI-Variability/")

# setwd("c:/Users/paul_/Google Drive/People_2/Fabian Yii/fy.r/LVPEI-Variability/")


# install.packages("devtools")
# library(devtools)
# install_github("imarinfr/vf1/source")

library( "visualFields" )

lvdat    <- read.csv( "data/LVPFirst100Data_processed.2.csv", stringsAsFactors = FALSE )
lvdat$date <- as.Date(lvdat$date, "%m/%d/%y")

lvdat.h <- stack(lvdat[,12:65])
hist(lvdat.h$values, xlab = "Sensitivity (dB)", main = "Sensitivity Distribution", ylim=c(0,10000), xlim=c(-10,40) )

plot_res <- data.frame( id = unique( lvdat$id ), mean.s = 0, varb.s = 0)

# lvdat[12:65] [lvdat[12:65] <= 0] <- NA

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
