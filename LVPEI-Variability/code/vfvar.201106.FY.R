
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

plot_res <- data.frame( id = unique( lvdat$id ), mean.s = 0, varb.s = 0)

for (i in 1:length(plot_res$id)) {
  idx <- plot_res$id[i]
  
  d <- subset (lvdat, id == idx, select = L1:L54 )
  d.t <- subset (lvdat, id == idx, select = L1:L54 )
  d.t<- 16.7764*(exp(0.078*d.t))
  
  plot_res$varb.s[i] <- mean( apply(d.t, 2, sd) )
  plot_res$mean.s[i] <- mean(apply (d, 2, mean))
}                               
plot_res2 <- plot_res [which (plot_res$varb.s > 0.01),]

pdf (file="plot.pdf", width=6, height=6)
plot (plot_res2$mean.s, plot_res2$varb.s, main = "VF Variability vs Mean Sensitivity", 
      col="blue", bty="n", pch=19, xlab="Mean Sensitivity (dB)", ylab="VF Variability (dB)")
dev.off()


