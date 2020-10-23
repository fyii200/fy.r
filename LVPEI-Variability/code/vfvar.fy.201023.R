rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/LVPEI-Variability/")
# install.packages("devtools")
# install_github("imarinfr/vf1/source")
library(devtools)
library("visualFields")

lvdat    <- read.csv( "data/LVPFirst100Data_processed.2.csv", stringsAsFactors = FALSE )
lvdat$date <- as.Date(lvdat$date, "%m/%d/%y")

# for(i in 1:54){
# names(lvdat)[i+11] <- i 
# }

plot_res <- unique(data.frame(eye = lvdat$eye, id = lvdat$id))
for (i in 1:length(plot_res$id)) {
  idx <- which(lvdat$id == plot_res$id[i] & lvdat$eye == plot_res$eye[i])
  d <- data.frame( lvdat[idx[1:length(idx)],2:65] )
  m <- d[,-c(1:10)]
  var <- mean( apply( m, 2, sd) )
  m.s <- mean( apply(m,2,mean) )
  plot(m.s, var, ylim=c(-3,6))
}
