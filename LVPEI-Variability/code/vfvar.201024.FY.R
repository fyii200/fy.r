
# work on Variability Index
# Fabian Yii, Paul Artes

# To do: make a scatterplot variability vs sensitivity using LVPEI data


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
  variab <- mean( apply( m, 2, sd) )
  m.s <- mean( apply(m,2,mean) )
  
  print(variab) #see lines 31 & 32: how do I organize all of the numerical values in a single data frame? I tried in line 33 but it did not work
  print(m.s)
  # p <- data.frame("variab"=variab, "m.s"=m.s)
}                               
# plot(p)
plot(m.s, variab, ylim=c(-3,6)) #I put the function outside the loop but it will only generate a plot from a single patient 

