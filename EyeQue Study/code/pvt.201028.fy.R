rm (list=ls())
setwd ("/Users/fabianyii/Desktop/fy.r/EyeQue Study")
# setwd ("C:/Users/paul_/Google Drive/R.Stuff/pvt/")

library("readxl") #attach readxl package
library(RColorBrewer)
library(gtools)

d <- data.frame(read_excel("data/eyeque.xlsx"))

d$m <- d$sphere + d$cyl/2 
d$j0 <- -d$cyl/2*cos(2*d$axis*pi/180)
d$j45 <- -d$cyl/2*sin(2*d$axis*pi/180)

ID <- unique(data.frame(name = d$name, eye = d$eye))

#create plots

pdf (file = "Plots.pdf", width = 12, height = 8)

for(i in 1: length(ID$name)){
  print(i)
  idx <- which (d$name == ID$name[i] & d$eye == ID$eye[i])  
  e <- d [idx, ]
  
  par(mfrow = c(2,3), cex.main = 1.5, cex.lab = 1.2, font.lab = 4, font.main = 2, font.axis = 3, bty="n")

  plot.pvt <- function(x, y, a, b) {
    plot(x, y, main = paste0(ID$name[i], "(",ID$eye[i],")", ":", " ", b, ",", " ", a),
         xlab = paste0(a, " ","(D)"), ylab = paste0(b, " ","(D)"),
         xlim = c(min(x)-1, max(x)+1), ylim = c(min(y)-1, max(y)+1),
         abline (h = mean(y), v = mean(x), lty = 1, lwd = 0.15),
         pch = 19, cex = 1.2, col = brewer.pal(10, 'RdYlBu'))
    lines (x, y, lty = 1, lwd = 0.1)
    text(x, y, labels = row.names(data.frame(x)), pos=2, cex=0.7) }
  
  plot.pvt( e$m, e$j0, capwords(colnames(e[7])), capwords(colnames(e[8])))
  plot.pvt( e$m, e$j45, capwords(colnames(e[7])), capwords(colnames(e[9])))
  plot.pvt( e$j0, e$j45, capwords(colnames(e[8])), capwords(colnames(e[9])))
  plot.pvt( e$sphere, e$cyl, capwords(colnames(e[4])), capwords(colnames(e[5])))
  
  e$slope <- (5*sin(e$axis*pi/180))/(5*cos(e$axis*pi/180))

  plot(0, 0, cex=0.1, pch=3, xlab="90°", ylab="180°", 
       main = paste0(ID$name[i], "(",ID$eye[i],")", ":", " ", "Axis"), 
       bty="n", xaxt="n", yaxt="n") 
  
  abline(h=0, v=0, lwd=0.5)
  
  for (i in 1:length( e$slope ) ) {abline(a=0, b=e$slope[i], col="red") } }
dev.off()