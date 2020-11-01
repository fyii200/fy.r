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

for(i in 1:nrow(d)) {
  d$m[i] <- d$m[i] + runif(1, min=0.00001, max=0.0001)
  d$j0[i] <- d$j0[i] + runif(1, min=0.00001, max=0.0001)
  d$j45[i] <- d$j45[i] + runif(1, min=0.00001, max=0.0001)
  }

d.mad <- data.frame(name=ID$name, eye=ID$eye, m.mad=0, j0.mad=0, j45.mad=0)
for (i in 1: length(ID$name)) {
  idx <- which (d$name == ID$name[i] & d$eye == ID$eye[i])  
  e <- d [idx, ]
  a.m <- data.frame(unique(t(apply(expand.grid(e$m,e$m),1,sort))) )
  a.j0 <- data.frame(unique(t(apply(expand.grid(e$j0,e$j0),1,sort))) )
  a.j45 <- data.frame(unique(t(apply(expand.grid(e$j45,e$j45),1,sort))) )
  
  b.m <- a.m[-which(a.m$X1==a.m$X2),]
  b.j0 <- a.j0[-which(a.j0$X1==a.j0$X2),]
  b.j45 <- a.j45[-which(a.j45$X1==a.j45$X2),]
  
  d.mad$m.mad[i] <- round(mad(b.m$X1-b.m$X2), digits=3)
  d.mad$j0.mad[i] <- round(mad(b.j0$X1-b.j0$X2), digits=3)
  d.mad$j45.mad[i] <- round(mad(b.j45$X1-b.j45$X2), digits=3) }
print(d.mad) 

