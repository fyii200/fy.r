rm(list=ls())
setwd("/Users/fabianyii/Desktop/fy.r/myopia/")
# d <- read.csv("data/myopia2.csv")
d <- read.csv("data/myopia.csv")

# r.d <- d[which (d$time == "bef" & d$eye == "r"),] 
# l.d <- d[which (d$time == "bef"& d$eye == "l"),]
# t.test (r.d$ct, l.d$ct, alternative="two.sided",paired=TRUE) #paired t-test baseline R vs baseline L

ex <- d[which (d$eye == "exp" & d$time == "bef"),4:12]
co <- d[which (d$eye == "cont" & d$time == "bef"), 4:12]
dat <- data.frame(b.exp = c(ex[,1],ex[,2],ex[,3],ex[,4],ex[,5],ex[,6],ex[,7],ex[,8],ex[,9]) )
# dat$b.cont <- c(co[,1],co[,2],co[,3],co[,4],co[,5],co[,6],co[,7],co[,8],co[,9])
# shapiro.test(dat$b.cont-dat$b.exp)
# t.test(dat$b.exp, dat$b.cont, alternative="two.sided", paired=TRUE) #baseline exp vs baseline cont

a <- stack(d[1:23,4:12])
a <- data.frame(eye = "exp", time = "bef", ct = a$values)
b <- stack(d[24:46,4:12])
b <- data.frame(eye = "exp", time = "fl", ct = b$values)
c <- stack(d[47:69, 4:12])
c <- data.frame(eye = "exp", time = "fu", ct = c$values)
fin.exp <- rbind(a,b,c)
shapiro.test(fin.exp$ct)
kruskal.test(ct~time, data=fin.exp)

e <- stack(d[which (d$eye == "cont" & d$time == "bef"),4:12])
e <- data.frame(eye = "cont", time = "bef", ct = e$values)
f <- stack(d[which (d$eye == "cont" & d$time == "fl"),4:12])
f <- data.frame(eye = "cont", time = "fl", ct = f$values)
g <- stack(d[which (d$eye == "cont" & d$time == "fu"),4:12])
g <- data.frame(eye = "cont", time = "fu", ct = g$values)
fin.cont <- rbind (e,f,g)
shapiro.test(fin.cont$ct)
kruskal.test(ct~time, data=fin.cont)







