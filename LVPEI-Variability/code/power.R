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

install.packages("devtools")
library(devtools)
install_github("imarinfr/vf1/source")
library("visualFields")

# configure data frame d
d <- d[,c(1,9,5,6,8,7,12,13,14,15,17:70)]
names(d)[c(2:10)] <- c('eye','date','time','age','type','fpr','fnr','fl','duration')
d$date <- as.Date(d$date)

##########   isolate one vf series for simulation (px 4001 or 1; transformed variability index ~ 2.1) ##########
a <- d[1:12,]

## change date intervals between tests to 6 months ####
for (i in 1:11) {
a$date[i+1] <- a$date[1] + 180*i }



#GENERATE 5 random VF locations (except L26 & L35 -> blind spots) to be 'injected' with a negative linear trend
perm <- data.frame(id=4001, p=1:100, l1=0, l2=0, l3=0, l4=0, l5=0)
prop <- data.frame(id=4001 ,slope=seq(0.6,6.2,by=0.2), propor=0)

loc <- data.frame(no=1:2900, slope=prop$slope, l1=0, l2=0, l3=0, l4=0, l5=0)
loc <- loc[order(loc$slope),]
loc$no <- 1:2900

set.seed(12321)

for (k in 1:29) {
  
  for (i in 1:100) {
    sam <- sample(c(11:35,37:44,46:64), 5, replace=FALSE)
    
    a <- d[1:12,]
    for (z in 1:11) {
      a$date[z+1] <- a$date[1] + 180*z }
    
    for (j in 1:11){ a[j+1, sam] <- as.numeric(a[j+1, sam]) - prop$slope[k] * (j/2) }
    
    perm$p[i] <- poplr(a)$cslp/100
    perm[i,3:7] <- sam-10 #which VF locations were injected with the negative trend?
    vfspa(a,file=paste0(prop$slope[k],'dBperm',i,'.pdf')) #generate PoPLR plots
  }
  
  loc[which(loc$slope == prop$slope[k]),3:7] <- perm[,3:7]
  
  if(any(perm$p < 0.05) == 'FALSE') {prop$propor[k] = 0} else{prop$propor[k] = nrow( as.data.frame(which(perm$p < 0.05)) )/100}
}   
# ############# px 4001 or 1 COMPLETED ##################################



#PLOT proportion of TP vs slope
pdf(file=paste0('PowerReport','-',prop[k,2],'.pdf'), width=10, height=10)
layout(matrix(c(1,2,3,4,5,6), nrow=3, byrow=T))
scatter.smooth(prop$slope, prop$propor, degree=2.5, pch=19, col='maroon', cex=0.8, bty='n', xlab='Slope (-dB per year)',
               ylab='Proportion of TP (%)', main='Px 1')



######### Baseline pre-transformed variability of sensitivity measurements at each of the 54 VF locations #######
a <- d[1:12,]
var <- data.frame(loc=1:54)

for (i in 11:64) { 
  var$sd[i-10] <- sd(a[,i])
  var$md[i-10] <- mean(a[,i]) }

#PLOT pre-transformed variability (SD) at each location
plot(var$loc,var$sd, bty='n', pch=19, ylab='SD', xlab='VF Location', 
     main='Pre-transformed Variability at 54 Locations', col='maroon')
abline(h=median(var$sd)) # the median
abline(h=as.numeric(quantile(var$sd, 0.75)), col='blue') #70th percentile
abline(h=as.numeric(quantile(var$sd, 0.9)), col='green') #90th percentile
text(which(var$sd > median(var$sd))-1.3, var[which(var$sd > median(var$sd)),2], labels=which(var$sd > median(var$sd)), cex=0.8)
legend('topright',c('90th','75th','50th'), lty=1, col=c('green','blue','black'), bty='n', text.col='gray', cex=1.3)



#dummy VF plot (locations with SD > 90th percentile -> s=0; SD between 75th and 90th -> s=10; SD between 50th and 75th -> s=20; below median -> s=30 )
dummy <- d[1,]
dummy[,11:64] <- 1:54
dummy[,var[which(var$sd>=as.numeric(quantile(var$sd, 0.90))),1]+10] <- 0 #locations with SD ≥ 90th percentile
dummy[,var[which(var$sd>=as.numeric(quantile(var$sd, 0.75)) & var$sd<as.numeric(quantile(var$sd, 0.90)) ),1]+10] <- 10 #locations with SD ≥75th but <90th
dummy[,var[which(var$sd>as.numeric(quantile(var$sd, 0.50)) & var$sd<as.numeric(quantile(var$sd, 0.75)) ),1]+10] <- 20 #locations with SD >50th but <75th
dummy[,var[which(var$sd<=as.numeric(quantile(var$sd, 0.50))),1]+10] <- 30 #locations with SD ≤ 50th percentile

#PLOT 'variability' vf plot; darker grids correspond to higher variability
vfplot(dummy,'s')
legend('topleft','Variability Plot', bty='n', cex=1.3)

#PLOT total deviation map (average sensitivities across 12 VF series and find TD for each location)
vfplot(vfmean(vfselect(a, sel="first", n=12), by="eye"),'td')
legend('topleft','TD Plot (12 VFs)', bty='n', cex=1.3)


######### POST-HOC #############
## DEFINE 'K' !! <- which 'Prop' row contains slope of interest?
# k <- 9

#Which slope (rate of progression) are you investigating?
perm[,3:7] <- loc[which(loc$slope == prop[k,2]) ,3:7] # loc$slope == ______?

# Re-generate the p-values associated with each of the 100 deteriorated series at a specific rate (k)
k <- which(prop$slope == prop[k,2]) # using k, which slope (rate of progression) are you investigating?
for (i in 1:100) { sam <- as.numeric(perm[i,3:7]) + 10
  a <- d[1:12,]
  for (z in 1:11) {
    a$date[z+1] <- a$date[1] + 180*z }

  for (j in 1:11){ a[j+1, sam] <- as.numeric(a[j+1, sam]) - prop$slope[k] * (j/2) }

  perm$p[i] <- poplr(a)$cslp/100 }

#Frequency (VF locations) for 'false-negative' series and show frequency on a VF PLOT
dummyfn <- a[1,] 
dummyfn[,11:64] <- 0
dummyfn[,as.numeric(paste(data.frame(table(stack(perm[which(perm$p >0.05),3:7])$values) )[,1]))+10] <-
  as.numeric(paste(data.frame(table(stack(perm[which(perm$p >0.05),3:7])$values) )[,2]))

vfplot(dummyfn,'s',cex=1.4)
legend('topleft',c('False-negative Series: ', paste0('-', prop[k,2],'dB/y') ), title='Freq. of VF Locations', cex=0.6, bty='n')

#Frequency table (VF locations) for 'true-positive' series and show frequency on a VF PLOT
dummytp <- a[1,] 
dummytp[,11:64] <- 0
dummytp[,as.numeric(paste(data.frame(table(stack(perm[which(perm$p <0.05),3:7])$values) )[,1]))+10] <-
  as.numeric(paste(data.frame(table(stack(perm[which(perm$p <0.05),3:7])$values) )[,2]))

vfplot(dummytp,'s',cex=1.4)
legend('topleft',c('True-positive Series: ', paste0('-', prop[k,2],'dB/y') ), title='Freq. of VF Locations', cex=0.6, bty='n')

dev.off()


