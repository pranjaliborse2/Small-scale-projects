library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2)
rm(list=ls())
#download.file("https://climexp.knmi.nl/data/inodc_temp100_US_New_York_80p_a.txt","seadata.csv")
sea = read.csv("seadata.csv")
sea1<-list()
seasummer<- list()
for(i in seq(180,804-24,12)){
  sea1<-c(sea1,mean(sea[(i+5):(i+8), 2]))
  seasummer <- c(seasummer,sea[(i+5):(i+8), 2])
}
plot(t,as.numeric(sea1),type="l",ylab="summer mean sea temp. anomaly", xlab="Year")
par(new=TRUE)
plot(t,tmean,type="l",col="red",ylab="summer mean temp.(C)", xlab="Year")
cor.test(as.numeric(tmean[26:51]),as.numeric(sea1[26:51]))

#source("Summer_temp analysis.R")
source("rproject2.R")
par(new = TRUE)
plot(seq(1970,2020),tavg,type="l")
cor.test(as.numeric(sea1),tavg)
frame = data.frame(as.numeric(sea1),tempanom[1:51],tavg)
frame = frame[order(as.numeric(sea1)),]
plot(frame$as.numeric.sea1.,frame$tavg,type="l")


start <- c(0, 0,0 , -3)
optim.fit <- optim(start, co.rmse, gr = NULL, time = as.numeric(sea1), co.levels = tempanom[1:51])
# Extract the parameters of the best-fit polynomial, and the root mean squared
# error, from optim_fit.
best.a <- optim.fit$par[1]
best.b <- optim.fit$par[2]
best.c <- optim.fit$par[3]
best.t.0 <- optim.fit$par[4]
best.rmse <- optim.fit$value

coopt = best.a*((as.numeric(sea1)-best.t.0)^2)+best.b*(as.numeric(sea1)-best.t.0)+best.c

frame = data.frame(as.numeric(sea1),tempanom[1:51],coopt)
frame = frame[order(as.numeric(sea1)),]

lines(frame$as.numeric.sea1.,frame$coopt)