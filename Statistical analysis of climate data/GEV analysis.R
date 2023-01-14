rm(list=ls())
temp_tab <- read.csv("D:\\UIUC Academics\\SEM2 UIUC\\Risk analysis\\Project\\NYC Daily temp. data.csv")
library(fExtremes)
temp = as.vector(temp_tab[,6])
prob_temp = (1:length(temp))/(length(temp)+1)

# Calculate theoretical quantiles
temp_quant=qnorm(prob_temp,mean=mean(temp),sd=sd(temp))
#plot(sort(temp), temp_quant,main="QQ Normal",xlab="Observations (Celsius)")
    #ylab="Theoretical (Celsius)")
#qqnormPlot(temp)
tmax<- list()

tmean <-list()
freq <-vector()
for(i in seq(1,18628-365,365)){
  tmax1 = max(temp[i:(i+364)])
  tmax<- c(tmax,tmax1)
  tm = mean(temp[i:(i+364)])
  tmean<- c(tmean,tm)
  freq<- c(freq,length(unlist(list(temp[i:(i+364)][temp[i:(i+364)]>90]))))
}
plot(seq(2000,2020),tmax[31:51], type="l",xlab= "Year",ylab="Max.summer temperature")
abline(lm(as.numeric(freq[1:31])~seq(1970,2000)),col="red")
tmax2 = unlist(tmax)
#maxt = unlist(maxt)
#hist(tmax2)
plot(density(unlist(tmean)),col="red",lwd=3)
#qqnormPlot(tmax2)
#tmax5yr = blockMaxima(tmax2,block = 5)
#plot(density(tmax5yr),col="red",lwd=3)
#plot(density(temp_tab[,4][10951:length(temp_tab)]),col="red",lwd=3)
#plot(density(maxt),col="red")

prob = (1:length(tmax2))/(length(tmax2)+1)

# Use gevFit to fit GEV function to data
gev_est=gevFit(tmax2,type="pwm")

# Return the estimated GEV parameters
xi=gev_est@fit$par.ests[1]
mu=gev_est@fit$par.ests[2]
beta=gev_est@fit$par.ests[3]
quant=qgev(prob,xi = xi, mu = mu, beta = beta)


# compare theoretical quantiles with data
plot(quant,sort(tmax2),main="QQ Estimate",xlab="Theoretical GEV Quantiles",
     ylab="Observations (block max)")

cdf_data=ecdf(tmax2)
# Use CDF to convert data quantiles to probabilities
p_data=cdf_data(tmax2)
plot(tmax2,1-p_data,pch=19,main="Mod.1 Temperature Risk Curve, 
  Central Illinois",ylab="log(1-CDF)", xlab="Temperature Anomalies (C)",xlim=c(min(tmax2),max(tmax2+30)) , ylim=c(0.01,1))


extrap_quantiles=seq(min(tmax2)-20,max(tmax2)+20,1)#Interval used for model1 =0.75
#Interval used for model 3 = 0.2
extrap_prob=pgev(extrap_quantiles,xi=xi, mu=mu, beta=beta)
lines(extrap_quantiles,1-extrap_prob,lwd=4,col="red")
t = (1/(1-extrap_prob))
plot(1/(1-extrap_prob)[15:47],qgev(extrap_prob,xi=xi,mu=mu, beta=beta)[15:47])


