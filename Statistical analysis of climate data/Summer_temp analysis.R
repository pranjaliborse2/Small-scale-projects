rm(list=ls())
temp <- read.csv("D:\\UIUC Academics\\SEM2 UIUC\\Risk analysis\\Project\\NYC Daily temp. data.csv")
tsummer<-list()
for(i in seq(1,18628-365,365)){
  tsummer<-c(tsummer,temp[(i+150):(i+150+120), 6])
}
tsummer = unlist(tsummer)
library("fExtremes")
blockmax = blockMaxima(tsummer,block = 120)
#tempanom = blockmax - 90
blockmax = unname(blockmax)
tmean <-list()
freq <-vector()
for(i in seq(1,length(tsummer)-120,120)){
  tm = mean(as.numeric(tsummer)[i:(i+120)])
  tmean<- c(tmean,tm)
  freq<- c(freq,length(unlist(list(tsummer[i:(i+120)][tsummer[i:(i+120)]>90]))))
}

tempanom = unlist(blockmax)-90
heatwave = tempanom[tempanom>0]

plot(seq(2000,2020),freq[31:51], type="l",xlab= "Year",ylab="summer extreme heat days")

#abline(lm(as.numeric(freq[1:31])~seq(1970,2000)),col="red")


plot(density(unlist(tsummer)),main="Distribution of NYC summer temperature",col="Blue",lwd=2, xlab="temp.")
library("ggpubr")

ggqqplot(unlist(tsummer))


co= read.csv("D:\\UIUC Academics\\SEM2 UIUC\\Risk analysis\\Project\\SeriesExport-03-30-2022-20-11-15.csv")
co1 <- as.numeric(rev(co[8:length(co[,11]),11]))
t<- seq(1970,2020)

pdf("freq and temp.pdf", width=6.5, height=4.5)
plot(t,tmean,type="l",col="red",xlab="time(yr)",ylab="Mean summer temp. and frequency of extreme heat days")
par(new=TRUE)
plot(t,freq,type="l",col="blue",yaxt="n",xlab="time(yr)",ylab="Mean summer temp. and frequency of extreme heat days")
legend("bottomleft",legend=c("Temp.", "frequency"), col=c("red","blue"),lwd=1.5)
dev.off()
#plot(df1$seatemp, df1$blockmax.7.51.,type="l")
cor.test(freq,as.numeric(tmean))


co.rmse <- function(params, time, co.levels) { # don't forget the curly braces
  # Step 1: Pick apart the vector params into its individual values.
  a <- params[1]
  b <- params[2]
  c <- params[3]
  t.0 <- params[4]
  coest <- a*((time-t.0)^2)+b*(time-t.0)+c
  rmse= sqrt(mean((co.levels-coest)^2))
  return(rmse)
}


start <- c(0, 0, -10, 1980)
optim.fit <- optim(start, co.rmse, gr = NULL, time = t[11:49], co.levels = co1)
# Extract the parameters of the best-fit polynomial, and the root mean squared
# error, from optim_fit.
best.a <- optim.fit$par[1]
best.b <- optim.fit$par[2]
best.c <- optim.fit$par[3]
best.t.0 <- optim.fit$par[4]
best.rmse <- optim.fit$value

coopt = best.a*((t[11:49]-best.t.0)^2)+best.b*(t[11:49]-best.t.0)+best.c
plot(t[11:49], co1, type = "l", xlab = "Time (yr)", ylab = "CO2 emissions (MMT)", lwd=1.5)
lines(t[11:49],coopt, col="red")
#plot(t,tavg[11:49],type="l")
res <- co1-coopt

pdf("emissions and temp.pdf", height=6)
plot(t[11:49], res,type="l",yaxt="n", ylab = "Co2 emissions residuals and mean temp.", xlab="Time(yr)")
par(new='TRUE')
plot(t[11:49],tmean[11:49],type="l",col="green",lwd=2, ylab = "Co2 emissions residuals and mean temp.", xlab="Time(yr)")
legend("topright",legend = c("Mean temp. (C)","GHG emission residuals"), col = c("green","black"),lwd=1)
dev.off()

pdf("Mean and max temp.pdf", height=6)
plot(t,tmean,type="l",col="green",lwd=2, ylab = "Mean temp. and max. temp.", xlab="Time(yr)",ylim=c(70,120))
par(new=TRUE)
plot(t,blockmax[1:51], type="l", col="red",lwd=2, ylab = "Mean temp. and max. temp.", xlab="Time(yr)",ylim=c(70,120))
legend("topright",legend = c("Mean temp. (C)","Max. temp."), col = c("green","red"),lwd=1)
dev.off()
cor.test(as.numeric(tmean),blockmax[1:51])




pdf("blockmax.pdf",width = 6.5,height=4.5)
plot(t,blockmax[1:51],type="l",col="red",lwd=2,xlab="time(yr)",ylab="Max. 3 day temperature")
abline(lm(blockmax[1:26]~t[1:26]),col="black")
abline(lm(blockmax[27:51]~t[27:51]),col="blue")
legend("bottomleft",legend = c("3-day max. temp.","1970-1995 linear model","1996-2020 linear model"), col = c("red","black","blue"),lwd=1)
dev.off()

resids1 = unname(summary(lm(blockmax[1:26]~t[1:26]))$residuals)
resids2 = unname(summary(lm(blockmax[27:51]~t[27:51]))$residuals)
plot(t[1:26],resids1,type="l",ylim=c(-10,10),xlim=c(1970,2020),col="blue",lwd=1.5,xlab="Time(Yr)",ylab="Residuals")
par(new=TRUE)
plot(t[27:51],resids2,type="l",ylim=c(-10,10),xlim=c(1970,2020),col="red",lwd=1.5,xlab="Time(Yr)",ylab="Residuals")

incept = summary(lm(blockmax[27:51]~t[27:51]))$coefficients[1]
slope = summary(lm(blockmax[27:51]~t[27:51]))$coefficients[2]
blockfut = sample(resids2)+slope*seq(2020,2044)+incept
blockfut2 = sample(resids2)+slope*seq(2045,2069)+incept

plot(seq(2020,2044),blockfut,type="l", xlim=c(1970,2070), ylim = c(90,120),xlab="time(yr)",ylab="Max. 3 day temperature",lwd=2)
par(new=TRUE)
plot(t,blockmax[1:51],type="l",col="red",lwd=2,xlab="time(yr)",ylab="Max. 3 day temperature",xlim=c(1970,2070), ylim = c(90,120))
par(new=TRUE)
plot(seq(2045,2069),blockfut2,type="l", xlim=c(1970,2070), ylim = c(90,120),xlab="time(yr)",ylab="Max. 3 day temperature",lwd=2)



#####GEV fitting ######################
prob = (1:length(blockmax))/(length(blockmax)+1)

# Use gevFit to fit GEV function to data
gev_est=gevFit(blockmax,type="mle")

# Return the estimated GEV parameters
xi=gev_est@fit$par.ests[1]
mu=gev_est@fit$par.ests[2]
beta=gev_est@fit$par.ests[3]
quant=qgev(prob,xi = xi, mu = mu, beta = beta)

cdf_data=ecdf(blockmax)
# Use CDF to convert data quantiles to probabilities
p_data=cdf_data(blockmax)
plot(blockmax,1-p_data,pch=19, log="y",main="Summer temperature Risk Curve, 
  New York",ylab="log(1-CDF)", xlab="Temperature extreme (C)",xlim=c(min(blockmax),max(blockmax+30)) , ylim=c(0.01,1))


extrap_quantiles=seq(min(blockmax),max(blockmax)+20,1)#Interval used for model1 =0.75
#Interval used for model 3 = 0.2
extrap_prob=pgev(extrap_quantiles,xi=xi, mu=mu, beta=beta)
lines(extrap_quantiles,1-extrap_prob,lwd=4,col="red")

#plot(1/(1-extrap_prob)[1:45],qgev(extrap_prob,xi=xi,mu=mu, beta=beta)[1:45],xlab="Return period",ylab="3days Return level", type="l",lwd=2,col="blue")

qgev(0.99,xi=xi,mu=mu, beta=beta)

# Define number of bootstrap samples
N=100

# Define arrays of parameter values
xi_array=rep(NA,N)
mu_array=rep(NA,N)
beta_array=rep(NA,N)

#print(temp_max)



# Create bootstrap samples
for(i in 1:N) {
  dummy_data=sample(blockmax,size=52,replace=TRUE)
  dummy_fit=gevFit(dummy_data,type="mle")
  
  
  xi_array[i]=dummy_fit@fit$par.ests[1]
  mu_array[i]=dummy_fit@fit$par.ests[2]
  beta_array[i]=dummy_fit@fit$par.ests[3]
}



# Determine the 90% credible interval
#   Find the 0.05 and 0.95 quantiles of parameter distributions
xi_q=quantile(xi_array,probs=c(0.05,.95))
#print(xi_q)
mu_q=quantile(mu_array,probs=c(0.05,.95))
#print(mu_q)
beta_q=quantile(beta_array,probs=c(0.05,.95))
#print(beta_q)


### Hold the shape parameter (tail behavior) fixed at best estimate ###
ci_low=pgev(extrap_quantiles,xi=xi, mu=mu_q[1], beta=beta_q[1])
ci_hi=pgev(extrap_quantiles,xi=xi, mu=mu_q[2], beta=beta_q[2])
print(qgev(0.98,xi=xi,mu=mu_q[1], beta=beta_q[1]))#50 year return level
print(qgev(0.98,xi=xi,mu=mu_q[2], beta=beta_q[2]))
print(qgev(0.99,xi=xi,mu=mu_q[1], beta=beta_q[1]))#50 year return level
print(qgev(0.99,xi=xi,mu=mu_q[2], beta=beta_q[2]))


pdf("risk curve with confidence intervals.pdf", width=6, height=4.5)
plot(blockmax,1-p_data,pch=19, log="y",main="Summer temperature Risk Curve, 
  New York",ylab="log(1-CDF)", xlab="Temperature extreme (C)",xlim=c(min(blockmax),max(blockmax+30)) , ylim=c(0.01,1))
lines(extrap_quantiles,1-extrap_prob,lwd=4,col="red")


#  Add curves for the 90% interval

lines(extrap_quantiles,1-ci_low,lwd=2,col="blue")
lines(extrap_quantiles,1-ci_hi,lwd=2,col="blue")


legend(x="topright",
       inset=c(0.1,0.1),
       legend=c("Data","GEV Fit","90 % CI"),
       col=c("black","red","blue"),
       lty=c("solid","solid","solid"),
       lwd=c(3))
dev.off()


