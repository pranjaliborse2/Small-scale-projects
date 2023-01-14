########################################
# Analyze Daily Precipitation Anomalies
#  - Histograms and QQ plots
#  - Full Distribution and Block Maxima
########################################
# Written by: Ryan Sriver (rsriver@illinois.edu)
# ATMS 526
########################################

### Load Necessary Libraries
### May need to install packages
library(fExtremes)
library(MASS)   # Modern Applied Statistics Package

precip_table = read.table("./precip_data.txt")
precip_full = as.vector(precip_table[,1])


pmin=0
pmax=150

#file=pdf("precip_qq.pdf")
par(mfrow=c(2, 2))  # 2x2 panel figure



#################################
### Analyze Daily Summer Precipitation
#################################


# Subset days with non-zero precipitation
  precip=precip_full[precip_full>0]


### Calculate some basic stats
# Mean daily precipitation over all days
#  print(mean(precip_full))

# fraction of days with rain
#  print(length(precip)/length(precip_full))

# Maximum daily precip
#  print(max(precip))



###########################
# Analyze Full Distribution
###########################


hist(precip,col="gray",breaks=seq(pmin,pmax,5),prob=TRUE,
  main="Daily Summer Precipitation",xlab="Observations",ylab="Probability")
  lines(density(precip,bw=1),col="red",lwd=3)


legend(x="topright",
       inset=c(0.,0.),
       legend=c("Density"),
       col=c("red"),
       lty=c("solid"),
       lwd=c(3)
       )


# QQ plot: Check data against gamma distribution

# Define vector of probabilities based on length of data
 prob_precip = (1:length(precip))/(length(precip)+1)

# Fit a gamma distribution to estimate theoretical parameters
 p_gamma=fitdistr(precip, "gamma")
 fit.coef <- coef(p_gamma)
# print(fit.coef)

  precip_quant=qgamma(prob_precip,shape=fit.coef["shape"], scale=1/fit.coef["rate"])

 plot(sort(precip), precip_quant,main="QQ Gamma",xlab="Observations (mm)",
    ylab="Theoretical (mm)",xlim=c(pmin,pmax),ylim=c(pmin,pmax))
  abline(0,1)
  abline(v=50,col="blue",lwd=3)

# Percentage of data less than 50 mm
#  print(length(precip[precip<50])/length(precip))



###########################
# Analyze Block Maxima
###########################

# Calculate Block Maxima
block_max=blockMaxima(precip,block=90)

# Plot histogram
hist(block_max,prob=TRUE,col="gray",breaks=seq(pmin,pmax,5),
     main="Summer Block Max Daily Precip (1961-2010)",
     xlab="Observed Daily Precip (mm)",ylab="Probability",
     xlim=c(0,pmax))

# Add empirical density
lines(density(block_max),col="red",lwd=3)


# QQ plot: Does GEV model fit the data?

# Define vector of probabilities based on length of data
prob = (1:length(block_max))/(length(block_max)+1)

# Use gevFit to fit GEV function to data
gev_est=gevFit(precip,block=90,type="pwm")

# Return the estimated GEV parameters
xi=gev_est@fit$par.ests[1]
mu=gev_est@fit$par.ests[2]
beta=gev_est@fit$par.ests[3]

# Calculate theoretical quantiles based on estimated model parameters
quant=qgev(prob,xi = xi, mu = mu, beta = beta)


# compare theoretical quantiles with data
  plot(quant,sort(block_max),main="QQ Estimate",xlab="Theoretical GEV Quantiles",
  ylab="Observations (block max)",xlim=c(pmin,pmax),ylim=c(pmin,pmax))


# Add  1:1 line to plot
  abline(0,1)

#dev.off()

stop()

#####################################
# Analyze Risk of Extreme Event
#####################################
file1= pdf("Risk curve_mod3Ppt.pdf")
par(mfrow=c(1,1))  

### Plot Survival Function (Risk Curve) ###

# Calculate Empirical CDF from Data
cdf_data=ecdf(block_max)

# Use CDF to convert data quantiles to probabilities
p_data=cdf_data(block_max)

# Plot the Risk Curve of the Data
 plot(block_max,1-p_data,pch=19,log="y",main="mod.3Precip Risk Curve, Central Illinois",ylab="log(1-CDF)",
  xlab="Daily Summer Precip (mm)",xlim=c(0,pmax+50),ylim=c(0.01,1))


# Add the cdf from the theoretical fit (extrapolate beyond data)
 extrap_quantiles=seq(0,pmax+50,1)
 extrap_prob=pgev(extrap_quantiles,xi=xi, mu=mu, beta=beta)
 lines(extrap_quantiles,1-extrap_prob,lwd=4,col="red")


#print(xi)
#print(mu)
#print(beta)


# What is the return period of the 10 degree temperature event?
#     Define return period as:  T=1/(1-p)
 print(qgev(0.98,xi=xi,mu=mu, beta=beta))#50 year return level
 print(qgev(0.99,xi=xi,mu=mu, beta=beta))#100 year return level
  t=1/(1-pgev(150,xi=xi,mu=mu,beta=beta))#return period of 150 mm
  print(t)



########  What about uncertainty?  #################
# There is uncertainty in the parameter estimates
# How do we represent in temp risk estimates?
# One method:  The bootstrap
#  - resample the block maxima (create synthetic data sets)
#  - create new fits to the samples
#  - can estimate confidence intervals from parameter pdfs 
####################################################


# Define number of bootstrap samples
N=200

# Define arrays of parameter values
xi_array=rep(NA,N)   # shape
mu_array=rep(NA,N)   # scale
beta_array=rep(NA,N) # shape

#print(temp_max)



# Create bootstrap samples
for(i in 1:N) {
  dummy_data=sample(block_max,size=50,replace=TRUE)
  na.exclude(dummy_data)
  dummy_fit=gevFit(dummy_data,type="mle")


  xi_array[i]=dummy_fit@fit$par.ests[1]
  mu_array[i]=dummy_fit@fit$par.ests[2]
  beta_array[i]=dummy_fit@fit$par.ests[3]
}



# Determine the 90% credible interval
#   Find the 0.05 and 0.95 quantiles of parameter distributions
xi_q=quantile(xi_array,probs=c(0.05,.95),na.rm=TRUE)
#  print(xi_q)
mu_q=quantile(mu_array,probs=c(0.05,.95),na.rm=TRUE)
#  print(mu_q)
beta_q=quantile(beta_array,probs=c(0.05,.95),na.rm=TRUE)
#  print(beta_q)


### Hold the shape parameter (tail behavior) fixed at best estimate ###
ci_low=pgev(extrap_quantiles,xi=xi, mu=mu_q[1], beta=beta_q[1])
ci_hi=pgev(extrap_quantiles,xi=xi, mu=mu_q[2], beta=beta_q[2])
print(qgev(0.98,xi=xi,mu=mu_q[1], beta=beta_q[1]))#50 year return level
print(qgev(0.98,xi=xi,mu=mu_q[2], beta=beta_q[2]))
print(qgev(0.99,xi=xi,mu=mu_q[1], beta=beta_q[1]))#50 year return level
print(qgev(0.99,xi=xi,mu=mu_q[2], beta=beta_q[2]))
#  Add curves for the 90% interval
lines(extrap_quantiles,1-ci_low,lwd=2,col="blue")
lines(extrap_quantiles,1-ci_hi,lwd=2,col="blue")


legend(x="topright",
       inset=c(0.1,0.1),
       legend=c("Data","GEV Fit","90 % CI"),
       col=c("black","red","blue"),
       lty=c("solid","solid","solid"),
       lwd=c(3)
       )


dev.off()




