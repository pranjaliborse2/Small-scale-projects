########################################
# Analyze Daily Temperature Anomalies
#  - Histograms and QQ plots
#  - Full Distribution and Block Maxima
########################################
# Written by: Ryan Sriver (rsriver@illinois.edu)
# ATMS 526
########################################

### Load Necessary Libraries
### May need to install packages
library(fExtremes)


# Read in Temperature Data
temp_table = read.table("./temp_data.txt")
  temp = as.vector(temp_table[,4])


tmin=-16
tmax=16

#file=pdf("temp_qq.pdf")
par(mfrow=c(2, 2))  # 2x2 panel figure



print(min(temp))
print(max(temp))


###########################
# Analyze Full Distribution
###########################


# Plot histogram
  hist(temp,col="gray",breaks=seq(tmin,tmax,1),prob=TRUE,
    main="Daily Summer Temperature (1961-2010)",
    xlab="Observed Temp Anomalies (Celsius)",ylab="Probability")

# Add empirical density
  lines(density(temp,bw=1),col="red",lwd=3)

legend(x="topright",
       inset=c(0.,0.),
       legend=c("Density"),
       col=c("red"),
       lty=c("solid"),
       lwd=c(3)
       )

# QQ plot: Is the temperature data normally distributed?

# Define vector of probabilities based on length of data
  prob_temp = (1:length(temp))/(length(temp)+1)

# Calculate theoretical quantiles
  temp_quant=qnorm(prob_temp,mean=mean(temp),sd=sd(temp))

# compare theoretical quantiles with data
  plot(sort(temp), temp_quant,main="QQ Normal",xlab="Observations (Celsius)",
    ylab="Theoretical (Celsius)",xlim=c(tmin,tmax),ylim=c(tmin,tmax))
  abline(0,1)
  abline(v=5,col="blue",lwd=3)

# Fraction of data less than 5 C
 # print(length(temp[temp<5])/length(temp))




###########################
# Analyze Block Maxima
###########################

# Calculate Block Maxima
block_max=blockMaxima(temp,block=90)

# Plot histogram
hist(block_max,prob=TRUE,col="gray",
     main="Summer Block Max Temp (1961-2010)",
     xlab="Observed Temp Anomalies (Celsius)",ylab="Probability",
     xlim=c(tmin,tmax))

# Add empirical density
lines(density(block_max),col="red",lwd=3)



# QQ plot: Does GEV model fit the data?

# Define vector of probabilities based on length of data
prob = (1:length(block_max))/(length(block_max)+1)

# Use gevFit to fit GEV function to data
gev_est=gevFit(temp,block=90,type="pwm")

# Return the estimated GEV parameters
xi=gev_est@fit$par.ests[1]
mu=gev_est@fit$par.ests[2]
beta=gev_est@fit$par.ests[3]

# Calculate theoretical quantiles based on estimated model parameters
quant=qgev(prob,xi = xi, mu = mu, beta = beta)


# compare theoretical quantiles with data
  plot(quant,sort(block_max),main="QQ Estimate",xlab="Theoretical GEV Quantiles",
  ylab="Observations (block max)",xlim=c(tmin,tmax),ylim=c(tmin,tmax))


# Add  1:1 line to plot
  abline(0,1)


#dev.off()


#stop()


#####################################
# Analyze Risk of Extreme Event
#####################################

 
file1 = pdf("Risk curve_mod1Temp.pdf")
par(mfrow=c(1,1)) 
### Plot Survival Function (Risk Curve) ###

# Calculate Empirical CDF from Data
cdf_data=ecdf(block_max)

# Use CDF to convert data quantiles to probabilities
p_data=cdf_data(block_max)

# Plot the Risk Curve of the Data
 plot(block_max,1-p_data,pch=19,log="y",main="Mod.1 Temperature Risk Curve, 
  Central Illinois",ylab="log(1-CDF)", xlab="Temperature Anomalies (C)",
  xlim=c(0,tmax+2),ylim=c(0.01,1))


# Add the cdf from the theoretical fit (extrapolate beyond data)
 extrap_quantiles=seq(0,tmax+2,0.2)#Interval used for model1 =0.75
 #Interval used for model 3 = 0.2
 extrap_prob=pgev(extrap_quantiles,xi=xi, mu=mu, beta=beta)
 lines(extrap_quantiles,1-extrap_prob,lwd=4,col="red")
 #print(1/(1-extrap_prob))


#print(xi)
#print(mu)
#print(beta)


# What is the return period of the 10 degree temperature event?
#     Define return period as:  T=1/(1-p)


print(qgev(0.98,xi=xi,mu=mu, beta=beta))#50 yr return level
print(qgev(0.99,xi=xi,mu=mu, beta=beta))#100 yr return level 

p=pgev(10,xi=xi,mu=mu,beta=beta)
t=1/(1-p)

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
N=100

# Define arrays of parameter values
xi_array=rep(NA,N)
mu_array=rep(NA,N)
beta_array=rep(NA,N)

#print(temp_max)



# Create bootstrap samples
for(i in 1:N) {
  dummy_data=sample(block_max,size=50,replace=TRUE)
  dummy_fit=gevFit(dummy_data,type="mle")


  xi_array[i]=dummy_fit@fit$par.ests[1]
  mu_array[i]=dummy_fit@fit$par.ests[2]
  beta_array[i]=dummy_fit@fit$par.ests[3]
}



# Determine the 90% credible interval
#   Find the 0.05 and 0.95 quantiles of parameter distributions
xi_q=quantile(xi_array,probs=c(0.05,.95))
  print(xi_q)
mu_q=quantile(mu_array,probs=c(0.05,.95))
  print(mu_q)
beta_q=quantile(beta_array,probs=c(0.05,.95))
  print(beta_q)


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

stop()

par(mfrow=c(2, 2))  # 2x2 panel figure
hist(xi_array,main="shape")
hist(mu_array,main="location")
hist(beta_array,main="scale")


