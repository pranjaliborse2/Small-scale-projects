##########################################
###  Bayes Inference Example
###    - Written by Ryan Sriver
###    - ATMS526, Spring 2022
##########################################
###  Application of Bayes' Law 
###    - Coin Flip Example
###  Objectives:
###    - Consruct a simple Bayesian inference model
###    - Use it to make a probabilistic assessment about
###      the fairness of a hypothetical coin
###    - Highlight the effect of prior informtion 
###    - Quantify uncertainty using Credible Intervals
##########################################


#load functions
  source("./bayesian_functions.R")


#############################
#  Coin Flip Simulation
#############################

#Consider a coin with two outcomes: H-1 or T-0
#Define outcomes
  h<-c(0,1)

# Specify number of tosses
  n= 10

# Simulate flipping the coin n times
# and calculate the number of Heads shown
  #r=sum(sample(h,size=n,replace=TRUE))
  #Q.4 
  r=as.integer(0.7*n)



# Define x: 
#  Specifies bins for constructing posterior
#  Represents chance of tossing heads on any given flip 
#  Range of  x is [0:1]

  dx=0.001 # quantile spacing
  x<-seq(0,1,dx)




#Construct the prior (call new prior function)
  prior =fprior(x=x, mu=0.5, sigma=0.3)

#Construct the likelihood (call new likelihood function)
  likelihood=flikelihood(x=x,n=n,r=r)

#Construct the posterior
  posterior=prior*likelihood

#Normalize the pdfs (integrate to unity)
  posterior=posterior/sum(posterior)
  likelihood=likelihood/sum(likelihood)
  prior=prior/sum(prior)
  x1 = x[which(posterior == max(posterior))]
  print(x1)


#Plot the normalized pdfs on the same figure
  pdf("Posterior(test).pdf", width = 4.5, height = 7)
  par(mfrow = c(1, 1), cex = 0.4)
  plot(x,posterior, type="l",lwd=8,col="black",
    ylab="Normalized Density")
    lines(x,likelihood,lwd=4, col="blue")
    lines(x,prior,lwd=4, col="red")
   

#Create Legend
  legend(x="topleft",
       legend=c("Prior", "Likelihood", "Posterior"),
       col=c("red","blue","black"),
       lty=c("solid","solid","solid"),
       lwd=c(5,5,5)
        )
  


#######################################
#####  Quantify credible intervals
#######################################
#
# To calculate credible intervals, we 
#  need to know where our quantiles occur
#  in the posterior distribution
#
# Problem:  Posterior is an empirical pdf.
#  - We can't use ecdf because it requires the
#    original data/observations (not likelihoods)
#  - We can integrate the posterior to get the cdf
#    and invert (using quantile function) to relate
#    posterior/quantiles.
#
########################################


# One simple (verbose) method:
# Define a vector for probabilities (same size as x)
# Integrate area under posterior pdf curve
   cdf=array(x)
  for (i in 1:length(x)) { 
      cdf[i]=sum((posterior[1:i])/sum(posterior))
    }

# Use spline interpolation to create an empirical quantile 
# function relating the cdf and quantiles
  quant_fit=splinefun(cdf,x,method="natural")


#The quantile function returns the location of a specified 
#  quantile (x) in the posterior distribtion (cdf)

#Example: Highlight the 95% Credible Interval
  print(quant_fit(0.025))
  print(quant_fit(0.975))


# Add 95% range to plot
  abline(v=quant_fit(0.025),lty="dashed")
  abline(v=quant_fit(0.975),lty="dashed")
  dev.off()

