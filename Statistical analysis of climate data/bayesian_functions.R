#############################
# Some Bayesian Functions
#############################

# likelihood function (binomial)
#  x - vector of quantiles
#  r - number of heads shown
#  n - total number of tosses

flikelihood = function(x, r, n)
  {
    ret = choose(n,r)*x^r*(1-x)^(n-r)
  }

#############################

# prior function (Gaussian/Normal)
#  x     - vector of quantiles
#  mu    - expected value
#  sigma - standard deviation

fprior = function(x, mu, sigma)
   {
     ret = 1/(sigma*sqrt(2*pi))*exp(-1/(2*sigma^2)*(x-mu)^2)
   }


#############################

