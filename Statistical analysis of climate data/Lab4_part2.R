rm(list = ls())
graphics.off()

# Constants from van Dantzig (1956)
p_0 = 0.0038      # unitless; probability of flooding in 1 yr if the dikes
# aren't built
alpha = 2.6       # unitless; constant associated with flooding probabilities
V = 10^ 10        # guilders; value of goods threatened by flooding
delta = 0.04      # unitless; discount rate
I_0 = 0           # guilders; initial cost of building dikes
k = 42* 10^ 6     # guilders/m; cost of raising the dikes by 1 m

# Set some other values.  
n.trials <- 10^5  # number of Monte Carlo trials to do
range <- 0.1      # fractional range of each parameter to test
probs <- c(0.025, 0.5, 0.975)
# which quantiles to report

# Set the seed for random sampling.  
set.seed(1)

# Perform the random sampling.  
facs <- c((1- 0.5* range), (1+ 0.5* range))
delta.vals <- runif(n.trials, min = facs[1]* delta, 
                    max = facs[2]* delta)
V.vals <- runif(n.trials, min = facs[1]* V, 
                    max = facs[2]* V)
k.vals <- runif(n.trials, min = facs[1]* k, 
                max = facs[2]* k)

# Calculate the optimal dike heights.  
best.heights <- alpha^-1* log((V.vals* p_0* alpha)/ (delta.vals* k.vals))

# Make a histogram and print the quantiles to the screen.
pdf("Hist with delta,V,k change.pdf")
hist(best.heights, main = 'With changing delta,v,k', xlab = 'Optimal dike heights (m) ')
abline(v = alpha^-1* log((V* p_0* alpha)/ (delta* k)), lwd = 2, col = 'red')
dev.off()
print(round(quantile(best.heights, probs = probs), 3))
