rm(list = ls())
graphics.off()
require(triangle)

n.obs <- 10^ 4 # number of random values to generate
n.bins <- 25 # number of bins in the histogram
set.seed(1)
u <- runif(n.obs,min=0,max=1)
log<- rlnorm(n.obs,meanlog=0,sdlog=1)
triangle<- rtriangle(n.obs,0,1,(0+1)/2)
exp = rexp(n.obs,rate=1)
dir.create("figures1")
pdf("figures1/exponential.pdf")
par(mfrow = c(2, 1))
# Plot the histogram.
hist(exp, breaks = n.bins, freq = FALSE, xaxs = "i", main = "exponential",
     xlab = "x", ylab = "f(x)")
# Show the mean and the median on the histogram.
abline(v = mean(exp), lty = 1, lwd = 2, col = "red")
abline(v = median(exp), lty = 2, lwd = 2, col = "blue")
# Put a legend on the histogram.
legend("topright", legend = c("mean", "median"),
       lty = c(1, 2), lwd = 2, bty = "n", col = c("red", "blue"))
# Find the extents of the histogram's axes.
axis.lims <- par("usr")
dev.off()