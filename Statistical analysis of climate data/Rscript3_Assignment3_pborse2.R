rm(list = ls())
graphics.off()
mu <- c(5,50) # mean of normal distribution
sigma <- c(3,20) # standard deviation of normal distribution
n.obs <- c(10^ 4,1000) # number of random values to generate
n.bins <- c(25,10) # number of bins in the histogram
set.seed(1)
data <- rnorm(n = n.obs[1], mean = mu[1], sd = sigma[1])
data1 <- rnorm(n = n.obs[1], mean = mu[1], sd = sigma[1])
mean.data <- mean(data)
median.data <- median(data)
dir.create("figures")
pdf("figures/lab3_sample_plot4.pdf")
par(mfrow = c(2, 1))
# Plot the histogram.
hist(data, breaks = n.bins[1], freq = FALSE, xaxs = "i", main = "Histogram",
     xlab = "x", ylab = "f(x)")
# Show the mean and the median on the histogram.
abline(v = mean.data, lty = 1, lwd = 2, col = "red")
abline(v = median.data, lty = 2, lwd = 2, col = "blue")
# Put a legend on the histogram.
legend("topright", legend = c("mean", "median", "0.025 and 0.975"),
       lty = c(1, 2), lwd = 2, bty = "n", col = c("red", "blue"))
# Find the extents of the histogram's axes.
axis.lims <- par("usr")

hist(data1, breaks = n.bins[2], freq = FALSE, xaxs = "i", main = "Histogram-bins",
     xlab = "x", ylab = "f(x)")
# Show the mean and the median on the histogram.
abline(v = mean(data1), lty = 1, lwd = 2, col = "red")
abline(v = median(data1), lty = 2, lwd = 2, col = "blue")
# Put a legend on the histogram.
legend("topright", legend = c("mean", "median", "0.025 and 0.975"),
       lty = c(1, 2), lwd = 2, bty = "n", col = c("red", "blue"))
# Find the extents of the histogram's axes.
axis.lims <- par("usr")
dev.off()