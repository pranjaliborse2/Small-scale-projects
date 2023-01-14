#Assignment 1 - Plotting histogram
f1 = pdf("hist_pborse2.pdf")
x <- rnorm(n=10^4, mean=5, sd=2)
hist(x,main = "Pranjali",xlab="Variable",ylab="Frequency(n=10^4)")
abline(v=mean(x),col="red",lwd=2)
abline(v=mean(x)+sd(x),col="blue",lwd=2)
abline(v=mean(x)-sd(x),col="blue",lwd=2)
dev.off()
