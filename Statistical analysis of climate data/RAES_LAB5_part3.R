source("RAES_LAB5_part2.R")
start <- c(0, 0, -100, 1800)
optim.fit <- optim(start, sl.rmse, gr = NULL, time = t, sea.levels = obs.sl)
# Extract the parameters of the best-fit polynomial, and the root mean squared
# error, from optim_fit.
best.a <- optim.fit$par[1]
best.b <- optim.fit$par[2]
best.c <- optim.fit$par[3]
best.t.0 <- optim.fit$par[4]
best.rmse <- optim.fit$value

slopt = best.a*((t-best.t.0)^2)+best.b*(t-best.t.0)+best.c
resids <- obs.sl- slopt
pdf("optfig.pdf")
par(mfrow = c(2,1))
plot(t, obs.sl, type = "l", xlab = "Time (yr)", ylab = "Sea level anomaly (mm)",col="red")
lines(t,slopt,col="blue")
legend(x = 'topleft', legend = c('Data', '2nd-Order Polynomial'), lwd = 1, 
       col = c('red', 'blue'), bty = 'n')
plot(t, resids, type = 'l', xlab = 'Time (yr)', ylab = 'Residuals (mm)', 
     lwd = 1)

dev.off()

