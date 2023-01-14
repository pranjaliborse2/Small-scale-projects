rm(list=ls())

sl.data <- read.table("data/gslGRL2008.txt", skip = 14, header = FALSE)
t <- sl.data[, 1]
obs.sl <- sl.data[, 2]


sl1.rmse <- function(params, time, sea.levels) { # don't forget the curly braces
  # Step 1: Pick apart the vector params into its individual values.
  a <- params[1]
  b <- params[2]
  c <- params[3]
  t.0 <- params[4]
  sl1 <- a*((time-t.0)^2)+b*(time-t.0)+c
  rmse= sqrt(mean((sea.levels-sl1)^2))
  return(rmse)
}

##########BEST FIT##############
start <- c(0, 0, -100, 1800)
optim.fit <- optim(start, sl1.rmse, gr = NULL, time = t, sea.levels = obs.sl)
# Extract the parameters of the best-fit polynomial, and the root mean squared
# error, from optim_fit.
best.a <- optim.fit$par[1]
best.b <- optim.fit$par[2]
best.c <- optim.fit$par[3]
best.t.0 <- optim.fit$par[4]
best.rmse <- optim.fit$value

slopt = best.a*((t-best.t.0)^2)+best.b*(t-best.t.0)+best.c
resids <- obs.sl- slopt




##Bootstrapping
proj.t <- seq(1700, 2100)
boot.start <- c(best.a, best.b, best.c, best.t.0)
n.boot <- 10^3
boot.a <- rep(NA,length=n.boot)
boot.b<- rep(NA,length=n.boot)
boot.c<- rep(NA,length=n.boot)
boot.t.0 <- rep(NA,length=n.boot)

proj.sl <-matrix(NA,nrow=length(proj.t),ncol=n.boot)
resid.best<-matrix(NA,nrow=length(t),ncol=n.boot)
for(i in 1:n.boot){
  boot.resids <- sample(resids, length(resids), replace = TRUE)
  boot.sl <- slopt+ boot.resids
  
  optim.fit <- optim(boot.start, sl1.rmse, gr = NULL, time = t, sea.levels = boot.sl)
# Extract the parameters of the best-fit polynomial, and the root mean squared
# error, from optim_fit.
  boot.a[i] <- optim.fit$par[1]
  boot.b[i] <- optim.fit$par[2]
  boot.c[i] <- optim.fit$par[3]
  boot.t.0[i] <- optim.fit$par[4]
  best.rmse <- optim.fit$value
  proj.sl[,i] = (boot.a[i]*((proj.t-boot.t.0[i])^2))+(boot.b[i]*(proj.t-boot.t.0[i]))+boot.c[i]
  #resid.best[i] <- boot.sl- sl.best[i]
}
sloptfut =best.a*((proj.t-best.t.0)^2)+best.b*(proj.t-best.t.0)+best.c
sloptfut[401]
#########PART 1(2)
pdf("Lab7_part1(2).pdf")
#par(mfrow = c(2, 1))

matplot(proj.t, proj.sl, lwd = 1, col = 'grey',pch=19, type="l",ylim=c(-300,500),xlab = 'Time (yr)',ylab = 'Sea level anomaly (mm)')
par(new=TRUE)
plot(t, obs.sl, type = 'l', xlab = 'Time (yr)', ylab = 'Sea level anomaly (mm)', 
     lwd = 1, col = 'black',ylim= c(-300,500),xlim = c(1700,2100))
lines(proj.t,sloptfut,col="red",ylim=c(-300,500))
legend(x = 'topleft', legend = c('Data', '2nd-Order Polynomial',"Bootstrapping"), lwd = 1, 
       col = c('black','red','grey'), bty = 'n')
#plot(t, resids, type = 'l', xlab = 'Time (yr)', ylab = 'Residuals (mm)', 
 #    lwd = 1)
dev.off()
#######PART 1(3)
pdf("Lab7_part1(3).pdf")
par(mfrow=c(2,2))
hist(boot.a,xlab= "a (mm/yr^2)")
abline(v=best.a,col="red",lwd=2)
hist(boot.b,xlab= 'b (mm/yr)')
abline(v=best.b,col="red",lwd=2)
hist(boot.c,xlab= 'c (mm)')
abline(v=best.c,col="red",lwd=2)
hist(boot.t.0,xlab= 'Time(year)')
abline(v=best.t.0,col="red",lwd=2)
dev.off()


###Part 1(4)
pdf("Lab7_part1(4).pdf")
hist(proj.sl[401,], xlab="Estimate in year 2100 (mm)")
abline(v=sloptfut[401],col="red",lwd=2)
dev.off()

#####PART 2
residboot = sample(resids,length(resids),replace = TRUE)
pdf("Lab7_part2.pdf")
par(mfrow=c(2,1))
acf(resids,plot=TRUE,xlab="Timelagged resids")
acf(residboot, plot=TRUE,xlab="Timelagged sampled resids")
dev.off()

###Discussion question 2
pdf("Lab7_disque.pdf")
par(mfrow=c(2,1))
plot(t, resids, type="l",ylab="Resids")
abline(lm(resids~t),col="blue",lwd=2)

plot(t, residboot, type="l",ylab="Sampled resids")
abline(lm(residboot~t),col="blue",lwd=2)
dev.off()