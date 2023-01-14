##################
# t-test
##################
#
# Test the hypothesis that the means 
#  of two different distributions
#  are equal
#
# Null hypothesis: true difference in means 
#  is equal to zero
#
##################


# Read in Feb temp data from text file
  temp_table = read.table("./temp_il_jan-feb_1880-2016.txt")

# Create temperature variables

# Define temperature period one: 1886-1915
  temp1 = as.vector(temp_table[7:36,2])
# Define temperature period two: 1986-2015
  temp2 = as.vector(temp_table[107:136,2])
#pdf(file="temp_hist.pdf")
#par(mfrow = c(2, 1), cex = 0.4)
#hist(temp1,main="Temp. 1886-1915")
#hist(temp2,main="Temp. 1986-2015")
#dev.off()

# Create theoretical normal distribution
# Specify vector of quantiles
  x=seq(-4,4,length=1000)

# Specify significance level
  alpha1 = 0.1
  alpha2=0.01
  alpha3=0.001

# perform standard two-sample t-test
  test1=t.test(temp1,temp2,mu=0,
    conf.level=1-alpha1,
    var.equal=TRUE,  
    alternative="two.sided")
  test2=t.test(temp1,temp2,mu=0,
               conf.level=1-alpha2,
               var.equal=TRUE,  
               alternative="two.sided")
  test3=t.test(temp1,temp2,mu=0,
               conf.level=1-alpha3,
               var.equal=TRUE,  
               alternative="two.sided")
# look at test result
  print(test1)

##################

pdf(file="t-test_plot3.pdf")


# plot theoretical t distribution

# First specify number of degrees of freedom based on number of data elements
  df=length(temp1)+length(temp2)-2
  plot(x,dt(x,df=df),type="l",lwd=5,xlab="Temperature Anomaly (degC)",
       ylab="Density")

# plot location of t-stat
  abline(v=test3$statistic,col="red",lwd=5)

# Calculate quantiles for critical significance levels

# Lower limit
  abline(v=qt(alpha3/2,df=df),lwd=3,lty="dashed",col="black")
  print(qt(alpha3/2,df=df))

# Upper limit
  abline(v=qt(1-alpha3/2,df=df),lwd=3,lty="dashed",col="black")
  print(qt(1-alpha3/2,df=df))


# plot the confidence interval
  abline(v=mean(temp1)-mean(temp2),lwd=3,col="blue")
  abline(v=test3$conf.int[1],lwd=3,lty="dashed",col="blue")
  abline(v=test3$conf.int[2],lwd=3,lty="dashed",col="blue")


# Recall the parameter we are estimating is difference in population means
# Does the confidence interval contain the true value?
#  - For example, if the difference in populations is zero, then
#    confidence interval should contain 0 (1-alpha)% of time


#Create Legend
  legend(x="topleft",
       legend=c("Theoretical T-distribution", 
       "Sample T-statistic", "95% Confidence Interval"),
       col=c("black","red","blue"),
       lty=c("solid","solid","dashed"),
       lwd=c(5,5,5)
        )

dev.off()




