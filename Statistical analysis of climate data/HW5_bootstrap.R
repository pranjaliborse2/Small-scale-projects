##################
# bootstrap hypothesis test
##################
#
# Consider two samples drawn from normally distributed
#  data sets (populations)
#
# We want to test the hypothesis that the population means
# are significantly different from each other
# 
# Use the bootstrap method (resampling with replacement) to 
# create an empirical distribution of the test statistic
# (difference in sample means)
# 
# Steps:
#  - Resample the original data to create new samples
#  - Calculate a test statistic for each bootstrap sample (eg. mean difference)
#  - construct empirical distributions of the test statistic
#  - analyze the statistical properties of the empirical 
#    distributions (quantiles, 95% range, etc.)
#
##################


# Read in Feb temp data from text file
  temp_table = read.table("./temp_il_jan-feb_1880-2016.txt")

# Create temperature variables

# Define temperature period one: 1886-1915
  temp1 = as.vector(temp_table[7:36,2])
# Define temperature period two: 1986-2015
  temp2 = as.vector(temp_table[107:136,2])

# Number of samples
N1=10
N2=100
N3=1000


# Define arrays for store results
mean_array_1=rep(NA, length.out=10000)
mean_array_2=rep(NA, length.out=10000)
mean_array_diff=rep(NA, length.out=10000)

# Use a for loop to calculate bootstrap statistics
  for(i in 1:10000) {
    sample_1_boot=sample(temp1,size=length(temp1),replace=TRUE)
    sample_2_boot=sample(temp2,size=length(temp2),replace=TRUE)

    mean_array_1[i]=mean(sample_1_boot)
    mean_array_2[i]=mean(sample_2_boot)
    mean_array_diff[i]=mean(sample_1_boot)-mean(sample_2_boot)
  }


#  Compare empirical densities of distributions of mean summer temp
     # 2x2 panel plot
pdf(file="bootstrap_plot4.pdf")
par(mfrow=c(2,1), cex=0.4)
   plot(density(mean_array_1),
      xlim=c(-4,4),col="blue",lwd=3,
      main="Bootstrap Distribution of means",      
      ylab="density",
      xlab="sample mean")
  abline(v=mean(mean_array_1),col="blue",lwd=3)

  lines(density(mean_array_2),col="red",lwd=3)
  abline(v=mean(mean_array_2),col="red",lwd=3)


   plot(density(mean_array_diff),
      xlim=c(-4,4),col="black",lwd=3,
      main="Bootstrap Distribution of Difference in Means",
      ylab="density",
      xlab="sample mean difference")


# Analyze the width of the 95% Range
q95=quantile(mean_array_diff,probs=c(0.025,0.975))
print(q95)

abline(v=q95,col="black",lwd=3)
abline(v=mean(mean_array_diff),col="black",lwd=3)
dev.off()


################################
# Question:  How does the 95% range calculated using the bootstrap test
# compare with the 95% confidence interval calculated from the t-test?
################################



