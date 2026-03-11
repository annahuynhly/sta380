#TUT9

#sampling distribution of an estimator is difficult or impossible to derive analytically.
#estimate properties of an estimator using the observed data itself rather than relying on theoretical formulas.

#Question1
#install.packages("MEMSS")
library("MEMSS")
data(RatPupWeight, package = "MEMSS")
# to read the documentation
??MEMSS::RatPupWeight
B <- 200 #the number of bootstrap samples.
n <- nrow(RatPupWeight) #sample size of the original dataset.

# this is to obtain which indices to sample from.
mat <- matrix(
  sample.int(n, size = n * B, replace = TRUE),
  nrow = B, ncol = n)

# now we want the matrix that contains the actual sampled weight value
weights <- matrix(RatPupWeight$weight[mat], nrow = B, ncol = n)
# computing the mean of each replicate
R <- rowMeans(weights)

#(a)Using nonparametric bootstrapping, estimate the mean of the rat’s weights. Use 200 replicates
mean(R) #6.083458
# optional: comparing this to the sample mean
samp_mean <- mean(RatPupWeight$weight)
samp_mean #6.080963

#(b)Compute the bootstrap estimate of the standard error of the bootstrap sample mean. 
sd(R) #0.03568181
# optional: comparing this to the sample standard deviation
sd(RatPupWeight$weight) / sqrt(n) #0.03607971

#(c) Compute the bootstrap estimate of bias of the bootstrap sample mean. 
mean(R) - samp_mean #0.002495186

#(d) 95% bootstrap percentile confidence interval for the mean of the rat’s weights
#alpha = 0.05
#c(quantile(R, alpha/2), quantile(R, 1-alpha/2))
quantile(R, c(0.025, 0.975)) #(6.012896, 6.148082) 

#0.05/2=0.025
#alpha/2 = 0.025; 1-0.025= 0.975


#(e) to test whether the weights of the male and female pups come from the same distribution. Perform
#a permutation test for equal distributions by computing the K-S Statistic.

#Permutation tests are based on resampling without replacement
#null:Male and female weights come from the same distribution

f <- RatPupWeight[RatPupWeight$sex == "Female", ]
m <- RatPupWeight[RatPupWeight$sex == "Male", ]
D0 <- suppressWarnings(ks.test(f$weight, m$weight)$statistic) #observed KS statistic #0.2247783 
R <- 999
D <- numeric(R)
for (i in 1:R) {
  k <- sample(1:n, size = n/2, replace = FALSE)
  xi <- RatPupWeight$weight[k]
  yi <- RatPupWeight$weight[-k] #rest of k
  D[i] <- suppressWarnings(ks.test(xi, yi)$statistic)
} #D[i]: KS statistics from the i-th permutation sample.

# we need to include D0 as technically, it counts as a sample...
p <- mean(c(D0, D) >= D0) #permutation p-value.
p #0.001 < 0.05
#length(c(D0, D))
#conclusion: There is strong evidence that the weight distributions of male and female rat pups are different.

#Question2
data(Relaxin, package = "MEMSS")
??MEMSS::Relaxin
# Beginning with boostrap method
B <- 200
n <- nrow(Relaxin)
mat <- matrix(
  sample.int(n, size = n * B, replace = TRUE),
  nrow = B, ncol = n)
rel_conc <- matrix(Relaxin$conc[mat], nrow = B, ncol = n)
# (a) Using nonparametric bootstrapping, estimate the standard error of the glucose concentration levels. 
#Use 200 replicates.
R <- rowMeans(rel_conc) #mean concentration for each bootstrap sample.
sd(R) #0.5242067

#(b) Using jackknife, estimate the standard error of the glucose concentration levels.
#Jackknife method, removing one observation at a time.
jack_est <- numeric(n) #creates a vector to store
for(i in 1:n){
  new_samp <- Relaxin$conc[-i]
  jack_est[i] <- mean(new_samp)
}
sqrt((n-1)/n * sum((jack_est - mean(jack_est))^2)) #lecture slides P.37 #0.5162592

#(c) Using nonparametric bootstrapping, estimate the bias of the variance estimator, as described below,
#of the glucose concentration levels.Use 200 replicates.
boot_est <- (1/n) * rowSums((rel_conc - rowMeans(rel_conc))^2)
var_mle <- (1/n) * sum((Relaxin$conc - mean(Relaxin$conc))^2) #(1/n) * sum(x_i - x_bar)^2 

boot_ver <- mean(boot_est) - var_mle #-0.1789443

#(d)Using jackknife, estimate the bias of the variance estimator, the one described in the last question,
#of the glucose concentration levels.
jack_est_bias <- numeric(n)
var_est <- (1/n) * sum((Relaxin$conc - mean(Relaxin$conc))^2)
for(i in 1:n){
  new_samp <- Relaxin$conc[-i]
  jack_est_bias[i] <- (1/(n-1)) * sum((new_samp - mean(new_samp))^2)
}
jackknife_ver <- (n-1)*(mean(jack_est_bias) - var_est) #Lecture slides, Page 30 #-0.2665236
#Removing one observation out of n only changes the estimator by about 1/(n−1) of the true bias.
#hist(Relaxin$conc)

abs(boot_ver - jackknife_ver) #0.08757935

#Question 3
#(a) Check the histogram of the protein levels (hist(Milk$protein)). Do you expect the jackknife method
#to work better here? Why or why not?

data(Milk, package = "MEMSS")
# this one actually looks somewhat Gaussian distributed
hist(Milk$protein)

#(b)Using nonparametric bootstrapping, estimate the bias of the variance estimator, as described below,
#of the glucose concentration levels. 

# Bootstrap version
B <- 200
n <- nrow(Milk)
mat <- matrix(
  sample.int(n, size = n * B, replace = TRUE),
  nrow = B, ncol = n)

milk_pro <- matrix(Milk$protein[mat], nrow = B, ncol = n)
boot_est <- (1/n) * rowSums((milk_pro - rowMeans(milk_pro))^2)
var_mle <- (1/n) * sum((Milk$protein - mean(Milk$protein))^2)
boot_ver <- mean(boot_est) - var_mle #-5.251975e-05

#(c)Using jackknife, estimate the bias of the variance estimator, the one described in the last question,
#of the glucose concentration levels.
# Jackknife version
jack_est_bias <- numeric(n)
var_est <- (1/n) * sum((Milk$protein - mean(Milk$protein))^2)
for(i in 1:n){
  new_samp <- Milk$protein[-i]
  jack_est_bias[i] <- (1/(n-1)) * sum((new_samp - mean(new_samp))^2)
}
jackknife_ver <- (n-1)*(mean(jack_est_bias) - var_est)
abs(boot_ver - jackknife_ver) #2.979761e-05
