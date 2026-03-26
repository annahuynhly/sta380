# unit 4

# We are not using linear mixed modelling but I want to use the dataset in lme4

#Use the “sleepstudy” dataset from the lme4 R package. Do the following:
#  (a) Try to understand what the dataset is about. Use ‘??lme4::sleepstudy’

??lme4::sleepstudy

#(b) Using nonparametric bootstrapping, estimate the mean of the reaction time.

data("sleepstudy", package = "lme4") 
names(sleepstudy)

# METHOD 1
system.time({
  B <- 200 
  n <- nrow(sleepstudy)
  R <- numeric(B) 
  
  # Not the most efficient; but looks prettier
  for (b in 1:B) {
    #randomly select the indices
    i <- sample(1:n, size = n, replace = TRUE)
    R[b] <- mean(sleepstudy$Reaction[i])
  }
})

# Mean of the bootstrap replicates
mean(R)
# Mean of the sample
mean(sleepstudy$Reaction)
# Standard error of the replicates
sd(R)

# METHOD 2
# more efficient method, however, definitely harder to wrap your head around
system.time({
  B <- 200 
  n <- nrow(sleepstudy)
  
  # creating the matrix that has all of the sampled values
  mat <- matrix(
    sample.int(n, size = n * B, replace = TRUE), 
    nrow = B, ncol = n)
  
  # computing the mean of each replicate
  R <- rowMeans(
    matrix(sleepstudy$Reaction[mat], nrow = B, ncol = n)
  )
})

mean(R)
mean(sleepstudy$Reaction)

#(c) Compute the bootstrap estimate of the standard error of the sample mean.

sd(R)
sd(sleepstudy$Reaction) / sqrt(nrow(sleepstudy))

#(d) Compute the bootstrap estimate of bias of the sample mean.

mean(R) - mean(sleepstudy$Reaction)

# bootstrap confidence interval

alpha = 0.05
#c(quantile(R, alpha/2), quantile(R, 1-alpha/2))
quantile(R, c(alpha/2, 1-alpha/2))

mean(R)

# some people don't like it; may want to talk about the empirical bootstrap:
# as discussed in the link below:
# https://math.mit.edu/~dav/05.dir/class24-prep-a.pdf

deltastar <- R - mean(sleepstudy$Reaction)
alpha = 0.05
# by default, R is LOWER tail...
q_star <- quantile(deltastar, c(1 -alpha/2, alpha/2))
# below is the confidence interval
mean(sleepstudy$Reaction) - q_star

######################################## side note:
# There's lots of cool packages in R
??lme4::sleepstudy

library(packageRank)

cranDownloads("easystats")

packageRank("easystats")

packageRank("tidyverse")

packageRank("lme4")

# see top 30 packages...
packageRank::queryRank(1:30)

######################################## 

# Compute a jackknife estimate of the sample mean and variance of the reaction 
# time from the sleepstudy dataset found in lme4. 
# Now, compare this to the sample mean of the reaction times. What do you notice?
jack_mean <- numeric(n)
jack_var <- numeric(n)
for(i in 1:n){
  new_samp <- sleepstudy$Reaction[-i]
  jack_mean[i] <- mean(new_samp)
  jack_var[i] <- var(new_samp)
}

mean(jack_mean)
mean(sleepstudy$Reaction)
# they are the same value!
mean(jack_var)
var(sleepstudy$Reaction) 
# same with the variance -- they are the same value!

# Consider the reaction time data from the sleepstudy dataset in the lme4 package.
# Compute the jackknife estimate of the bias of the following estimator for the variance of
# the reaction times:

jack_est_bias <- numeric(n)
var_est <- (1/n) * sum((sleepstudy$Reaction - mean(sleepstudy$Reaction))^2)
for(i in 1:n){
  new_samp <- sleepstudy$Reaction[-i]
  jack_est_bias[i] <- (1/(n-1)) * sum((new_samp - mean(new_samp))^2)
}
(n-1)*(mean(jack_est_bias) - var_est)


##################################
# Jackknife fails example (may want to change the code example?)
n <- 10
x <- sample(1:100, size = n)

M <- numeric(n)
for (i in 1:n) { #leave one out
  y <- x[-i]
  M[i] <- median(y)
}

# jackknife
sqrt((n-1)/n * sum((M - mean(M))^2))

############### bootstrap version for standard error
B = 200
# don't have commands like rowMedians...
R <- numeric(B)
for (b in 1:B) {
  i <- sample(x, size = n, replace = TRUE)
  R[b] <- median(i)
}
sqrt(sum((R - mean(R))^2) / (B-1))
# this works too tbh:
sd(R)

# true SE is messy. if you tried parametric bootstrapping:
m = 10^4
samp <- numeric(m)
for(i in 1:m){
  # note: this is parametric because we are getting the sample directly, NOT
  # with x.
  val <- sample(1:100, size = n) 
  samp[i] <- median(val)
}
sd(samp)

######


n <- 30
x <- rexp(n, rate = 3)
y <- rgamma(n, shape = 2, rate = 5)

# here, we pool the samples, assuming that Z is a distribution in which
# is the same as both X and Y.
z <- c(x, y)

## brute force KS test
# compute ecdf from x and y, separately (unpooled)
Fn <- ecdf(x)
Gn <- ecdf(y)
D0 <- max(abs(Fn(z) - Gn(z)))
# optional: compare to the ks.test() function
# D0 <- ks.test(x, y, exact = FALSE)$statistic

## now we want to resample.
R <- 999
D <- numeric(R)
for (i in 1:R) {
  k <- sample(1:n, size = 2*n, replace = FALSE)
  xi <- z[k]
  yi <- z[-k]
  
  Fni <- ecdf(xi)
  Gni <- ecdf(yi)
  
  D[i] <- max(abs(Fni(z) - Gni(z)))
}
# we need to include D0 as technically, it counts as a sample...
p <- mean(c(D0, D) >= D0)
p

