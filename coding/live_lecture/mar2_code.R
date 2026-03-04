
# use the sleepstudy dataset from lme4 package
# if you need to install...
install.packages("lme4")

data(sleepstudy, package = "lme4")

# a) try to undersand the dataset

??lme4::sleepstudy

## b) use nonparametric bootstrapping for mean of reaction time

# the number of bootstrapped samples
B <- 200
# below is the sample size of the bootstrapped sample
n <- nrow(sleepstudy)
R <- numeric(B) # the size of the bootstrapped statistics should be the same
# as the number of bootstrapped samples.

for(b in 1:B){
  ind <- sample(1:n, size = n, replace = TRUE)
  R[b] <- mean(sleepstudy$Reaction[ind])
}

# mean of the bootsrapped replicates
mean(R)
mean(sleepstudy$Reaction)

# version 2, matrix method for computing the bootstrap.

B <- 200
n <- nrow(sleepstudy)
R <- numeric(B) 

mat <- matrix(
  sample.int(n, size = n*B, replace = TRUE), nrow = B, ncol = n
)

R <- rowMeans(
  matrix(sleepstudy$Reaction[mat], nrow = B, ncol = n)
)

mean(R)
mean(sleepstudy$Reaction)

# c) compute the estimate of the standard error of the sample mean.

# borrow R from part b)

sd(R)
# compare this to the true standard erorr
sd(sleepstudy$Reaction) / sqrt(nrow(sleepstudy))

# d) compute the bootstrap estimate of the bias of the sample mean.

mean(R) - mean(sleepstudy$Reaction)


######## package rank examples...

install.packages("packageRank")

library("packageRank")
cranDownloads("lme4")
cranDownloads("ggplot2")

packageRank::queryRank(1:30)


#### percentile bootstra confidence interval

alpha = 0.05
# R from before...
quantile(R, c(alpha/2, 1-alpha/2))

### empiricial bootstrap confidence interval

delta_star <- R - mean(sleepstudy$Reaction)

qstar <- quantile(delta_star, c(1-alpha/2, alpha/2))

mean(sleepstudy$Reaction) - qstar

########################
# JACKKNIFE TIME

data(sleepstudy, package = "lme4")
n <- nrow(sleepstudy)
jack_mean <- numeric(n)
jack_var <- numeric(n)

for(i in 1:n){
  new_samp <- sleepstudy$Reaction[-i]
  jack_mean[i] <- mean(new_samp)
  jack_var[i] <- var(new_samp)
}

mean(jack_mean)
mean(jack_var)
# compare jackknife mean to true sample mean.
mean(sleepstudy$Reaction)
var(sleepstudy$Reaction)


#### compute the jackknife bias of the sleepstudy dataset.
jack_est_bias <- numeric(n)

true_samp_var <- (1/n) * sum((sleepstudy$Reaction - mean(sleepstudy$Reaction))^2)
for(i in 1:n){
  new_samp <- sleepstudy$Reaction[-i]
  jack_est_bias[i] <- (1/(n-1)) * sum((new_samp - mean(new_samp))^2)
}
(n-1)*(mean(jack_est_bias) - true_samp_var)

########## eaxample when jackknife is very bad.

n <- 10
# below im basically generating from a discrete uniform distribution
# Uniform(1, 2, 3, ..., 100)
x <- sample(1:100, size = n) # data...

J <- numeric(n)
for(i in 1:n){
  new_samp <- x[-i]
  J[i] <- median(new_samp)
}

# jackknife estimate for standard erorr...
sqrt((n-1)/n * sum( (J - mean(J))^2 ))

## compute the bootstrap estimate for the standard error of the median
B <- 800
R <- numeric(B)
for (b in 1:B){
  ind <- sample(x, size = n, replace = TRUE)
  R[b] <- median(ind)
}
sd(R)

## below is parametric bootstrap (unit 3)
m = 10^5
samp <- numeric(m)
for(i in 1:m){
  # below is very similar to rnorm, rexp, etc..
  val <- sample(1:100, size = n)
  samp[i] <- median(val)
}
sd(samp)

####################### permutation test example....

n <- 3000
x <- rexp(n, rate = 3)
y <- rgamma(n, shape = 2, rate = 5)

# directly compute the test statistic.
Fn <- ecdf(x)
Gn <- ecdf(y)

z<-c(x,y)
D0 <- max(abs(Fn(z) - Gn(z)))

# WANT TO DO THE RESAMPLING PART!!
R <- 10^5
D <- numeric(R)
for (i in 1:R){
  k <- sample(1:n, size = n/2)
  xi <- z[k]
  yi <- z[-k]
  
  Fin <- ecdf(xi)
  Gin <- ecdf(yi)
  
  D[i] <- max(abs(Fin(z) - Gin(z)))
}
# compute our p-value....
ftr <- c(D, D0) >= D0
p <- mean(ftr)
p

ks.test(x, y)


