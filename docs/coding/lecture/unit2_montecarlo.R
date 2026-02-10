##############################
# Lecture 2 Notes            #
##############################

# Compute a MC estimate of:
#  \theta = \int_{0}^{1} e^{-x} dx
# and compare the estimate with the exact value.

n <- 10000
u <- runif(n, 0, 1)
# Monte Carlo estimate
mean(exp(-u))
# True value
1-exp(-1)

# We're going to start using the testthat library.
library("testthat")
test_that("MC test 1", {
  expect_equal(mean(exp(-u)), 1-exp(-1), tol = 0.05)
})

# Compute a MC estimate of:
#  \theta = \int_{2}^{4} e^{-x} dx
# and compare the estimate with the exact value.

n <- 10000
# Monte Carlo estimate (method 1)
y <- runif(n, 0, 1)
method1 <- mean(exp(-(y*(4-2) + 2)) * (4-2))

# Monte Carlo estimate (method 2)
u <- runif(n, 2, 4)
method2 <- (4-2) * mean(exp(-u))

test_that("MC test 2", {
  expect_equal(method1, -exp(-4) + exp(-2), tol = 0.05)
  expect_equal(method2, -exp(-4) + exp(-2), tol = 0.05)
})

# Use the Monte Carlo approach to estimate the standard Gaussian cdf:
# \Phi(x) = \int_{-\infty}^{x} \frac{1}{\sqrt{2\pi}} e^{-t^2/2} dt
# Where $x > 0$. Try both methods and compare the results to pnorm().

n <- 10000
x1 <- 0.5 # choice of x is arbitrary
x2 <- -x1

# Method 1
mc_gaussian = function(n, x){
  u <- runif(n, 0, 1)
  if(x > 0){
    thetahat <- 1/sqrt(2*pi) * mean(x * exp((-(u*x)^2)/2))
    return(0.5 + thetahat)
  } else {
    return(1 - mc_gaussian(n, -x))
  }
}

q3_method1_pos <- mc_gaussian(n, x1)
q3_method2_neg <- mc_gaussian(n, x2)

test_that("MC test 3 (Method 1)", {
  expect_equal(q3_method1_pos, pnorm(x1), tol = 0.01)
  expect_equal(q3_method2_neg, pnorm(x2), tol = 0.01)
})

# Method 2
mc_gaussian2 = function(n, x){
  u <- runif(n, 0, abs(x))
  thetahat <- 1/sqrt(2*pi) * mean(exp((-(u)^2)/2))
  if(x > 0){
    return(0.5 + x * thetahat)
  } else {
    # calling the same function again, lol.
    return(1 - mc_gaussian2(n, -x))
  }
}

q3_method2_pos <- mc_gaussian2(n, x1)
q3_method2_neg <- mc_gaussian2(n, x2)

test_that("MC test 3 (Method 2)", {
  expect_equal(q3_method2_pos, pnorm(x1), tol = 0.01)
  expect_equal(q3_method2_neg, pnorm(x2), tol = 0.01)
})

# Let $Z$ be a standard Gaussian distribution. 
# Compute a simple Monte Carlo estimate of $\mathbb{E}[Z | -2 \leq Z \leq 1]$.
n <- 10^6
u <- runif(n, -2, 1)
num <- 3 * mean(u/sqrt(2*pi) * exp(-u^2/2))
denom <- 3 * mean(1/sqrt(2*pi) * exp(-u^2/2))

# comparing to the true value
cond_exp_gau <- function(x){x/sqrt(2*pi) * exp(-x^2/2)}
true_num <- integrate(cond_exp_gau, -2, 1)
true_denom <- pnorm(2) - pnorm(-1)

test_that("comparing conditional expectation vs true", {
  expect_equal(num/denom, true_num$value/true_denom, tol = 0.01)
})


# Let $X \sim N(0, 1)$. Use the hit or miss method to estimate 
# $\mathbb{P}(X > 2)$ and $\mathbb{P}(0 < X < 1)$. 
# Compare this value with the  R-function pnorm().

n <- 10000
x <- rnorm(n)
# note that the "x>2" is a boolean operator; if TRUE, we can treat it as 1.
estimate1 <- sum(x>2)/n
estimate2 <- sum((0 < x) & (x < 1))/n

test_that("Compare to pnorm()", {
  expect_equal(estimate1, pnorm(2, lower.tail = FALSE), tol = 0.01)
  expect_equal(estimate2, pnorm(1) - pnorm(0), tol = 0.01)
})

# Using R, generate a standard Gaussian random sample. Create the following 
# lines and put them on the same plot:
# - The empirical cumulative distribution function (ECDF).
# - The standard Gaussian cumulative distribution function.

n1 <- 10^2; n2 <- 10^3; n3 <- 10^4
x1 <- rnorm(n1); x2 <- rnorm(n2); x3 <- rnorm(n3)
Fn1 <- ecdf(x1); Fn2 <- ecdf(x2); Fn3 <- ecdf(x3)

curve(pnorm(x) , col = "black", xlim = c(-3,3), lwd = 2)
lines(Fn1, do.points = FALSE, col = "blue", lwd = 2)
lines(Fn2, do.points = FALSE, col = "red", lwd = 2)
lines(Fn3, do.points = FALSE, col = "green", lwd = 2)
legend("bottomright", 
       legend = c("pnorm", "n=10^2", "n=10^4", "n=10^6"),
       lwd = c(2, 2, 2, 2),
       col = c("black", "blue", "red", "green"))

# Estimate the variance of the hit-or-miss estimators for $\mathbb{P}(Z < 2)$ 
# and $\mathbb{P}(Z < 2.5)$ and construct approximate 95\% confidence intervals 
# for these estimates.

n < 10000
z <- rnorm(n)

g1 <- (z < 2)
g2 <- (z < 2.5)

est1 <- mean(g1)
est2 <- mean(g2)

test_that("Compare to pnorm()", {
  expect_equal(est1, pnorm(2), tol = 0.01)
  expect_equal(est2, pnorm(2.5), tol = 0.01)
})

var1 <- sum((g1 - est1)^2)/n^2
var2 <- sum((g2 - est2)^2)/n^2

alpha <- 0.05
err_bds1 <- qnorm(1-alpha/2) * sqrt(var1)
err_bds2 <- qnorm(1-alpha/2) * sqrt(var2)

# first confidence interval
c(est1 - err_bds1, est1 + err_bds1)
# second confidence interval
c(est2 - err_bds2, est2 + err_bds2)

# Derive a Monte Carlo estimator of $S$ using the Geometric distribution 
# as defined below, and then approximate a value for S:
# S = \sum_{i=0}^{\infty} (i^{2} + 2)^{-5} 5^{-i}

n <- 10000
p <- 4/5

geo_samp <- rgeom(n, p)
S_est <- 5/4 * mean((geo_samp + 2)^(-5))

# Brute force S
S_brute = function(n){
  i <- 0:n
  total <- sum((i^2 + 2)^(-5) * 5^(-i))
  return(total)
}

test_that("Computing S", {
  S_bru = S_brute(n)
  expect_equal(S_est, S_bru, tol = 0.05)
})

# Using Monte Carlo to estimate pi!

n <- 10^8
u1 <- runif(n, -1 ,1)
u2 <- runif(n, -1, 1)
chk <- ((u1^2 + u2^2) <= 1)
est_pi = 4*mean(chk)

test_that("Checking pi", {
  expect_equal(est_pi, pi, tol = 0.02)
})

# Let $X \sim N(0, 1)$. Estimate $\mathbb{P}(X > 2.5)$ using importance 
# sampling in R, where the importance function is an exponential density with 
# mean 1.

n <- 10^8
x <- rexp(n)
weights <- 1/sqrt(2*pi) * exp(-(x^2)/2 + x)
imp_est <- mean((x>2.5) * weights)
true_prob = pnorm(2.5, lower.tail = FALSE)

test_that("Checking importance sampler", {
  expect_equal(imp_est, true_prob, tol = 0.0001)
})

# Using antithetic variables, estimate the following integral:
# \int_{0}^{1} \frac{1}{1+x} dx
# Compare the result to the simple monte carlo estimator and the true value.

n <- 10^8
x1 <- runif(n/2)
x2 <- runif(n)
g_total <- (1/(1+x1) + 1/(1+(1-x1)))/2

ans1 <- mean(g_total)
ans2 <- mean(1/(1+x2))

# notice that the first answer performs better..
abs(ans1 - log(2))
abs(ans2 - log(2))

# Using antithetic variables, estimate $\Phi(2.9)$ and $\Phi(-3.2)$. Recall:
# \Phi(x) = \int_{-\infty}^{x} \frac{1}{\sqrt{2\pi}} \exp\{-t^{2}/2\} dt
# Compare the result to the simple Monte Carlo estimator that we computed earlier.

# antithetic variable approach
x <- 2.9
n <- 10^8

time1 <- system.time({
  u <- runif(n/2)
  g1 <- 1/sqrt(2*pi) * x * exp(-(x*u)^2 / 2)
  g2 <- 1/sqrt(2*pi) * x * exp(-(x*(1-u))^2 / 2)
  g_total <- (g1 + g2)/2
  ans1 <- 0.5 + mean(g_total) 
})

# simple mc approach (copied and pasted)
time2 <- system.time({
  ans2 <- mc_gaussian(n, x)
})

# again, first answer performs better (even in terms of time.)
help(system.time)
time1
time2

abs(ans1 - pnorm(2.9))
abs(ans2 - pnorm(2.9))


