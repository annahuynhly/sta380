
n<-10^4
u<-runif(n)

# method 1
total <- 0
for(i in 1:n){
  total = total + exp(-u[i])
}
total/n

# take the mean...
mean(exp(-u))
# true value
1-exp(-1)

install.packages("testthat")
library(testthat)

test_that("test simple MC vs true value",{
  expect_equal(mean(exp(-u)), 1-exp(-1), tol=0.0001)
})

########

# method 1:

n<-10^4
u<-runif(n, 0, 1)
mean(2*exp(-(2*u + 2)))

# method 2:
u2 <-runif(n, 2, 4)
mean(2*exp(-u2))

help(integrate)

fun <- function(x){exp(-x)}

true_val <- integrate(fun, 2, 4)
true_val$value

test_that("second example", {
  expect_equal(true_val$value, mean(2*exp(-(2*u + 2))),
               tol = 0.05)
  expect_equal(true_val$value, mean(2*exp(-u2)),
               tol = 0.05)
})

##### gaussian example.

gauss_test <- function(x, n = 10^4){
  u<-runif(n, 0, abs(x))
  theta_hat <- (1/sqrt(2*pi)) *mean(exp(-(u^2)/2))
  if(x >=0){
    est <- 1/2 + x * theta_hat
  } else {
    est <- 1-gauss_test(-x, n)
  }
  return(est)
}

gauss_test(6)

library(testthat)

test_that("checking gauss", {
  seq_x <- seq(-3, 3, by = 0.5)
  for(x in seq_x){
    expect_equal(gauss_test(x, n=10^6), 
                 pnorm(x), tol = 0.005)
  }
})

### conditional expectation question

n<-10^4
u<-runif(n, -2, 1)

# compute numerator
num <- 3 * mean(u * 1/sqrt(2*pi) * exp(-(u^2)/2))
# compute denominator
denom <- 3 * mean(1/sqrt(2*pi) * exp(-(u^2)/2))

est <- num/denom
est

# true value
# denom
true_denom <- pnorm(1) - pnorm(-2)

exp_gaus <- function(x){x*dnorm(x)}
true <- integrate(exp_gaus, -2, 1)

library(testthat)
test_that("..", {
  expect_equal(num, true$value, tol = 0.05)
  expect_equal(denom, true_denom, tol = 0.05)
  #expect_equal(est, true$value/true_denom, tol = 0.05)
})


#### hit or miss method

# want to est P(X > 2)
# before the break, P(X<2)
n <- 10^4
x <- rnorm(n)
hit <- (x > 2) # BIG MISTAKEEE WHOOPS
mean(hit)
test_that("..", {
  expect_equal(mean(hit), pnorm(2, lower.tail = FALSE), 
               tol =0.05)
})

# P(0 < X < 1)

n<-10^4
x <- rnorm(n)
hit <- ((0 < x) & (x < 1))
mean(hit)

test_that("..", {
  expect_equal(mean(hit), pnorm(1) - pnorm(0), tol =0.05)
})

###
n1 <- 10^2; n2 <- 10^3; n3 <- 10^4
x1 <- rnorm(n1); x2 <- rnorm(n2)
x3 <- rnorm(n3)
# dont worry too much about copying,
# I won't make you plot on a test.
Fn1 <- ecdf(x1); Fn2 <- ecdf(x2); Fn3 <- ecdf(x3)
curve(pnorm(x) , col = "black", xlim = c(-3,3), lwd = 2)
lines(Fn1, do.points = FALSE, col = "blue", lwd = 2)
lines(Fn2, do.points = FALSE, col = "red", lwd = 2)
lines(Fn3, do.points = FALSE, col = "green", lwd = 2)
legend("bottomright", 
       legend = c("pnorm", "n=10^2", "n=10^4", "n=10^6"),
       lwd = c(2, 2, 2, 2),
       col = c("black", "blue", "red", "green"))

####### construct the 95% confidence intervals for these
# estimates.

# P(Z < 2)

n<-10^4
z <- rnorm(n)
hit <- (z < 2)
est <- mean(hit)

var <- mean((hit - est)^2)/n # mistake!!!!

alpha <- 0.05
err_bd <- qnorm(alpha, lower.tail = FALSE) * sqrt(var)

# lower confidence level
est - err_bd
# upper confidence level.
est + err_bd

c(est - err_bd, est + err_bd)










