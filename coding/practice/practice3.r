# practice 3

n <- 10^4
u <- runif(n, 2, 4)
est <- 1/4 + 2*mean(u/8)

# comparing to the true value...
# you can integrate by hand or use the R integrate function
help(integrate)

fy <- function(x) {
  ifelse(x <= 0, 0,
         ifelse(x < 2, 1/8,
                ifelse(x < 4, x/8, 0)))
}

true_val <- integrate(fy, lower = 0, upper = 4)
# use below if you ever want to diagnose a specific value for comparison
str(true_val)
# we see the value of the integral is hidden within $value

abs(est - true_val$value)

library(testthat)

test_that("Ensuring mc estimation of mean is sensible", {
  expect_equal(est, true_val$value, tol = 1e-3)
})

## weibull question

n <- 10^4
x <- rweibull(n, shape = 2, scale = 3)
prob_est <- sum(x < 3)/n
prob_est

prob_exa <- pweibull(3, shape = 2, scale = 3)

test_that("Ensuring mc estimation of mean is sensible", {
  expect_equal(prob_est, prob_exa, tol = 1e-2)
})
# if you increase n, you can also decrease the above tolerance.

# do the computation without using rweibull
# acceptance-rejection from before...

n <- 10^4
accepted <- numeric(n) 
u_accepted <- numeric(n) 
i <- 0 
iteration <- 0 
while(i < n){
  y <- rexp(n = 1, rate = 1/3) # candidate from g
  u <- runif(1) # u ~ uniform(0, 1)
  ftgt <- (1/3) * y * exp(-(y/3)^2 + y/3) # f(x)/cg(x)
  
  if(u < ftgt){
    i <- i+1
    accepted[i] <- y
    u_accepted[i] <- u
  }
  iteration <- iteration + 1
}
# below are the samples we are going to use
accepted
prob_est2 <- sum(accepted < 3)/n
prob_est2

test_that("Ensuring mc estimation of mean is sensible", {
  expect_equal(prob_est2, prob_exa, tol = 1e-2)
})

