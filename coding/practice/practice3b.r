# easy hit or miss method
n <- 10^5
x <- rgamma(n, shape = 2, scale = 2)
cond <- (x<2)

est <- mean(cond)
var <- sum((cond - est)^2)/n^2
alpha <- 0.10
err_bds <- qnorm(1-alpha/2) * sqrt(var)
c(est - err_bds, est + err_bds)

##########

n <- 10^4
u <- runif(n, 0, 10)

# estimate
mc_est <- sum((10*log(u)*u - mean(10*log(u)*u))^2) / n^2

# exact
fun1 <- function(x){log(x)^2*x^2/10}
fun2 <- function(x){log(x)*x/10}

ex2 <- integrate(fun1, 0, 10)
ex_sq <- integrate(fun2, 0, 10)
exact <- 100 * (ex2$value - (ex_sq$value)^2) / n

# big difference, somewhat better mean relative difference
abs(mc_est - exact)
all.equal(mc_est, exact)

# additional comparisons
n <- 10^7
u <- runif(n, 0, 10)
mc_est2 <- sum((10*log(u)*u - mean(10*log(u)*u))^2) / n^2
exact2 <- 100 * (ex2$value - (ex_sq$value)^2) / n^2

abs(mc_est2 - exact2)
all.equal(mc_est2, exact2)


#######


# getting the through value using integrate (yess we are lazy)
fun <- function(x){exp(-(log(x))^2 / 2)}
true <- integrate(fun, lower = 1, upper = 4)

# using uniform
n <- 10^5
x <- runif(n, 1, 4)
est1 <- 3 * mean(exp(-(log(x))^2 / 2))

# using inverse trannsform to generate from the log Cauchy distribution
n <- 10^5
x <- rlnorm(n)
est2 <- mean(x * sqrt(2*pi) * (x >= 1 & x <= 4))

true$value
est1
est2

all.equal(true$value, est1)
all.equal(true$value, est2)

## comparing the variance

fun1 <- function(x){3 * exp(-(log(x))^2)}
fun2 <- function(x){exp(-(log(x))^2/2) * x * sqrt(2*pi)}

int1 <- integrate(fun1, 1, 4)
int2 <- integrate(fun2, 1, 4)

# variance with uniform importance function
(1/n) * (int1$value - true$value^2)
## [1] 3.442339e-06
# variance with lognormal importance function
(1/n) * (int2$value - true$value^2)
## [1] 7.335948e-05


##############################################################
# antithetic approach question

n <- 10^4
u <- runif(n/2)
theta <- 3 # arbitrary; pick what you want
g1 <- (1/theta) * u * exp(-u/theta)
g2 <- (1/theta) * (1-u) * exp(-(1-u)/theta)
g_total <- mean((g1 + g2)/2)

# true value, as computed using IBP
true_cond_exp <- -exp(-1/theta) * (1 + theta) + theta

library(testthat)
test_that("checking the MC cond exp versus true", {
  expect_equal(g_total, true_cond_exp, tol = 0.001)
})




# side work; can ignore.
##############################################################
##### try for a function that will explode; will it make the estimator worse?

n <- 10^4
u <- runif(n, 0, 10)

# estimate
mc_est <- sum((10*log(u)*exp(u) - mean(10*log(u)*exp(u)))^2) / n^2

# exact
fun1 <- function(x){log(x)^2*exp(2*x)/10}
fun2 <- function(x){log(x)*exp(x)/10}

ex2 <- integrate(fun1, 0, 10)
ex_sq <- integrate(fun2, 0, 10)
exact <- 100 * (ex2$value - (ex_sq$value)^2) / n

# big difference, somewhat better mean relative difference
abs(mc_est - exact)
all.equal(mc_est, exact)




