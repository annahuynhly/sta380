
help(pgeom)

n <- 10^4
samp <- rgeom(n, prob = 4/5)
est <- (5/4)*mean((samp^2 + 2)^(-5))
est

m<-10000 # brute force method
total <- 0
for(i in 0:m){
  total <- total + (i^2 + 2)^(-5) * (5)^(-i)
}
total

library(testthat)

test_that("comparing s", {
  expect_equal(est, total, tol = 0.0001)
})

#approximate pi

n <- 10^7
u1 <- runif(n, -1, 1)
u2 <- runif(n, -1, 1)

indicator <- ((u1^2 + u2^2) <= 1)

4*mean(indicator)
pi

test_that("comparing pi", {
  expect_equal(4*mean(indicator), pi, tol = 0.0001)
})

n <- 10^6
x <- rexp(n)
pnorm(2.5, lower.tail = FALSE)

test_that("comparing pi", {
  est <- mean(1/(sqrt(2*pi)) * exp(-(x^2/2) + x) * (x > 2.5))
  x1 <- pnorm(2.5, lower.tail = FALSE)

  expect_equal(est, x1, tol = 0.0001)
})


n <- 10^6
x <- rnorm(n)
est <- sqrt(2*pi) * mean(tan(x) * ((0 < x) & (x < 1)))
est

fun <- function(x){tan(x) * exp(-(x^2/2))}

int <- integrate(fun, 0, 1)$value
int

test_that("importance sampling part 2", {
  expect_equal(est, int, tol= 0.0006)
})


n<-10^4
u <- runif(n/2) # typo!! should be doing for n/2
anti1 <- mean( 1/(2*(1+u)) + 1/(2*(1+(1-u))) )

log(2)

test_that("antithetic variables part 1", {
  expect_equal(anti1, log(2), tol= 0.0005)
})

n <- 10^4
u <- runif(n/2)

phi_est <- function(x){
  if(x > 0){
    est <- mean(1/(2*sqrt(2*pi)) * x * exp(-(x*u)^2/2) 
                + 1/(2*sqrt(2*pi)) * x * exp(-(x*(1-u))^2/2))
    return((1/2) + est)
  } else {
    return(1 - phi_est(-x))
  }
}

# phi(2.9) phi(-3.2)
phi_est(2.9)
pnorm(2.9)

phi_est(-3.2)
pnorm(-3.2)

phi_est(2)
pnorm(2)


test_that("test for mul values of phi", {
  expect_equal(phi_est(2.9), pnorm(2.9), tol = 0.002)
  expect_equal(phi_est(-3.2), pnorm(-3.2), tol = 0.002)
})








