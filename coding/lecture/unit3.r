library("testthat")
# Quick demo on matrix examples

matrix(1:9, byrow = TRUE, nrow = 3)
matrix(1:9, byrow = FALSE, nrow = 3)

# ESTIMATING THE MEAN #########################################################
# Suppose that $X_{1},X_{2}$ are iid from a standard Gaussian distribution.
# Find the Monte Carlo estimator of $\mathbb{E}[|X_1 − X_2|]$.

# Solution 1 (using matrices)
n <- 10000
samp <- matrix(c(rnorm(n), rnorm(n)), byrow = FALSE, ncol = 2)
g <- abs(samp[,1] - samp[, 2])
val1 <- mean(g)

# Solution 2 (easiest and most efficient; vectorization)
x1 <- rnorm(n)
x2 <- rnorm(n)
val2 <- mean(abs(x1-x2))

# WARNING! The book suggests the following solution:
m <- 10000
g <- numeric(m)
for (i in 1:m) {
  x <- rnorm(2)
  g[i] <- abs(x[1] - x[2])
}
val3 <- mean(g)

test_that("estimating the mean", {
  # Showing that my answer and the textbook are somewhat
  expect_equal(val1, val3, tol = 0.01)
  expect_equal(val2, val3, tol = 0.01)
  # Showing that we get a reasonable estimate to the true value
  expect_equal(val1, 2/sqrt(pi), tol = 0.01)
  
})

# Although this is easier to read, this is actually less efficient.
system.time({
  n <- 1000000
  samp <- matrix(c(rnorm(n), rnorm(n)), byrow = FALSE, ncol = 2)
  g <- abs(samp[,1] - samp[, 2])
  mean(g)
})

system.time({
  n <- 1000000
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  mean(abs(x1-x2))
})


system.time({
  m <- 1000000
  g <- numeric(m)
  for (i in 1:m) {
    x <- rnorm(2)
    g[i] <- abs(x[1] - x[2])
  }
  mean(g)
})

# In this case we see the matrix method is a bit more inefficient, however,
# in more complicated scenarios it's better than vectorization.

# ESTIMATING STANDARD ERROR ###################################################

n <- 10000
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- abs(x1-x2)
se1 <- sqrt(sum((y - mean(y))^2)) / n
se2 <- sqrt(sum((y - mean(y))^2)) / (n-1)

test_that("verifying sd and var", {
  # Need to ensure it matches theoretical standard error
  se_the <- sqrt(2 - 4/pi)/n
  expect_equal(se1, se_the, tol = 0.01)
  expect_equal(se2, se_the, tol = 0.01)
})

# ESTIMATING MSE ##############################################################

theta <- 3
# n is size of the sample
n <- 30
# m is number of replicates
m <- 100000
# Each row represents a replicate
x <- matrix(rnorm(n*m, mean = theta, sd = theta), nrow = m)

est1 <- rowMeans(x)
est2 <- sqrt((1/(n-1)) * rowSums( (x - rowMeans(x))^(2)) )

mse1 <- mean((est1 - theta)^2)
mse2 <- mean((est2 - theta)^2)





