
mat <- matrix(1:9, nrow = 3, byrow = TRUE)

rowSums(mat)
colSums(mat)
rowMeans(mat)
colMeans(mat)

##### monte carlo estimate
m <- 10^5
x1 <- rnorm(m)
x2 <- rnorm(m)
# below is a vector of size m
# of all of the differences...
y <- abs(x1 - x2)
# below is our mc estimate for the mean.
mean(y)
2/sqrt(pi)

library(testthat)

test_that("testing 1st mc estimate", {
  expect_equal(mean(y), 2/sqrt(pi), tol = 0.01)
})

# compute the standard error.

m <- 10^5
x1 <- rnorm(m)
x2 <- rnorm(m)
y <- abs(x1 - x2)

# first mc estimate, biased est. (1/n)
se_est1 <- sqrt(sum( (y - mean(y))^2 )) / m
# second mc estimate, unbiased est. (1/(n-1))
se_est2 <- sqrt( sum( (y - mean(y))^2 ) / (m*(m-1))  )
# exact value...
se_exact <- sqrt((2 - (4/pi))/m)

test_that("se test", {
  expect_equal(se_est1, se_exact, tol = 0.01)
  expect_equal(se_est2, se_exact, tol = 0.01)
})

#1. generate our sample using a matrix.
n <- 30
m <- 10^5
theta <- 5
# the below matrix will have n*m entries.
# create normal samples that has n*m number of replicates.

mat <- matrix(rnorm(n*m, mean = theta, sd = theta), nrow = m, 
              byrow = TRUE)
dim(mat)
#2. compute theta_hat1, theta_hat2
est1 <- rowMeans(mat)
est2 <- sqrt( (1/(n-1)) * rowSums((mat - rowMeans(mat))^2) )

length(est1)
length(est2)

# computing the MSE.
mean((est1 - theta)^2)
mean((est2 - theta)^2)

# estimator 2 performs better.

# new example: compute the MC pitman closness
# use the same est1, est2 from before.

criterion <- (abs(est1 - theta) <= abs(est2 - theta))
criterion
mean(criterion) # PC probability
# if we had that the above probability >= 1/2,
# we then say that est1 is "better" (Pitman closer) than est2.

# estimator 2 is better here.
# this is consistent with the MSE.

### estimating confidence levels.

# generating our sample.
n <- 30 # sample size
m <- 10^4 # number of replicates.
mat <- matrix(rnorm(n*m, mean = 5, sd = sqrt(2)), nrow = m)

# compute the mean statstic.
est <- rowMeans(mat)
# don't forget to divide by sqrt(n) bc this is se. not sd.
# below I just used the unbiased esimator.
sample_se <- sqrt( (1/(n-1)) * rowSums( (mat - rowMeans(mat))^2 ) ) / sqrt(n)

alpha <- 0.10
lower_ests <- est - qt(alpha/2, lower.tail = FALSE, df = n-1) * sample_se
# alternatively, use lower tail:
#lower_ests <- est - qt(1- alpha/2) * sample_se
upper_ests <- est + qt(alpha/2, lower.tail = FALSE, df = n-1) * sample_se

length(lower_ests)

y <- ifelse(((5 >= lower_ests) & (5 <= upper_ests)), 1, 0)
mean(y)

###### type 1 error rate test.

n <- 20
m <- 10^5
mu0 <- 500
alpha <- 0.05

# generate the data!
mat <- matrix(rnorm(n*m, mean = mu0, sd = 100), nrow = m)
dim(mat)
# compute the test statistic
test_stat <- (rowMeans(mat) - 500) / (100/sqrt(20))

# multiply by 2 because of symmetry.
p_val <- 2 * pnorm(abs(test_stat), lower.tail = FALSE)

type_1_error <- mean(p_val < alpha)
type_1_error

## type-II error rate.

n <- 20
m <- 10^5
mu0 <- 500
mu1 <- 1000
alpha <- 0.05

# generate the data! -> type-II error rate, ASSUME alt.
# replace MEAN with mu1 INSTEAD of m0.
mat <- matrix(rnorm(n*m, mean = mu1, sd = 100), nrow = m)
# compute the test statistic
test_stat <- (rowMeans(mat) - 500) / (100/sqrt(20))
# multiply by 2 because of symmetry.
p_val <- 2 * pnorm(abs(test_stat), lower.tail = FALSE)

# type-II error is P(fail to reject | h0 is false.)
type_II_error <- mean(p_val > alpha)

# power: P(reject H0 | h0 is false.)
1-type_II_error
