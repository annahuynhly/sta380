source("mcmc_example.R")

#########################################
# A working example for independent MH
#########################################

set.seed(1)
N <- 100000
burn <- 100
rho <- 0.7
sigma_matrix <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
mu_vec <- c(0, 0)
test_pt1 = rmvnorm(n = 1, mean = c(0, 0), sigma = sigma_matrix)
sim_attempt1 = rbivariate.mh_ind(N, burn, test_pt1, sigma_matrix, mu_vec)

##### Doing testing (using testthat library)
# Ensuring making sure the estimated mean, and sd, are identical
test_that("Simulated data matches target distribution properties", {
  # Check marginal means
  expect_equal(mean(sim_attempt1[,1]), 0, tolerance = 0.05)
  expect_equal(mean(sim_attempt1[,2]), 0, tolerance = 0.05)
  # Check marginal standard deviations
  expect_equal(sd(sim_attempt1[,1]), 1, tolerance = 0.05)
  expect_equal(sd(sim_attempt1[,2]), 1, tolerance = 0.05)
  # Check correlation between variables
  cor_val <- cor(sim_attempt1[,1], sim_attempt1[,2])
  expect_equal(cor_val, 0.7, tolerance = 0.05)
})

##### Creating plots
par(mfcol=c(1,1))
plot(sim_attempt1[,1], sim_attempt1[,2],
     xlab = "Variable 1",
     ylab = "Variable 2",
     main = "Scatter plot of the simulated bivariate Gaussian samples obtained 
     via the independence Metropolis–Hastings algorithm.")

default1 = c("#FF6666", "#6699FF", "#05DEB2", "#3333FF", "#5b10a7")

par(mfcol=c(2,2))
xx = seq(-4,4,by=0.01)
hist(sim_attempt1[,1], breaks=100, freq=FALSE, 
     main = "Histogram of Variable 1",
     xlab = "Variable 1",
     col = "#6699FF", border = "white")
lines(xx, dnorm(xx), col="red", lwd = 2, lty = 2)
qqnorm(sim_attempt1[,1], main = "Normal Q–Q Plot for Variable 1",
       col = "#6699FF")
qqline(sim_attempt1[,1], col="red", lwd = 2, lty = 2)

hist(sim_attempt1[,2], breaks=100, freq=FALSE,
     main = "Histogram of Variable 2",
     xlab = "Variable 2",
     col = "#05DEB2", border = "white")
lines(xx, dnorm(xx), col="red", lwd = 2, lty = 2)
qqnorm(sim_attempt1[,2], main = "Normal Q–Q Plot for Variable 2",
       col = "#05DEB2")
qqline(sim_attempt1[,2], col="red", lwd = 2, lty = 2)

#########################################
# A working example for random walk MH
#########################################

set.seed(1)
N <- 100000
burn <- 100
rho <- 0.7
sigma_matrix <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
mu_vec <- c(0, 0)
test_pt1 <- rmvnorm(n = 1, mean = c(0, 0), sigma = sigma_matrix)
sim_attempt2 <- rbivariate.rwmh(N, burn, test_pt1, sigma_matrix, mu_vec)

##### Doing testing (using testthat library)
# Ensuring making sure the estimated mean, and sd, are identical
test_that("Simulated data matches target distribution properties", {
  # Check marginal means
  expect_equal(mean(sim_attempt2[,1]), 0, tolerance = 0.05)
  expect_equal(mean(sim_attempt2[,2]), 0, tolerance = 0.05)
  # Check marginal standard deviations
  expect_equal(sd(sim_attempt2[,1]), 1, tolerance = 0.05)
  expect_equal(sd(sim_attempt2[,2]), 1, tolerance = 0.05)
  # Check correlation between variables
  cor_val <- cor(sim_attempt2[,1], sim_attempt2[,2])
  expect_equal(cor_val, 0.7, tolerance = 0.05)
})

##### Creating plots
par(mfcol=c(1,1))
plot(sim_attempt2[,1], sim_attempt2[,2],
     xlab = "Variable 1",
     ylab = "Variable 2",
     main = "Scatter plot of the simulated bivariate Gaussian samples obtained 
     via the Random Walk Metropolis–Hastings algorithm.")

par(mfcol=c(2,2))
xx = seq(-4,4,by=0.01)
hist(sim_attempt2[,1], breaks=100, freq=FALSE, 
     main = "Histogram of Variable 1",
     xlab = "Variable 1",
     col = "#6699FF", border = "white")
lines(xx, dnorm(xx), col="red", lwd = 2, lty = 2)
qqnorm(sim_attempt2[,1], main = "Normal Q–Q Plot for Variable 1",
       col = "#6699FF")
qqline(sim_attempt2[,1], col="red", lwd = 2, lty = 2)

hist(sim_attempt2[,2], breaks=100, freq=FALSE,
     main = "Histogram of Variable 2",
     xlab = "Variable 2",
     col = "#05DEB2", border = "white")
lines(xx, dnorm(xx), col="red", lwd = 2, lty = 2)
qqnorm(sim_attempt2[,2], main = "Normal Q–Q Plot for Variable 2",
       col = "#05DEB2")
qqline(sim_attempt2[,2], col="red", lwd = 2, lty = 2)

#########################################
# A working example for the Gibbs Sampler
#########################################

set.seed(1)
N <- 100000
burn <- 100
rho <- 0.7
sigma_matrix <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
mu_vec <- c(0, 0)
test_pt1 <- rmvnorm(n = 1, mean = c(0, 0), sigma = sigma_matrix)
sim_attempt3 <- gibbs_sampler(N, burn, test_pt1, sigma_matrix, mu_vec)

##### Doing testing (using testthat library)
# Ensuring making sure the estimated mean, and sd, are identical
test_that("Simulated data matches target distribution properties", {
  # Check marginal means
  expect_equal(mean(sim_attempt3[,1]), 0, tolerance = 0.05)
  expect_equal(mean(sim_attempt3[,2]), 0, tolerance = 0.05)
  # Check marginal standard deviations
  expect_equal(sd(sim_attempt3[,1]), 1, tolerance = 0.05)
  expect_equal(sd(sim_attempt3[,2]), 1, tolerance = 0.05)
  # Check correlation between variables
  cor_val <- cor(sim_attempt3[,1], sim_attempt3[,2])
  expect_equal(cor_val, 0.7, tolerance = 0.05)
})

##### Creating plots
par(mfcol=c(1,1))
plot(sim_attempt3[,1], sim_attempt3[,2],
     xlab = "Variable 1",
     ylab = "Variable 2",
     main = "Scatter plot of the simulated bivariate Gaussian samples obtained 
     via the Random Walk Metropolis–Hastings algorithm.")

par(mfcol=c(2,2))
xx = seq(-4,4,by=0.01)
hist(sim_attempt3[,1], breaks=100, freq=FALSE, 
     main = "Histogram of Variable 1",
     xlab = "Variable 1",
     col = "#6699FF", border = "white")
lines(xx, dnorm(xx), col="red", lwd = 2, lty = 2)
qqnorm(sim_attempt3[,1], main = "Normal Q–Q Plot for Variable 1",
       col = "#6699FF")
qqline(sim_attempt3[,1], col="red", lwd = 2, lty = 2)

hist(sim_attempt3[,2], breaks=100, freq=FALSE,
     main = "Histogram of Variable 2",
     xlab = "Variable 2",
     col = "#05DEB2", border = "white")
lines(xx, dnorm(xx), col="red", lwd = 2, lty = 2)
qqnorm(sim_attempt3[,2], main = "Normal Q–Q Plot for Variable 2",
       col = "#05DEB2")
qqline(sim_attempt3[,2], col="red", lwd = 2, lty = 2)




