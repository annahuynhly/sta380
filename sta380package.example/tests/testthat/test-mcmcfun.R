#########################################
# Independent MH Example                #
#########################################

set.seed(1)
N <- 100000
burn <- 100
rho <- 0.7
sigma_matrix <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
mu_vec <- c(0, 0)
test_pt1 = rmvnorm(n = 1, mean = c(0, 0), sigma = sigma_matrix)
sim_attempt1 = rbivariate.mh_ind(N, burn, test_pt1, sigma_matrix, mu_vec)

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

#########################################
# Random Walk MH Example                #
#########################################

set.seed(1)
N <- 100000
burn <- 100
rho <- 0.7
sigma_matrix <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
mu_vec <- c(0, 0)
test_pt1 <- rmvnorm(n = 1, mean = c(0, 0), sigma = sigma_matrix)
sim_attempt2 <- rbivariate.rwmh(N, burn, test_pt1, sigma_matrix, mu_vec)

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

#########################################
# Gibbs Sampler Example                 #
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
