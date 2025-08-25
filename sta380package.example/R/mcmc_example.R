library(mvtnorm)
library(testthat)

#' An independence Metropolis–Hastings algorithm for Generating the
#' bivariate Gaussian distribution.
#' @description This uses the bivariate standard normal as
#' the proposal distribution.
#' @param N represents the number of samples to retain after discarding the
#' number of burn-in samples.
#' @param burn of initial samples to discard as burn-in, to reduce the
#' dependence from the starting state.
#' @param Y0 represents the initial state of the MCMC chain.
#' @param sigma_matrix a 2x2 positive definite covariance matrix of the
#' target bivariate normal distribution from which we want to sample.
#' @param mu_vec a vector of size 2 that represents the means of the
#' target bivariate normal distribution from which we want to sample.
#' @return an Nx2 matrix of samples generated using the independence
#' Metropolis–Hastings algorithm.
#' @examples
#' set.seed(1)
#' N <- 100000
#' burn <- 100
#' rho <- 0.7
#' sigma_matrix <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
#' mu_vec <- c(0, 0)
#' test_pt1 = rmvnorm(n = 1, mean = c(0, 0), sigma = sigma_matrix)
#' sim_attempt1 = rbivariate.mh_ind(N, burn, test_pt1, sigma_matrix, mu_vec)
#' @importFrom stats rnorm runif
#' @importFrom mvtnorm dmvnorm
#' @export
rbivariate.mh_ind <- function(N, burn, Y0, sigma_matrix, mu_vec) {
  V1 = rnorm(N + burn)
  V2 = rnorm(N + burn)
  U = runif(N+burn)
  Y = matrix(NA, nrow = N + burn + 1, ncol = 2)
  Y[1, ] = Y0
  for (t in 1:(N+burn)) {
    Yt = Y[t, ]
    Vt = c(V1[t], V2[t])

    # Calculating the acceptance probability
    qV  <- dnorm(Vt[1]) * dnorm(Vt[2])
    qY  <- dnorm(Yt[1]) * dnorm(Yt[2])
    piV <- dmvnorm(Vt, mean = mu_vec, sigma = sigma_matrix)
    piY <- dmvnorm(Yt, mean = mu_vec, sigma = sigma_matrix)

    a <- min(1, (piV * qY) / (piY * qV))  # acceptance prob
    # The acceptance-rejection portion
    if(U[t] < a){
      Y[t+1, ] = Vt
    } else {
      Y[t+1, ] = Yt
    }
  }
  return(Y[-(1:(burn+1)),])
}

#' A random walk Metropolis–Hastings algorithm for Generating the
#' bivariate Gaussian distribution.
#' @description Here, the proposal distribution is a random
#' walk with independent normal increments whose variances are taken from the
#' diagonal of the 2x2 positive definite covariance matrix.
#' @param N represents the number of samples to retain after discarding the
#' number of burn-in samples.
#' @param burn of initial samples to discard as burn-in, to reduce the
#' dependence from the starting state.
#' @param Y0 represents the initial state of the MCMC chain.
#' @param sigma_matrix a 2x2 positive definite covariance matrix of the
#' target bivariate normal distribution from which we want to sample.
#' @param mu_vec a vector of size 2 that represents the means of the
#' target bivariate normal distribution from which we want to sample.
#' @return an Nx2 matrix of samples generated using the random walk
#' Metropolis–Hastings algorithm.
#' @examples
#' set.seed(1)
#' N <- 100000
#' burn <- 100
#' rho <- 0.7
#' sigma_matrix <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
#' mu_vec <- c(0, 0)
#' test_pt1 <- rmvnorm(n = 1, mean = c(0, 0), sigma = sigma_matrix)
#' sim_attempt2 <- rbivariate.rwmh(N, burn, test_pt1, sigma_matrix, mu_vec)
#' @importFrom stats rnorm runif dnorm
#' @importFrom mvtnorm dmvnorm
#' @export
rbivariate.rwmh <- function(N, burn, Y0, sigma_matrix, mu_vec) {
  Z1 = rnorm(N+burn, 0, sqrt(sigma_matrix[1,1]))
  Z2 = rnorm(N+burn, 0, sqrt(sigma_matrix[2,2]))
  U = runif(N+burn)
  Y = matrix(NA, nrow = N + burn + 1, ncol = 2)
  Y[1, ] = Y0
  for (t in 1:(N+burn)) {
    Yt = Y[t, ]
    Vt = c(Yt[1] + Z1[t], Yt[2] + Z2[t])
    # Calculating the acceptance probability
    num = dmvnorm(Vt, mean = mu_vec, sigma = sigma_matrix)
    denom = dmvnorm(Yt, mean = mu_vec, sigma = sigma_matrix)
    pt = min(1, num/denom)
    # The acceptance-rejection portion
    Ut = U[t]
    if(Ut<pt){
      Y[t+1, ] = Vt
    } else {
      Y[t+1, ] = Yt
    }
  }
  return(Y[-(1:(burn+1)),])
}

#' A Gibbs sampler for Generating the bivariate Gaussian distribution.
#' @param N represents the number of samples to retain after discarding the
#' number of burn-in samples.
#' @param burn of initial samples to discard as burn-in, to reduce the
#' dependence from the starting state.
#' @param Y0 represents the initial state of the MCMC chain.
#' @param sigma_matrix a 2x2 positive definite covariance matrix of the
#' target bivariate normal distribution from which we want to sample.
#' @param mu_vec a vector of size 2 that represents the means of the
#' target bivariate normal distribution from which we want to sample.
#' @return a list of samples generated using the Gibb sampler.
#' @examples
#' set.seed(1)
#' N <- 100000
#' burn <- 100
#' rho <- 0.7
#' sigma_matrix <- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)
#' mu_vec <- c(0, 0)
#' test_pt1 <- rmvnorm(n = 1, mean = c(0, 0), sigma = sigma_matrix)
#' sim_attempt3 <- gibbs_sampler(N, burn, test_pt1, sigma_matrix, mu_vec)
#' @importFrom stats rnorm
#' @export
gibbs_sampler <- function(N, burn, Y0, sigma_matrix, mu_vec){

  sigX <- sqrt(sigma_matrix[1,1])
  sigY <- sqrt(sigma_matrix[2,2])
  rho <- sigma_matrix[1,2]/(sigX * sigY)
  muX <- mu_vec[1]
  muY <- mu_vec[2]

  coef_X <- rho * (sigX/sigY)
  coef_Y <- rho * (sigY/sigX)
  sd_X <- sqrt(sigX^2 * (1 - rho^2))
  sd_Y <- sqrt(sigY^2 * (1 - rho^2))

  X <- Y <- rep(NA, N + burn)

  X[1] <- Y0[1]
  Y[1] <- Y0[2]

  for (t in 2:(N+burn)) {
    X[t] <- rnorm(1, mean = muX + coef_X * (Y[t-1] - muY), sd = sd_X)
    Y[t] <- rnorm(1, mean = muY + coef_Y * (X[t] - muX), sd = sd_Y)
  }

  X <- X[-(1:burn)]
  Y <- Y[-(1:burn)]

  return(cbind(X, Y))
}
