
n <- 100 # assume we are missing 1.
data <- rbeta(n-1, shape1 = 2, shape2 = 3)

neg_incomplete_beta_likelihood <- function(theta, n, data){
  # note: data is assumed to be incomplete, assuming complete data n is given
  alpha = theta[1]
  beta = theta[2]
  
  part1 <- (n-1) * (lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta))
  part2 <- (alpha-1) * sum(log(data)) + (beta - 1) * sum(log(1-data))
  
  return(-(part1+part2))
}

neg_ev_complete_beta_likelihood <- function(theta, n, data, theta_hat){
  alpha = theta[1]
  beta = theta[2]
  alpha_hat = theta_hat[1]
  beta_hat = theta_hat[2]
  
  part1 <- (n) * (lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta))
  part2 <- (alpha-1) * sum(log(data)) + (beta - 1) * sum(log(1-data))
  
  # using MC methods to estimate two of them...
  
  # for part 5, out of laziness of not wanting to compute the integral,
  # we are simply going to use computational statistics techniques for this.
  m <- 10^5
  temp_samp <- rbeta(2 * m, shape1 = alpha_hat, shape2 = beta_hat)
  part3 = (alpha - 1) * mean(log(temp_samp[1:m]))
  part4 = (beta - 1) * mean(log(1 - temp_samp[(m+1):(2*m)]))
  
  return(-(part1 + part2 + part3 + part4))
}

em_beta <- function(n, data, tol = 0.01){
  # part 1: finding an "appropriate" starting point
  start_val <- c(1, 1) # this one is random; the one below is more proper.
  em <- optim(start_val, neg_incomplete_beta_likelihood, 
              n = n, data = data)$par
  no_sol <- TRUE
  while(no_sol){
    # neg_ev_complete_gamma_likelihood is the expectation step
    # and the use of optim below is the maximization step
    em_new <- optim(em, neg_ev_complete_beta_likelihood, 
                    n = n, data = data, theta_hat = em)$par
    # keep iterating until the difference is small!!!!!
    if(sqrt(sum((em_new - em)^(2))) <= tol){
      no_sol = FALSE
    }
    em <- em_new
  }
  return(em)
}

em_beta(n, data = data)
