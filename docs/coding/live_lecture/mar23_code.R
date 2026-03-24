
# specifically for the hessian...
install.packages("numDeriv")

library(numDeriv)

??numDeriv::grad
??numDeriv::hessian

n <- 10^5
data <- rgamma(n, shape = 5, rate = 2)

gamma_likelihood_nr <- function(theta, data){
  alpha <- theta[1]
  lambda <- theta[2]
  n <- length(data)
  sum_xi <- sum(data)
  sum_log_xi <- sum(log(data))
  part1 <- alpha*n*log(lambda) 
  part2 <- n * lgamma(alpha)
  part3 <- (alpha-1)*sum_log_xi - lambda*sum_xi
  # we don't need to return the NEGATIVE log likelihood uNLESS
  # we are using optim() or a function that looks for the MINIMUM.
  return(part1 - part2 + part3)
}

# theta0 is our starting point.
# tol = tolerance. (epsilon in our equation...)
newton_raphson_gamma <- function(theta0, sample, tol = 0.01){
  theta <- theta0 # our starting point.
  no_root <- TRUE
  while(no_root){
    L <- numDeriv::grad(gamma_likelihood_nr, theta, data = sample)
    H <- numDeriv::hessian(gamma_likelihood_nr, theta, data = sample)
    
    new_theta <- theta - solve(H, L)
    if(sqrt(sum( (new_theta - theta)^2 )) < tol){
      no_root = FALSE
    }
    theta <- new_theta
  }
  return(theta)
}

newton_raphson_gamma(theta0 = c(2, 1), sample = data)


library("numDeriv")

normal_likelihood <- function(theta, data){
  mu <- theta[1]
  sigma <- theta[2]
  # sometimes... numerical approximation is super weird.
  if(sigma < 0){
    return(-Inf)
  }
  n <- length(data)
  part1 <- -(n/2) * log(2 * pi * sigma^2)
  part2 <- -(1/(2*sigma^2)) * sum( (data - mu)^2 )
  return(part1 + part2)
}

newton_raphson_normal <- function(theta0, sample, tol = 0.01){
  theta <- theta0 # our starting point.
  no_root <- TRUE
  while(no_root){
    # main difference: all you need to do is make sure you put the correct
    # likelihood function here!!!
    L <- numDeriv::grad(normal_likelihood, theta, data = sample)
    H <- numDeriv::hessian(normal_likelihood, theta, data = sample)
    
    new_theta <- theta - solve(H, L)
    if(sqrt(sum( (new_theta - theta)^2 )) < tol){
      no_root = FALSE
    }
    theta <- new_theta
  }
  return(theta)
}

n <- 10^2
data <- rnorm(n, mean = 2, sd = 4)
# the issue was literally just the name below.... (which I think a student
# pointed out.)
newton_raphson_normal(theta0 = c(2, 3), sample = data)

###### EM ALGORITHM CASE

n <- 30
# n-1 for rpois because we are missing the first value...
incomplete_data <- rpois(n-1, lambda = 3)

em_poisson <- function(sample, tol = 0.01){
  n_minus_one <- length(sample)
  # first estimate from the incomplete log likelihood.
  tau <- sum(sample) / n_minus_one
  no_sol <- TRUE
  while(no_sol){
    # here, we write new tau based on the maximization step.
    new_tau <- (sum(sample) + tau)/ (n_minus_one + 1)
    if ( sqrt(sum(  (new_tau - tau)^2   )) < tol){
      no_sol = FALSE
    }
    tau <- new_tau
  }
  return(tau)
}

em_poisson(incomplete_data, tol = 0.00000000001)

sum(incomplete_data) / length(incomplete_data)

#####

n <- 30
# n-1 for rpois because we are missing the first value...
incomplete_data <- rnorm(n-1, mean = 4, sd = 3)

em_normal <- function(sample, tol = 0.01){
  n_minus_one <- length(sample)
  # first estimate from the incomplete log likelihood.
  mu <- sum(sample) / n_minus_one # change this...
  no_sol <- TRUE
  while(no_sol){
    # here, we write new tau based on the maximization step.
    new_mu <- (sum(sample) + mu)/ (n_minus_one + 1) # change this..
    if ( sqrt(sum(  (new_mu - mu)^2   )) < tol){
      no_sol = FALSE
    }
    mu <- new_mu
  }
  return(mu)
}

em_normal(incomplete_data, tol = 0.00000000001)

# THE CODE BELOW IS INCOMPLETE!!!!!!
n <- 100
# shape = 2, rate = 3 is just some values we arbitrarily chose.
gam_data <- rgamma(n-1, shape = 2, rate = 3)

gamma_incomp_llk <- function(theta, data){
  alpha <- theta[1]
  lambda <- theta[2]
  n <- length(data) + 1 # assume we are missing 1...
  part1 <- alpha * (n-1) * log(lambda) 
  part2 <- (n-1) * lgamma(alpha)
  part3 <- (alpha - 1) * sum(log(data)) 
  part4 <- lambda * sum(data)
  # use optim -> negative log likelihood
  return( -(part1 - part2 + part3 - part4) )
}

em_gamma <- function(sample, tol = 0.01){
  # first estimate from the incomplete log likelihood.
  # note that we need to use optim to find this.
  start_val <- c(1, 1)
  # don' forget to include $par.
  theta <- optim(start_val, gamma_incomp_llk, data = sample)$par
  # above is step 1, finding the first part (fr)
  no_sol <- TRUE
  while(no_sol){
    
    # here, we write new tau based on the maximization step.
    
    if ( sqrt(sum(  (new_theta - theta)^2   )) < tol){
      no_sol = FALSE
    }
    theta <- new_theta
  }
  return(mu)
}













