##############################
# Unit 6 Notes               #
##############################

# Use optimize() to maximize this function with respect to $x$:
# f(x) = \frac{\log(1 + \log(x))}{\log(1+x)}

f <- function(x){
 return(log(x + log(x))/log(1+x)) 
}

curve(f(x), from = 2, to = 15, ylab = "f(x)", lwd = 2,
      ylim = c(0.9, 1.10))

op <- optimize(f, lower = 2.4, upper = 8, maximum = TRUE)

# obtain maximum
op$maximum
f(op$maximum)

abline(h = f(op$maximum), col = "red", lty = 2, lwd = 2)

# Recall this practice question: consider the Pareto distribution:
#    f_{X}(x) = \frac{\alpha \beta^{\alpha}}{x^{\alpha + 1}}, \quad x > \beta.
# Let $X_{1}, X_{2}, \dots, X_{n}$ be a random sample from a Pareto distribution 
# where $\beta = 2$, and we want to estimate $\alpha$. 
# Derive the MLE for $\alpha$. Then, use optimize() to do it for us.

n <- 10^4
alpha <- 3
samp <- extraDistr::rpareto(n, a = alpha, b = 2)
# below is the theoretical MLE
n/(sum(log(samp)) - n*log(2))

pareto_likelihood <- function(x, n, alpha){
  n*log(alpha) + n *alpha*log(2) - (alpha+1)*sum(log(x))
}

op <- optimize(pareto_likelihood, lower = 2, upper = 10, n = 10^4, 
               x = samp, maximum = TRUE)
op$maximum

# For $n = 10$ randomly sample from a $Gamma(\alpha = 5, \lambda = 2)$ using 
# rgamma(). Use optim() to maximize this function with respect to 
# $\boldsymbol{\theta} = (\alpha, \lambda)$
# Compare the results and time required between the 5 different methods.

samp <- rgamma(10^4, shape = 5, rate = 2)

gamma_likelihood = function(theta, sample){
  alpha <- theta[1]
  lambda <- theta[2]
  # need to add constraint for optimizer
  if (alpha < 0 || lambda < 0) {
    return(Inf)
  }
  n <- length(sample)
  sum_xi <- sum(sample)
  sum_log_xi <- sum(log(sample))
  part1 <- alpha*n*log(lambda) - n * lgamma(alpha)
  part2 <- (alpha-1)*sum_log_xi - lambda * sum_xi
  # By default, optim() gives the MINIMUM. need to negate for the max!
  return(-(part1 + part2))
}

start_val <- c(1, 1)

op_1 <- optim(start_val, sample = samp, gamma_likelihood, method = "Nelder-Mead")
op_2 <- optim(start_val, sample = samp, gamma_likelihood, method = "BFGS")
op_3 <- optim(start_val, sample = samp, gamma_likelihood, method = "CG")
op_4 <- optim(start_val, sample = samp, gamma_likelihood, method = "L-BFGS-B",
              lower = c(1e-6, 1e-6))
op_5 <- optim(start_val, sample = samp, gamma_likelihood, method = "SANN")

op_1$par
op_2$par
op_3$par
op_4$par
op_5$par

# For $n = 10^4$ randomly sample from a $Normal(\mu = 2, \sigma^{2} = 4)$ using 
# rnorm(). Use optim() to maximize this function with respect to 
# $\boldsymbol{\theta} = (\mu, \sigma)$
# Compare the results and time required between the 5 different methods.

samp <- rnorm(10^4, mean = 2, sd = 2)

norm_likelihood = function(theta, sample){
  mu <- theta[1]
  sigma <- theta[2]
  # need to add constraint for optimizer
  if (sigma < 0) {
    return(Inf)
  }
  n <- length(sample)
  part1 <- -(n/2) * log(2*pi*sigma^2)
  part2 <- -(1/(2*sigma^2)) * sum((sample - mu)^2)
  # By default, optim() gives the MINIMUM. need to negate for the max!
  return(-(part1 + part2))
}

start_val <- c(1, 1)

op_1 <- optim(start_val, sample = samp, norm_likelihood, method = "Nelder-Mead")
op_2 <- optim(start_val, sample = samp, norm_likelihood, method = "BFGS")
op_3 <- optim(start_val, sample = samp, norm_likelihood, method = "CG")
op_4 <- optim(start_val, sample = samp, norm_likelihood, method = "L-BFGS-B",
              lower = c(1e-6, 1e-6))
op_5 <- optim(start_val, sample = samp, norm_likelihood, method = "SANN")

op_1$par
op_2$par
op_3$par
op_4$par
op_5$par

# Use optim() to maximize this function with respect to $x$:
#    f(x, n, r, t) = \left( 1 + \frac{n-r}{2} \right)x - r + 
#    \left( \frac{n-r}{2} \right) t
# Compare the results between the 5 different methods.

f2 <- function(theta){
  x <- theta[1]
  n <- theta[2]
  r <- theta[3]
  t <- theta[4]
  part1 <- (1 + (n-r)/2) * x
  part2 <- ((n-r)/2) * t
  return(part1 - r + part2)
}

# By default, optim() gives the MINIMUM. need to negate for the max!
f2_neg <-function(theta){return(-f2(theta))}

# To use optim(), you need a reasonable starting point.
# You can ignore how I'm picking these values; the function was
# motivated from my thesis.
n <- 20
t <- 3
r <- 5
x <- sum(rexp(r))
start_val <- c(sum(exp(r)), n, r, t)

op_1 <- optim(start_val, f2_neg, method = "Nelder-Mead")
op_2 <- optim(start_val, f2_neg, method = "BFGS")
op_3 <- optim(start_val, f2_neg, method = "CG")
op_4 <- optim(start_val, f2_neg, method = "L-BFGS-B")
op_5 <- optim(start_val, f2_neg, method = "SANN")

f2(op_1$par)
f2(op_2$par)
f2(op_3$par)
f2(op_4$par)
f2(op_5$par)

# Consider the same log likelihood function we solved earlier for gamma. 
# Again, randomly sample values from  $Gamma(\alpha = 5, \lambda = 2)$ 
# and now use the Newton-Raphson Method to solve for the MLE. 
# Hint: you're allowed to use numDeriv::grad and numDeriv::hessian.

library("numDeriv")

# This computes the gradient of a function
??numDeriv::grad
# This computes the Hessian matrix for us
??numDeriv::hessian

samp <- rgamma(10^4, shape = 5, rate = 2)

# Similar function as before but we don't need to negate it
gamma_likelihood_nr = function(theta, sample){
  alpha <- theta[1]
  lambda <- theta[2]
  # need to add constraint for optimizer
  if (alpha < 0 || lambda < 0) {
    return(Inf)
  }
  n <- length(sample)
  sum_xi <- sum(sample)
  sum_log_xi <- sum(log(sample))
  part1 <- alpha*n*log(lambda) - n * lgamma(alpha)
  part2 <- (alpha-1)*sum_log_xi - lambda * sum_xi
  return(part1 + part2)
}

newton_raphson_gamma <- function(theta0, sample, tol = 0.01){
  theta <- theta0
  no_root <- TRUE
  while(no_root){
    L <- numDeriv::grad(gamma_likelihood_nr, theta, sample = sample)
    H <- numDeriv::hessian(gamma_likelihood_nr, theta, sample = sample)
    
    new_theta <- theta - solve(H, L)
    if(sqrt(sum((new_theta - theta)^(2))) <= tol){
      no_root = FALSE
    }
    theta <- new_theta
  }
  return(theta)
}

newton_raphson_gamma(theta0 = c(2, 1), sample = samp)

# Consider the same log likelihood function we solved earlier for the 
# Normal distribution. Again, randomly sample values from  
# $Normal(\mu = 2, \sigma = 4)$ and now use the Newton-Raphson Method to 
# solve for the MLE. 

samp <- rnorm(10^4, mean = 2, sd = 2)

# similar likelihood without negation
norm_likelihood_nr = function(theta, sample){
  mu <- theta[1]
  sigma <- theta[2]
  # need to add constraint for optimizer
  if (sigma < 0) {
    return(Inf)
  }
  n <- length(sample)
  part1 <- -(n/2) * log(2*pi*sigma^2)
  part2 <- -(1/(2*sigma^2)) * sum((sample - mu)^2)
  # By default, optim() gives the MINIMUM. need to negate for the max!
  return(part1 + part2)
}

newton_raphson_normal <- function(theta0, sample, tol = 0.01){
  theta <- theta0
  no_root <- TRUE
  while(no_root){
    L <- numDeriv::grad(norm_likelihood_nr, theta, sample = sample)
    H <- numDeriv::hessian(norm_likelihood_nr, theta, sample = sample)
    
    new_theta <- theta - solve(H, L)
    if(sqrt(sum((new_theta - theta)^(2))) <= tol){
      no_root = FALSE
    }
    theta <- new_theta
  }
  return(theta)
}

newton_raphson_normal(theta0 = c(2, 1), sample = samp)


# EM Algorithm for iid $X_{1}, \dots, X_{n}$ where $X_{i} \sim Poisson(\tau)$ 
# for $i = 1, 2, \dots, n$. Suppose we had all of the realizations 
# $x_{2}, x_{3}, \dots, x_{n}$ except for $X_{1}$.

n <- 10000
incomp_samp <- rpois(n-1, lambda = 3)

# Assume the sample we put in here is incomplete.
em_poisson <- function(sample, tol = 0.0001){
  # Step 1: get the starting point
  tau <- sum(sample) / length(sample)
  no_sol <- TRUE
  while(no_sol){
    new_tau <- (tau + sum(sample)) / (length(sample)+1)
    if(sqrt(sum((new_tau - tau)^(2))) <= tol){
      no_sol = FALSE
    }
    tau <- new_tau
  }
  return(tau)
}

em_poisson(incomp_samp)



