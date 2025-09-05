##############################
# Lecture 1 Notes            #
##############################

# Use the inverse transform method to simulate a random sample from the 
# distribution with density:  fX (x) = 3x^2, 0 < x < 1

n <- 10000
u <- runif(n)
x <- u^(1/3)

hist(x, prob = TRUE, col = "skyblue", border = "white")
y <- seq(0, 1, 0.01)
lines(y, 3*y^2, col = "red", lwd = 2)

# Use the inverse transform method to simulate a random sample from the 
# exponential distribution

n <- 10000
u <- runif(n)
theta <- 2 # Choice of theta is arbitrary
x <- -theta*log(1 - u)

hist(x, prob = TRUE, col = "skyblue", border = "white")
curve(dexp(x, rate = 1/theta), 0, 10, lwd=2, add=T, col = "red")

# Use the inverse transform method to simulate a random sample from the 
# Weibull distribution

n <- 10000
u <- runif(n)
alpha <- 2 # Choice of alpha is arbitrary
x <- (-log(1 - u))^(1/alpha)

hist(x, prob = TRUE, col = "skyblue", border = "white")
curve(dweibull(x, shape = alpha, scale = 1), 0, 3, lwd=2, add=T, col = "red")

# comparing the mean
mean(x)
gamma((1/alpha) + 1)

# Use the inverse transform method to simulate a random sample from the 
# Bernoulli distribution with p = 0.4

n <- 10000
p <- 0.4
u <- runif(n)
x <- ifelse(u < 0.6, 0, 1)
# comparing the mean and variance
mean(x) # compare to: p
var(x) 
p * (1-p) # true variance

# Use the inverse transform method to simulate a random sample with the 
# following pmf: 
# x        | 1    | 2  | 3   | 4
# p_{x}(x) | 0.2 | 0.5 | 0.2 | 0.1 

n <- 10000
u <- runif(n)
x <- ifelse(u < 0.2, 1, 
            ifelse(u < 0.7, 2, 
                   ifelse(u < 0.9, 3, 4)))
# Checking if we get the desired values
table(x) / n

# Use the inverse transform method to simulate a random sample from the 
# Geometric distribution

n <- 10000
u <- runif(n)
p <- 0.4 # Choice is arbitrary
q <- 1-p
x <- ceiling(log(1-u)/log(q))
# Comparing mean and variance
mean(x)
1/p
var(x)
(1-p)/(p^2)

# Use the inverse transform method to simulate a random sample from the 
# Binomial distribution

n_generate <- 100
u <- runif(n_generate)
sim_vec <- rep(0, n_generate)
n <- 10
p <- 0.4 # Choice of p is arbitrary
p_start <- (1-p)^n

for(i in 1:n_generate){
  j <- 0
  F_val <- p_start
  p_j <- p_start
  while((u[i] >= F_val) & (j < n)){
    j <- j + 1
    p_j <- ((n - (j - 1))/j) * (p/(1-p)) * p_j
    F_val <- F_val + p_j
  }
  sim_vec[i] <- j
}

# Comparing the mean and variance
mean(sim_vec)
n*p
var(sim_vec)
n*p*(1-p)

# Use the inverse transform method to simulate a random sample from the 
# logarithmic distribution

n <- 10000
u <- runif(n)
sim_vec <- rep(0, n)
theta <- 0.4 # Choice of theta is arbitrary
p_start <- (-log(1-theta))^(-1) * theta

for(i in 1:n){
  j <- 1
  F_val <- p_start
  p_j <- p_start
  while(u[i] >= F_val){
    j <- j + 1
    p_j <- ((theta * (j-1))/j) * p_j
    F_val <- F_val + p_j
  }
  sim_vec[i] <- j
}

# Comparing the mean and variance
mean(sim_vec)
(-log(1-theta))^(-1) * (theta/(1-theta))

var(sim_vec)
-(p^2 + p * log(1-p)) / ((1-p)^2 * (log(1-p))^2)



