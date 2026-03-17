x <- rnorm(10^7)

h <- hist(x)

length(h$breaks)
h$breaks
h$counts

str(h)
######

n <- 100 # originally 10^5
x1 <- rnorm(n, -2, 1)
x2 <- rnorm(n, 2, 1)

k <- sample(1:2, size = n, replace = TRUE, prob = c(0.5, 0.5))
x <- ifelse(k == 1, x1, x2)

true_density <- function(x){
  f1 <- 0.5 * dnorm(x, -2, 1)
  f2 <- 0.5 * dnorm(x, 2, 1)
  return(f1+f2)
}

par(mfrow = c(1, 3))

h <- hist(x, prob = TRUE, col = "skyblue", border = "white",
          ylim = c(0, 0.2))
curve(true_density, from = -6, to = 6, col = "red", lwd = 2,
      add = TRUE)

h2 <- hist(x, prob = TRUE, col = "green", border = "white",
          ylim = c(0, 0.2), breaks = 10)
curve(true_density, from = -6, to = 6, col = "red", lwd = 2,
      add = TRUE)

h3 <- hist(x, prob = TRUE, col = "blue", border = "white",
           ylim = c(0, 0.2), breaks = 25)
curve(true_density, from = -6, to = 6, col = "red", lwd = 2,
      add = TRUE)

par(mfrow = c(1, 1))



h3 <- hist(x, prob = TRUE, col = "black", border = "white",
           ylim = c(0, 0.2), breaks = 30)
curve(true_density, from = -6, to = 6, col = "red", lwd = 2,
      add = TRUE)

##### hard code sturge's rule.

n <- 10^5
x <- rnorm(n)

denom <- 1 + log(n, base = 2)
num <- max(x) - min(x)
# width below...
width <- ceiling(num/denom)

par(mfrow = c(1, 1))

h <- hist(x, prob = TRUE)
h$breaks

debug(hist)


n <- 10^2
x <- rnorm(n)

h <- 3.49 * sd(x) * n^(-1/3)

breaks <- seq(min(x), max(x) + h, h)

hist_scott <- hist(x, prob = TRUE,
                   breaks = "Scott")
hist_scott$breaks


n <- 10^2
x <- rnorm(n)
h <- 2 * IQR(x) * n^(-1/3)
breaks <- seq(min(x), max(x) + h, h)
length(breaks)

hist_fd <- hist(x, prob = TRUE,
                   breaks = "FD")
length(hist_fd$breaks)

##### opimization!!!

help(optimize)

fun <- function(x){
  log(1+log(x)) / log(1+x)
}

op <- optimize(fun, c(0.001, 10), maximum = TRUE)
str(op)
op$maximum
fun(op$maximum)

curve(fun, from = 0.001, to = 10)
abline(h = fun(op$maximum), col = "red")


#### 

install.packages("extraDist")

n <- 10^3
alp <- 3
# here, scale is beta, shape is alpha.
x <- VGAM::rpareto(n, scale = 2, shape = alp)
#mle estimate.
n / (sum(log(x)) - n *log(2))

#optimize

likelihood_pareto <- function(alpha, data){
  x = data
  part_1 <- n * log(alpha) + alpha * n* log(2) 
  part_2 <- (alpha + 1) * sum(log(x))
  return(part_1 - part_2)
}

optimize(likelihood_pareto, lower = 2, upper = 10,
         data = x, maximum = TRUE)

??VGAM::rpareto

##### gamma, solve for alpha and beta baby!!!

n <- 10^4
# scale = 1/rate = 1/2
x <- rgamma(n, shape = 5, rate = 2)

loglikelihood_gamma <- function(data, theta){
  alpha <- theta[1]
  lambda <- theta[2]
  if(alpha < 0 || lambda <0){
    return(-Inf)
  }
  n <- length(data)
  x <- data
  part1 <- alpha * n * log(lambda) 
  part2 <- n * lgamma(alpha)
  part3 <- lambda * sum(x)
  part4 <- (alpha - 1) * sum(log(x))
  return(part1 - part2 - part3 + part4)
}

neg_loglikelihood_gamma <- function(data, theta){
  return( - loglikelihood_gamma(data, theta))
}

op1 <- optim(c(1, 1), neg_loglikelihood_gamma, data = x,
      method = "Nelder-Mead")
op2 <- optim(c(1, 1), neg_loglikelihood_gamma, data = x,
             method = "BFGS")
op3 <- optim(c(1, 1), neg_loglikelihood_gamma, data = x,
             method = "CG")
op4 <- optim(c(1, 1), neg_loglikelihood_gamma, data = x,
             method = "L-BFGS-B")
op5 <- optim(c(1, 1), neg_loglikelihood_gamma, data = x,
             method = "SANN")
op6 <- optim(c(1, 1), neg_loglikelihood_gamma, data = x,
             method = "SANN")

op1$par
op2$par
op3$par
op4$par
op5$par
op6$par


neg_llk_norm <- function(data, theta){
  browser()
  x <- data
  mu <- theta[1]
  sigma <- theta[2]
  if(sigma < 0){
    # trying to find the minimum for optim....
    return(Inf)
  }
  part1 <- -n/2 * log(2 * pi * sigma^2)
  part2 <- 1/(2*sigma^2) * sum((x - mu)^2)
  return( - (part1 - part2) )
}

n <- 10^4
x <- rnorm(n, mean = 2, sd = 4)

op1 <- optim(c(2, 1), neg_llk_norm, data = x)
op2 <- optim(c(1, 1), neg_llk_norm, data = x,
             method = "BFGS")
op3 <- optim(c(1, 1), neg_llk_norm, data = x,
             method = "CG")
op4 <- optim(c(1, 1), neg_llk_norm, data = x,
             method = "L-BFGS-B")
op5 <- optim(c(1, 1), neg_llk_norm, data = x,
             method = "SANN")
op6 <- optim(c(1, 1), neg_llk_norm, data = x,
             method = "SANN")
op1$par
op2$par
op3$par
op4$par
op5$par
op6$par

fun <- function(theta){
  x <- theta[1]
  n <- theta[2]
  r <- theta[3]
  t <- theta[4]
  part1 <- (1 + (n-r)/2) * x - r
  part2 <- ((n-r)/2) * t
  return( -(part1 + part2) )
}

r <- 5
start_val <- c(20, 3, r, sum(rexp(r)))

op1 <- optim(start_val, fun)
op2 <- optim(start_val, fun, "BFGS")
op3 <- optim(start_val, fun, "CG")
op4 <- optim(start_val, fun, "L-BFGS-B")
op5 <- optim(start_val, fun, "SANN")
op6 <- optim(start_val, fun, "Brent")
op1$par
op2$par
op3$par
op4$par
op5$par
op6$par

op1



