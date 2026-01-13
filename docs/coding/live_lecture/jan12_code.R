
system.time({
  n <- 10^4
  accepted <- numeric(n)
  
  i <- 0
  while(i <= n){
    y <- runif(1)
    u <- runif(1)
    
    if(u < y^2){
      accepted[i] <- y 
      i <- i + 1
    }
  }
})




hist(accepted, prob = TRUE)
w <- seq(0, 1, by = 0.01)
lines(w, 3*w^2, col = "red", lwd = 2)

# more efficient method

system.time({
  n <- 10^4
  c <- 3
  m <- n*c
  
  y <- runif(m)
  u <- runif(m)
  accepted <- (u < y^2)
  x <- y[accepted]
})


length(x)

hist(x, prob = TRUE)
w <- seq(0, 1, by = 0.01)
lines(w, 3*w^2, col = "red", lwd = 2)

system.time({})


n <- 10^4
c <- 2.11
m <- n*c

y <- runif(m)
u <- runif(m)

accepted <- (u < ((20/c) * y* (1-y)^3))

x <- y[accepted]

hist(x, prob = TRUE)
curve(dbeta(x, 2, 4), 0, 1, col = "red", add = TRUE, lwd = 2)

w <- seq(0, 1, by = 0.01)
lines(w, 20*w*(1-w)^3, col = "blue", lty = 2, lwd = 2)

help(dbeta)


n <- 10^4
gamma_samp <- numeric(n)

for(i in 1:n){
  x <- rexp(10, rate = 2)
  gamma_samp[i] <- sum(x)
}

hist(gamma_samp, prob = TRUE)
curve(dgamma(x, shape = 10, scale = 1/2),
      0, 15, col = "red", lwd = 2, add = TRUE)

n <- 10^4
beta_samp <- numeric(n)

for(i in 1:n){
  x <- rexp(5)
  u <- sum(x[1:2])
  v <- sum(x[3:5])
  beta_samp[i] <- (u/(u+v))
}

hist(beta_samp, prob = TRUE)
curve(dbeta(x, 2, 3), 0, 1, col = "blue", lwd = 2, add = TRUE)


n <- 10^6
k <- sample(1:2, size = n, replace = TRUE, prob = c(0.4, 0.6))
samp <- numeric(n)

for(i in 1:n){
  if(k[i] == 1){
    samp[i] <- rnorm(1)
  } else {
    samp[i] <- rnorm(1, mean = 3, sd = 1)
  }
}

hist(samp, prob = TRUE)

true_dens <- function(x){
  0.4*dnorm(x) + 0.6*dnorm(x, mean = 3, sd=1)
}
curve(true_dens(x), -4, 6, col = "red", lwd = 2,
      add = TRUE)

# convolution
n <- 10^6
x1 <- rnorm(n)
x2 <- rnorm(n, mean = 3, sd = 1)
s <- 0.4 * x1 + 0.6 * x2
hist(s, prob = TRUE)
true_dens()


n <- 10^6
u <- runif(n)
samp <- numeric(n)

for(i in 1:n){
  if(u[i] <= 0.4){
    samp[i] <- rnorm(1)
  } else if(u[i] <= 0.7){
    samp[i] <- rnorm(1, mean = 3, sd = 1)
  } else {
    samp[i] <- rnorm(1, mean = 5, sd = 1)
  }
}

hist(samp, prob = TRUE)

true_dens <- function(x){
  f1 <-0.4*dnorm(x)
  f2 <- 0.3*dnorm(x, mean = 3, sd = 1)
  f3 <- 0.3*dnorm(x, mean = 5, sd = 1)
  return(f1 + f2 + f3)
}

curve(true_dens, -5, 10, col = "red", lwd = 2, add = TRUE)











