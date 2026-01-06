
set.seed(1)
n <- 10^4
u <- runif(n)
x <- u^(1/3)

hist(x, prob = TRUE, col = "skyblue")

y <- seq(0, 1, by = 0.01)
lines(y, 3*y^2, col = "red", lwd = 2)

#######################

set.seed(1)
n<-10^4
u<-runif(n)
theta <- 3
x <- -theta * log(1-u)

hist(x, prob = TRUE, col="blue")
y <- seq(0, 30, by = 0.01)
lines(y, (1/theta) * exp(-(1/theta)*y), col = "red", lwd = 2)

#######################

n<-10^4
u<-runif(n)
alpha <- 2
x <- (-log(1-u))^(1/alpha)

hist(x, prob = TRUE, col="blue")
y <- seq(0, 3, by = 0.01)
lines(y, alpha*y^(alpha-1)*exp(-(y)^alpha), col = "red", lwd = 3)

gamma(1/alpha + 1)
mean(x)

###
n <- 10^4
u <- runif(n)
x <- ifelse(u<=0.6, 0, 1)
table(x)/n

##
n <- 10^4
u <- runif(n)
x <- ifelse(u<= 0.2, 1, 
            ifelse(u<=0.7, 2, 
                   ifelse(u<=0.9, 3, 4)))
table(x)/n

####
n <-10^4
u <-runif(n)
p <- 0.3
q <- 1-p
x <- ceiling(log(1-u)/log(q))
table(x)/n

1/p
mean(x)

(1-p)/p^2
var(x)

########
m <- 10^4 # replicate size.
u <- runif(m) # the typo was here; I wrote n instead of m out of habit.
n <- 30 # size for the binomial dist.
p <- 0.3
q <- 1-p

sim_vec <- numeric(m)
p0 = choose(n, 0) * p^0 * (1-p)^(n-0)

for(i in 1:m){
  p_val <- p0
  F_val <- p0
  j <- 0
  while((u[i] >= F_val) && (j < n)){
   # below is the recursive formula.
    ## FOUND THE ISSUE BELOW... use j instead of i.
   p_val <- (n-j)/(j+1) * p/(1-p) * p_val
   F_val <- F_val + p_val
   j <- j + 1
  }
  sim_vec[i] <- j
}

# mean
n*p
mean(sim_vec)
















