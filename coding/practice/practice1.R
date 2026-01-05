n <- 10000
u <- runif(n)

y <- ifelse(u <= 1/4, 8*u, 4*sqrt(u))

# Histogram
hist(y, prob = TRUE, breaks = 50,
     main = "Histogram with True Density",
     xlab = "y", border = "white", col = "skyblue")

# Define density
fy <- function(x) {
  ifelse(x <= 0, 0,
         ifelse(x < 2, 1/8,
                ifelse(x < 4, x/8, 0)))
}

# Overlay true density curve
curve(fy, from = 0, to = 4, add = TRUE, col = "red", lwd = 2)

#####

n <- 10^4
accepted <- numeric(n) 
u_accepted <- numeric(n) 
i <- 0 
iteration <- 0 
while(i < n){
  y <- rexp(n = 1, rate = 1/3) # candidate from g
  u <- runif(1) # u ~ uniform(0, 1)
  ftgt <- (1/3) * y * exp(-(y/3)^2 + y/3) # f(x)/cg(x)
  
  if(u < ftgt){
    i <- i+1
    accepted[i] <- y
    u_accepted[i] <- u
  }
  iteration <- iteration + 1
}

hist(accepted, prob = TRUE, ylim = c(0, 1), col = "skyblue", border = "white")
curve((2/3) * (x/3) * exp(-(x/3)^2), 0, 8, add = TRUE, col = "red", lwd = 2)

########

# Let $U_{1} \sim Uniform(0, 1)$ and $U_{2} \sim Uniform\{0, 1, 2\}$. 
# (That is, $U_{1}$ is a continuous random variable but $U_{2}$ is a discrete 
# random variable.) Consider the random variable $X = 0.5 U_{1} + 0.5U_{2}$. 
# How would you simulate this using \texttt{R} code?

n = 10^4
u1 <- runif(n)
u2 <- sample(c(0, 1, 2), size = n, replace = TRUE, prob = c(1/3, 1/3, 1/3))
x <- 0.5*u1 + 0.5*u2



