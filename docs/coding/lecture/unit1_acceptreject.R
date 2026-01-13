##############################
# Lecture 1 Notes            #
##############################

# Use the acceptance-rejection method to simulate a random sample from the distribution
# with density: f(x) = 3x^2, 0 < x < 1

n <- 10000 # number of accepted candidates needed
accepted <- numeric(n) # storing values from the candidate density
u_accepted <- numeric(n) # storing corresponding u values for accepted candidates
i <- 0 # counter for accepted candidates
iteration <- 0 # (optional) counter for total iteraions
while(i < n){
  y <- runif(1, 0, 1) # candidate from g
  u <- runif(1, 0, 1) # u ~ uniform(0, 1)
  ftgt <- y^2 # f(x)/cg(x)
  
  if(u < ftgt){
    i <- i+1
    accepted[i] <- y
    u_accepted[i] <- u
  }
  iteration <- iteration + 1
}
# Graphing the generated density
hist(accepted, prob = TRUE, col = "skyblue", border = "white")
curve(3*x^2, -1, 1, add = TRUE, col = "red", lwd = 2)

plot(runif(iteration, 0, 1), runif(iteration, 0, 1), pch=1, col = "maroon")
points(accepted, u_accepted, pch = 1 , col = "green")

# Comparing the iterations
c <- 3
1/c
n/iteration

# Use the acceptance-rejection method to simulate a random sample from the 
# beta distribution where alpha = 2, beta = 4.

n <- 10000 
accepted <- numeric(n) 
u_accepted <- numeric(n) 
i <- 0 
iteration <- 0 
c <- 135/64
while(i < n){
  y <- runif(1, 0, 1) # candidate from g
  u <- runif(1, 0, 1) # u ~ uniform(0, 1)
  ftgt <- (20*y*(1-y)^3)/c # f(x)/cg(x)
  
  if(u < ftgt){
    i <- i+1
    accepted[i] <- y
    u_accepted[i] <- u
  }
  iteration <- iteration + 1
}
# Graphing the generated density
hist(accepted, prob = TRUE, ylim = c(0, 2.2), col = "skyblue", border = "white")
curve(dbeta(x, shape1 = 2, shape2 = 4), 0, 1, add = TRUE, col = "red", lwd = 2)

plot(runif(iteration, 0, 1), runif(iteration, 0, 1), pch=1, col = "maroon")
points(accepted, u_accepted, pch = 1 , col = "green")

# Comparing the iterations
1/c
n/iteration

# Use the acceptance-rejection method to simulate a random sample from the 
# distribution with density:
# f_{X}(x) = \begin{cases}
#    \frac{2}{\pi} \sqrt{1 - x^{2}} & |x| < 1, \\
#    0 & \text{otherwise.}
#    \end{cases}
#    \]

n <- 10000 
accepted <- numeric(n) 
u_accepted <- numeric(n) 
i <- 0 
iteration <- 0
c <- 4/pi
while(i < n){
  y <- runif(1, -1, 1) # candidate from g
  u <- runif(1, 0, 1) # u ~ uniform(0, 1)
  ftgt <- sqrt(1-y^2)  # f(x)/cg(x)
  
  if(u < ftgt){
    i <- i+1
    accepted[i] <- y
    u_accepted[i] <- u
  }
  iteration <- iteration + 1
}
# Graphing the generated density
hist(accepted, prob = TRUE, ylim = c(0, 1), col = "skyblue", border = "white")
curve(2/pi *sqrt(1-x^2), -1, 1, add = TRUE, col = "red", lwd = 2)

plot(runif(iteration, 0, 1), runif(iteration, 0, 1), pch=1, col = "maroon")
points(accepted, u_accepted, pch = 1 , col = "green")

# Comparing the iterations
1/c
n/iteration


