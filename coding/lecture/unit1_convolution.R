##############################
# Lecture 1 Notes            #
##############################

# Use rexp() to generate the $Gamma(\alpha = 10, \beta = 1/2)$ distribution.

n <- 10000
g <- numeric(n)

for(i in 1:n){
  # if scale = 1/2 then rate = 2
  g[i] <- sum(rexp(10, rate = 2))
}

hist(g, prob = TRUE, col = "skyblue", border = "white")
curve(dgamma(x, shape = 10, scale = 1/2), add = TRUE, col = "red", lwd = 2)

# Use rexp() to generate the $Beta(\alpha = 2, \beta = 3)$ distribution.

n <- 10000
b <- numeric(n)

for(i in 1:n){
  u <- sum(rexp(2, rate = 1))
  v <- sum(rexp(3, rate = 1))
  b[i] <- u/(u+v)
}

hist(b, prob = TRUE, col = "skyblue", border = "white")
curve(dbeta(x, shape1 = 2, shape2 = 3), add = TRUE, col = "red", lwd = 2)

