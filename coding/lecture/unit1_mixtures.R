##############################
# Lecture 1 Notes            #
##############################

# Suppose $X_{1} \sim N(0, 1)$, $X_{2} \sim N(3, 1)$, and $X_{1} \ind X_{2}$. 
# Simulating the following using $R$:
# F_{X}(x) := 0.4 F_{X_{1}}(x_{1}) + 0.6 F_{X_{2}}(x_{2})
# Then, compare the above to the following convolution counterpart:
# S := 0.4 X_{1} + 0.6 X_{2}

n <- 10000
x1 <- rnorm(n, 0, 1)
x2 <- rnorm(n, 3, 1)

# Mixture version
k <- sample(1:2, size = n, replace = TRUE, prob = c(0.4, 0.6))
x <- ifelse(k == 1, x1, x2)

# Convolution version
s <- (0.4 * x1) + (0.6 * x2)

par(mfrow = c(1, 2))
h1 <- hist(s, prob = TRUE, col = "skyblue", border = "white")
h2 <- hist(x, prob = TRUE, col = "skyblue", border = "white")

# (Optional) Checking that we have a valid density function
bin_widths <- diff(h1$breaks)
sum(h1$density * bin_widths)

bin_widths <- diff(h2$breaks)
sum(h2$density * bin_widths)

# Generate again using the alternative method
# X_{1} \sim N(0, 1)$, $X_{2} \sim N(3, 1), X_{3} \sim N(5, 1)$, and assume 
# they're all independent of each other. Code the following algorithm using R:
#  F_{X}(x) := 0.4 F_{X_{1}}(x_{1}) + 0.3 F_{X_{2}}(x_{2}) + 0.3 F_{X_{3}}(x_{3})

n = 10000
u <- runif(n, 0, 1)
s <- numeric(n)
for(i in 1:n){
  if(u[i] <= 0.4){
    s[i] <- rnorm(1, 0, 1)
  } else if (u[i] <= 0.7) {
    s[i] <- rnorm(1, 3, 1)
  } else {
    s[i] <- rnorm(1, 5, 1)
  }
}

par(mfrow = c(1, 1))
hist(s, prob = TRUE, ylim = c(0, 0.17), col = "skyblue", border = "white")

curve_example <- function(x){
  f1 <- 0.4 * dnorm(x, 0, 1)
  f2 <- 0.3 * dnorm(x, 3, 1)
  f3 <- 0.3 * dnorm(x, 5, 1)
  return(f1 + f2 + f3)
}
curve(curve_example(x), col = "red", lwd = 2, add = TRUE)

