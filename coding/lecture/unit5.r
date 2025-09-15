##############################
# Unit 5 Notes               #
##############################

n <- 10000
x1 <- rnorm(n, -2, 1)
x2 <- rnorm(n, 2, 1)

# Mixture version
k <- sample(1:2, size = n, replace = TRUE, prob = c(0.5, 0.5))
x <- ifelse(k == 1, x1, x2)

true_curve <- function(x){
  f1 <- 0.5 * dnorm(x, -2, 1)
  f2 <- 0.5 * dnorm(x, 2, 1)
  return(f1 + f2)
}

# Found colours here: https://r-charts.com/colors/ 
par(mfrow = c(1, 3))
hist(x, prob = TRUE, ylim = c(0, 0.2), breaks = 10, 
     col = "skyblue", border = "white")
curve(true_curve(x), col = "red", lwd = 2, add = TRUE)

hist(x, prob = TRUE, ylim = c(0, 0.2), breaks = 25, 
     col = "plum1", border = "white")
curve(true_curve(x), col = "red", lwd = 2, add = TRUE)

hist(x, prob = TRUE, ylim = c(0, 0.2), breaks = 50, 
     col = "chartreuse", border = "white")
curve(true_curve(x), col = "red", lwd = 2, add = TRUE)

