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

