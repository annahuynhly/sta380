##############################
# Unit 5 Notes               #
##############################

# Suppose $X_{1} \sim N(-2, 1)$, $X_{2} \sim N(2, 1)$, and $X_{1} \ind X_{2}$. 
# Simulating the following using $R$:
#    F_{X}(x) := 0.5 F_{X_{1}}(x_{1}) + 0.5 F_{X_{2}}(x_{2})
# Create three histograms; the first one shall have 10 bins, the second 25, 
# and the third 50. What patterns do you notice?

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

# Generate a random sample from the standard Gaussian distribution. 
# Compute the number of breaks in R manually using Struges' rule, and then 
# compare this to the default settings of hist(). 

par(mfrow = c(1, 3))

n <- 10^5
x <- rnorm(n)
# Doing this manually
nclass <- ceiling(1 + log2(n))
breaks <- seq(min(x), max(x), diff(range(x) / nclass))
# Using built-in R function
h <- hist(x, prob = TRUE, col = "skyblue", border = "white")
curve(dnorm(x), -4, 4, col = "red", lwd = 2, add = TRUE)

# comparing the breaks 
breaks 
h$breaks
# the reason why this is the case is because R actually uses a modified version
# of Sturges' rule for it to look "better". Let's look at the source code:
graphics::hist.default 

# Generate a random sample from the standard Gaussian distribution. 
# Compute the number of breaks manually in R using Scott's normal reference rule, 
# and then compare this to setting hist($\cdot$, breaks = ``Scott").

# Manually
breaks2 <- seq(min(x), max(x), by = 3.49 * sd(x) * n^(-1/3))

# Using R
h2 <- hist(x, prob = TRUE, breaks = "Scott", 
           col = "plum1", border = "white")
curve(dnorm(x), col = "red", lwd = 2, add = TRUE)

h2$breaks

# They look a lot different; again, a lot of this comes from R trying to give 
# more beautiful breaks...

# Generate a random sample from the standard Gaussian distribution. 
# Compute the number of breaks manually in R using the Freedman-Diaconis rule, 
# and then compare this to setting hist($\cdot$, breaks = ``FD").

# Manually
breaks3 <- seq(min(x), max(x), by = 2 * IQR(x) * n^(-1/3))
breaks3

# Using R
h3 <- hist(x, prob = TRUE, breaks = "FD", 
           col = "chartreuse", border = "white")
curve(dnorm(x), col = "red", lwd = 2, add = TRUE)
h3$breaks

# BELOW IS OPTIONAL...
# Version 4 (according to the OG source, 2.59 should be used for FD,
# but 2 was recommended by the authors.)

par(mfrow = c(1, 1))

# Manually
breaks4 <- seq(min(x), max(x), by = 2.59 * IQR(x) * n^(-1/3))
breaks4

# Using R
h4 <- hist(x, prob = TRUE, breaks = "FD", 
           col = "green", border = "white")
curve(dnorm(x), col = "red", lwd = 2, add = TRUE)
h4$breaks





