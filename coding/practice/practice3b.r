# easy hit or miss method
n <- 10^5
x <- rgamma(n, shape = 2, scale = 2)
cond <- (x<2)

est <- mean(cond)
var <- sum((cond - est)^2)/n^2
alpha <- 0.10
err_bds <- qnorm(1-alpha/2) * sqrt(var)
c(est - err_bds, est + err_bds)

## acceptance rejection




n <- 10^4
u <- runif(n, 0, 10)

# estimate
mc_est <- sum((10*log(u)*u - mean(10*log(u)*u))^2) / n^2

# exact
fun1 <- function(x){log(x)^2*x^2/10}
fun2 <- function(x){log(x)*x/10}

ex2 <- integrate(fun1, 0, 10)
ex_sq <- integrate(fun2, 0, 10)
exact <- 100 * (ex2$value - (ex_sq$value)^2) / n

# big difference, somewhat better mean relative difference
abs(mc_est - exact)
all.equal(mc_est, exact)

# additional comparisons
n <- 10^7
u <- runif(n, 0, 10)
mc_est2 <- sum((10*log(u)*u - mean(10*log(u)*u))^2) / n^2
exact2 <- 100 * (ex2$value - (ex_sq$value)^2) / n^2

abs(mc_est2 - exact2)
all.equal(mc_est2, exact2)

##############################################################
##### try for a function that will explode; will it make the estimator worse?

n <- 10^4
u <- runif(n, 0, 10)

# estimate
mc_est <- sum((10*log(u)*exp(u) - mean(10*log(u)*exp(u)))^2) / n^2

# exact
fun1 <- function(x){log(x)^2*exp(2*x)/10}
fun2 <- function(x){log(x)*exp(x)/10}

ex2 <- integrate(fun1, 0, 10)
ex_sq <- integrate(fun2, 0, 10)
exact <- 100 * (ex2$value - (ex_sq$value)^2) / n

# big difference, somewhat better mean relative difference
abs(mc_est - exact)
all.equal(mc_est, exact)




testfun <- function(x){600 * (1 - exp(-x+4)) * (exp(-x+4)^24)}
integrate(testfun, 4, 100)$value

