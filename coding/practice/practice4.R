
n <- 10
alpha <- 0.10
theta <- 3
m <- 10000
x <- matrix(rexp(n*m, rate = 1/theta), nrow = m)

# computing the test statistic
sumx <- rowSums(x)
# computing the critical value
k <- qgamma(alpha, shape = 10, scale = theta, lower.tail = FALSE)
# reject dependent on the rejection region
type_1_err <- mean(sumx > k)
type_1_err

########

n <- 10
alpha <- 0.10
theta_1 <- 4
theta_0 <- 3

m <- 10000

x <- matrix(rexp(n*m, rate = 1/theta_1), nrow = m)
# computing the test statistic
sumx <- rowSums(x)
# computing the critical value
k <- qgamma(alpha, shape = 10, scale = theta_0, lower.tail = FALSE)
# accept dependent on the rejection region
type_2_err <- mean(sumx < k)
type_2_err

power = 1 - type_2_err




