#STA380 TUT 10
#Question 1
#Use optimize() to maximise the Gamma(α = 2, β = 3) pdf with respect to x.

help(optimize)
op <- optimize(dgamma, interval = c(0, 100),
               shape = 2, scale = 3, maximum = TRUE)
op$maximum #x value where the pdf is largest
op$objective #the maximum pdf value

# graph; optional but nice to see
curve(dgamma(x, shape =2, scale = 3), 0, 10)
abline(h = dgamma(op$maximum, shape = 2, scale = 3),
       col = "red", lty = 2, lwd = 2)

#Theoretical result
#mode=(α−1)β, (2−1)×3=3, it means pdf is maximized at x=3.

#Question 2
anna_pdf <- Vectorize(function(a, rho, r){
  if(a > ((r-1)*rho)){return(0)}
  total <- 0
  denom <- (1 - exp(-rho))^(r-1)
  for(i in 0:(r-1)){
    part1 <- choose(r-1, i) * exp(-i*rho) * (-1)^(i)
    q <- a - i*rho
    part2 <- if((q > 0) && (a <= (r-1)*rho)){
      dgamma(q, shape = r - 1)
    } else {0}
    total <- total + part1*part2
  }
  return(total/denom)
}, vectorize.args = "a")


op <- optimize(anna_pdf, interval = c(0, 10),
               rho = 2.7, r = 5, maximum = TRUE)
op$maximum
curve(anna_pdf(x, rho = 2.7, r = 5), 0, 10)
abline(h = anna_pdf(op$maximum, rho = 2.7, r = 5),
       col = "red", lty = 2, lwd = 2)


#Question 3
#(b) generate samples from the Laplace distribution.
library(VGAM)
n<-10^4
set.seed(1)
samp <- rlaplace(n, location = 4, scale = 2)

#(c)Use optim() to maximize the log likelihood. Use all 5 methods. Do any of them provide a good result?

laplace_likelihood = function(theta, sample){
  mu <- theta[1]
  b <- theta[2]
  # need to add constraint for the optimizer...
  if (b <= 0) {return(Inf)}
  n <- length(sample)
  # By default, optim() gives the MINIMUM. need to negate for the max!
  return(-(-n*log(2*b) - (1/b)*sum(abs(sample-mu))))
}

start_val <- c(1, 1)

#numerical optimization algorithm
op_1 <- optim(start_val, sample = samp, laplace_likelihood, method = "Nelder-Mead") 
op_2 <- optim(start_val, sample = samp, laplace_likelihood, method = "BFGS")
op_3 <- optim(start_val, sample = samp, laplace_likelihood, method = "CG")
op_4 <- optim(start_val, sample = samp, laplace_likelihood, method = "L-BFGS-B",
              lower = c(1e-6, 1e-6))
op_5 <- optim(start_val, sample = samp, laplace_likelihood, method = "SANN")

#par means (mu_hat , b_hat), MLE estimates.
op_1$par
op_2$par
op_3$par
op_4$par
op_5$par

#$value: value of the objective function at the optimum.
#$counts: how many evaluations were required during optimization.

#(d)
#mu_hat MLE
mu <- median(samp)
mu #3.982892
#b_hat MLE
mean(abs(samp - mu)) #2.030977

