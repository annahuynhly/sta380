#STA380
#TUT11

#Q4
n<-10^4 
samp <- rbeta(n, shape1 = 3, shape2 = 4)
beta_likelihood = function(theta, sample){
  alpha <- theta[1] 
  beta <- theta[2] 
  # need to add constraint for the optimizer..
  if (alpha <= 0 || beta <= 0) {
    # we can properly maximize now 
    return(-Inf)
  }
  #lgamma(), the natural logarithm of the Gamma function
  gams <- lgamma(alpha + beta)- lgamma(alpha)- lgamma(beta) 
  n * gams + (alpha-1)*sum(log(sample)) + (beta-1) * sum(log(1-sample)) 
}


newton_raphson_beta <- function(theta0, sample, tol = 0.01){ 
  theta <- theta0 
  no_root <- TRUE 
  while(no_root){ 
    L <- numDeriv::grad(beta_likelihood, theta, sample = sample) 
    H <- numDeriv::hessian(beta_likelihood, theta, sample = sample)
    
    new_theta <- theta- solve(H, L) #multivariate Newton-Raphson
    if(sqrt(sum((new_theta- theta)^(2))) <= tol){ 
      no_root = FALSE
    } 
    theta <- new_theta
    } 
  return(theta)
} 

newton_raphson_beta(theta0 = c(2, 1), sample = samp) #True values: (3, 4)





#Q5
n<-10^4 
incomp_samp<-rexp(n-1,rate=1/3)
#Assume the sample we put in here is incomplete.
em_exp<-function(sample,tol=0.0001){
  #Step1:get the starting point
  theta <- mean(sample) #theta<-sum(sample)/length(sample)
  no_sol <- TRUE
  while (no_sol) { #Keep iterating until we decide the solution is good enough
    # Step 2: EM update
    new_theta <- (sum(sample) + theta) / (length(sample) + 1)
    
    # Step 3: check convergence
    if (sqrt((new_theta - theta)^2) <= tol) { #if (abs(new_theta - theta) <= tol)
      #^is the change between iterations small enough?
      no_sol <- FALSE
    }
    # Step 4: update theta
    theta <- new_theta
  }
  return(theta)
}
# Run the function
em_exp(incomp_samp) #true parameter is 3
