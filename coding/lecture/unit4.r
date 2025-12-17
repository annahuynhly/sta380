# unit 4

# We are not using linear mixed modelling but I want to use the dataset in lme4

data("sleepstudy", package = "lme4") 

names(sleepstudy)

B <- 200 
n <- nrow(sleepstudy)
R <- numeric(B) 

# Not the most efficient; but hard to incorporate sampling into matrix
for (b in 1:B) {
  #randomly select the indices
  i <- sample(1:n, size = n, replace = TRUE)
  R[b] <- mean(sleepstudy$Reaction[i])
}

# Mean of the bootstrap replicates
mean(R)
# Mean of the sample
mean(sleepstudy$Reaction)

# Standard error of the replicates
sd(R)

# There's lots of cool packages in R
??lme4::sleepstudy

library(packageRank)

cranDownloads("easystats")

packageRank("easystats")

packageRank("tidyverse")

packageRank("lme4")

library(packageRank)

# can get from GitHub; will post later...

# bootstrap confidence intervals
hist(sleepstudy$Reaction)


data("starwars", package = "dplyr") 
# this is so not Gaussian
hist(starwars$height)

##################################
# Jackknife fails example (may want to change the code example?)
n <- 10
x <- sample(1:100, size = n)

M <- numeric(n)
for (i in 1:n) { #leave one out
  y <- x[-i]
  M[i] <- median(y)
}

Mbar <- mean(M)
print(sqrt((n-1)/n * sum((M - Mbar)^2)))

#bootstrap estimate of se
Mb <- replicate(1000, expr = {
  y <- sample(x, size = n, replace = TRUE)
  median(y) })

print(sd(Mb))


