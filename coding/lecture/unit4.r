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



