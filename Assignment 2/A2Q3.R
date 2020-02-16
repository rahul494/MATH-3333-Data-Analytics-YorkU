################################################################################################
# Name: Rahul Sharma
# ID: 214393508
# Assignment: 2
# Question: 3
################################################################################################

# a) Consider the hypothesis testing of H0: µ = 0, vs Ha: µ =/= 0. Under the null hypothesis, the Z
# test statistic is a standard normal random variable. We reject the null hypothesis
# when |Z| is greater than 1.96 at the significance level of 0.05. Write a R program to
# simulate 1000 Z test statistic from standard normal N(0, 1) observations = rnorm(1000,0,1)

obv.size = 1000
observations = rnorm(obv.size, 0, 1)
mu = mean(observations)
stddev = sd(observations)

zstat <- function(x, mu, stddev, n){
  return (x - mu) / (stddev/sqrt(n))
} 

zscores = c()
for (i in 1:obv.size){
  zscores = c(zscores, zstat(observations[i], mu, stddev, obv.size))
}

zscores

# b) If we perform hypothesis testing using the significance level of 0.05. Among the 1000
# test statistics you generated, how many of them are rejected?

reject.count = 0

# in our case, we compare the value obtained to a signnificance level of 0.05, which coressponds
# corresponds to a critical value of 1.96
for (i in 1:obv.size){
  
  if(abs(zscores[i]) > 1.96){
    reject.count = reject.count + 1
  }
}

paste('Aproximately ', reject.count, ' observations were rejected.')

# c) As the Zs are generated from the null hypothesis, we consider these rejections are all
# false positive discoveries. Please use a short paragraph to summarize the problem we
# are facing when we perform multiple testings.

# The problem we are facing is refering to identifying significant markers by chance. If we 
# assume an Type 1 error rate of 0.05, then out of 1000 markers we expect to see approximately 
# 50 observations appear as significant by random chance.
