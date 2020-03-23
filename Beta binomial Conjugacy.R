# Beta binomial Conjugacy 
# in this code we use a bayesian approach to obtain the probability of p if p follows the Beta distribution
# imsging tossing a coin four times and HHHH as the outcome. number of heads follow a random binomial variable

library(ggplot2)
library(gridExtra)
integrand = function(p) {
  (1-p)**4 # this is the likelihood considering the binomial distribution
}

denominator = integrate(integrand, lower = 0, upper = 1)
p = seq(0,1, by = 0.005) # this vector is set to plot the distribution of p later
y1 = (1-p)**4 / denominator$value
plot1 = qplot(p, y1, geom = "line", col = I("orange"), main = "Results using Bayes' rule")

plot1

# now use the beta distribution
y2 = dbeta(p, shape1 = 1, shape2 = 5) # posterior. this posterior is calculated with the assumption of uniform prior, beta(1,1). 
plot2 = qplot(p, y2, geom = "line", col = I("blue"), main = "Results using conjugacy")
grid.arrange(plot1, plot2, ncol = 2)

#probability of p<0.5
pbeta(0.5, shape1 = 1, shape2 = 5)

# Lets find a 95% credible interval
library(coda)
# Draw random sample to approximate Beta(1,5)
y = rbeta(10000, shape1 = 1, shape2 = 5)

y_mcmc = as.mcmc(y)

HPDinterval(y_mcmc, prob = 0.95)
