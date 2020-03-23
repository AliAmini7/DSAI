#poisson-Gamma Conjugacy

library(ggplot2)
k = 0.25; theta = 2
n =1; counts = 2

#after the data
k_after = k + counts; theta_after = theta / (n*theta + 1)

#let's plot the prior and posterior distributions
x = seq(0, 4, by = 0.005)
y_prior = dgamma(x, shape = k, scale = theta)
y_posterior = dgamma(x, shape = k_after, scale = theta_after)
data = data.frame(x = x, y_prior = y_prior, y_posterior = y_posterior)

ggplot(data = data) + geom_line(aes(x = x, y = y_prior, col = "a"))+
  geom_line(aes(x = x, y = y_posterior, col = "b"))+
  scale_colour_manual(values = c("orange", "blue"), labels = c("prior", "posterior") ,name = "") +
  xlab(expression(lambda)) + ylab("Distribution") + theme_bw()
