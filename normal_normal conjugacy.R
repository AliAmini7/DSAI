
#this is a normal-normal conjugacy problem with prior updating
library(ggplot2)
nu = 10; tau = 2  #prior

n = 5; x_mean = 10.5; sigma = 0.2 #data

#after the data
nu_after = ((nu*sigma^2+n*x_mean*tau^2)/(sigma^2+n*tau^2))
tau_after = sqrt((sigma**2*tau**2)/(sigma**2+n*tau**2))

x = seq(7, 13, by = 0.001)
y_prior = dnorm(x, nu, tau)
y_posterior = dnorm(x, nu_after, tau_after)

data = data.frame(x = x, y_prior = y_prior, y_posterior = y_posterior)

ggplot(data = data) + 
  geom_line(aes(x, y = y_prior, col = "a"), size = 1.5)+
  geom_line(aes(x, y = y_posterior, col = "b"), size = 1.5)+
  scale_color_manual(values = c("orange", "blue"),
                     labels = c("prior", "posterior"), name = "")+
  xlab(expression(mu)) + ylab("distribution") 
