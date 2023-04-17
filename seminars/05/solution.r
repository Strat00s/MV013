
# ==============================================================================
# --------------------------------- SEMINAR 5 ----------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


load("toss_a_coin.RData")
n_obs <- 200 # number of observations


# ...................................... A .....................................


# Do You think the data sample is from the binomial distribution?
# Explain Your answer (for example using the histogram of the data).

hist(data, freq = F, col = "red")

# it seems like binomial data (revise previous seminar tasks)
# also the interpretation of the data is valid for the binomial distribution


# ...................................... B .....................................


# Use maximum likelihood method to estimate parameter p of the binomial distribution
# Bi(n = 100, p) for this data. Use both the numerical and the theoretical approach.

# create a function for computing of the negative log likelihood of the binomial
# distribution:

negloglik.binom <- function(par, x, n_trials) {
  return(- sum(log(dbinom(x, size = n_trials, prob = par))))
}
# dbinom computes densities or probabilities for each element of x,
# log will compute the logarithm of each value of the result vector,
# sum will sum this vector up and minus sign will flip the sign

# now we need to find minimum of this function (which is the maximum of original
# log likelihood function and our estimation of parameter par)

# univariate optimization ("Brent" method):
op <- optimize(negloglik.binom, interval = c(0, 1), x = data, n_trials = 100)

# results are stored in the list "op":
op$minimum   # estimation of parameter p (the point of minimum)
op$objective # value of the negative log-likelihood

# now compute the negative log likelihood for each value of parameters:

parameter <- seq(0.01, 0.99, len = 200)    # x-axis of parameter values
neg_log_like <- rep(0, times = 200) # prepare empty vector of lenght 200

for (i in 1:200) {
  neg_log_like[i] <- negloglik.binom(parameter[i], x = data, n_trials = 100)
}
# for each value of x-axis we compute the negative log-likelihood function value.

# plot the negative log likelihood function together with the numerical estimation:

plot(parameter, neg_log_like, type = "l", main = "Negative log likelihood")
points(op$minimum, op$objective, col = "red", pch = 16) # point [estimated p, its neg. log. likelihood]
abline(v = op$minimum, col = 'red') # "v" as "vertical" line


# ...................................... C .....................................


# Plot the histogram of Your data together with the density of the theoretical
# binomial distribution with estimated parameter p.

xx <- 0:100 # x-axis
density.b <- dbinom(xx, 100, op$minimum)

hist(data, xlim = c(0, 100), freq = F, col = "red")
points(xx, density.b, type = "o", pch = 19)


# ...................................... D .....................................


# Because n = 100 in the binomial distribution is large enough, we can assume
# our data distribution is normal (from central limit theorem). Use maximum
# likelihood method to estimate parameters mu and sigma of the normal
# distribution N(mu,sigma). Use both the numerical and the theoretical approach.

# create a function for computing the negative log likelihood for
# the normal distribution for both parameters:

negloglik.norm <- function(par, x) {
  mu <- par[1]
  sd <- par[2]
  return(- sum(dnorm(x, mean = mu, sd = sd, log = T)))
}
# the same as for the binomial distribution, but now for normal density

# multivariate optimazition using different numerical methods:

?optim
# 1st input = initial guess of parameters: we can use the E(X) and VAR(X) for
# random variable X with binomial distribution Bi(100, 0.5):
# E(X) = n*p = 50, VAR(X) = sqrt(n*p*(1-p)) = 5

# 1) unconstrainde qusi-Newton method:
optim(c(50, 5), negloglik.norm, x = data, method = "BFGS")

# 2) box-constrained quasi-Newton (making sure that sd >=0 ):
optim(c(50, 5), negloglik.norm, x = data, method = "L-BFGS-B",
      lower = c(-Inf, 0), upper = c(Inf, Inf))

# 3) Nelder-Mead method (default):
op <- optim(c(50, 5), negloglik.norm, x = data)

op$par

# 4) theoretical estimation from the lecture:

mu <- sum(data) / n_obs
sigma <- sqrt(sum((data - mu)^2) / n_obs)
sigma <- sqrt(var(data) * (n_obs - 1) / n_obs)
# var() is defaultly computing with the 1/(n - 1) multiplicative constant
# (which is consistent estimation), for MLE estimation we need 1/n constant


# ...................................... E .....................................


# Plot the histogram of Your data together with the density of the theoretical
# normal distribution with estimated parameters.

xx <- 0:100
density.n <- dnorm(xx, op$par[1], op$par[2]) # numerical estimation
density.n.t <- dnorm(xx, mu, sigma) # theoretical estimation

hist(data, xlim = c(0, 100), freq = F, col = 'red')
lines(xx, density.n, lwd = 3)
lines(xx, density.n.t, lwd = 3, col = "blue", lty = 3)

# numerical and theoretical estimation is nearly the same


# ----------------------------------- TASK 2 -----------------------------------


data <- read.csv("Computers.csv")


# ...................................... A .....................................


# Assume the price of 2 MB RAM computers is following a normal distribution. Use maximum
# likelihood method to estimate parameters mu and sigma of the normal distribution
# N(mu,sigma). Plot histogram of your data together with a theoretical normal distribution.

# create a vector of price values for computers with RAM size 2:
price2 <- data[data$ram == 2, "price"]
n <- length(price2)

# estimate the parameters mu and sigma using theoretical maximum likelihood estimation:
(mu <- mean(price2))
(sigma <- sqrt(sum((price2 - mu)^2) / n))
(sigma <- sqrt(var(price2) * (n - 1) / n))

# plot the histogram of price2 together with the density of normal distribution
# with estimated parameters mu and sigma:
xx <- seq(700, 2500, len = 100)
density.n <- dnorm(xx, mu, sigma)

hist(price2, xlim = c(700, 2500), freq = F, ylim = c(0, 0.0015), col = "green")
lines(xx, density.n, lwd = 3)

# choice of a theoretical model (normal distribution) looks appropriate


# ...................................... B .....................................


# Assume the price of 4 MB RAM computers is following a normal distribution. Use
# maximum likelihood method to estimate parameters mu and sigma of the normal
# distribution N(mu, sigma). Plot histogram of your data together with a theoretical
# normal distribution. Based on the histogram, do you think the assumption was correct?

# create a vector of price values for computers with RAM size 4:
price4 <- data[data$ram == 4, "price"]
n <- length(price4)

# estimate the parameters mu and sigma using theoretical maximum likelihood estimation:
(mu <- mean(price4))
(sigma <- sqrt(sum((price4 - mu)^2) / n))

# plot the histogram of price4 together with the density of normal distribution
# with estimated parameters mu and sigma:
xx <- seq(700, 2500, len = 100)
density.n <- dnorm(xx, mu, sigma)

hist(price4, xlim = c(700, 2500), freq = F, ylim = c(0, 0.0015), col = "green")
lines(xx, density.n, lwd = 3)

# data could be normal, but the fit is not so great,
# lets try compute the logarithm of the data and see what will happen (task (C))

# QQ-plot shows that the distribution differs from normal
# especially, high (upper tail) quantiles are much larger in the sample
# than they should be if a normal distribution was adequate
# theory about QQ-plot will be part of one of the lectures ahead

qqnorm(price4, pch = 19)
qqline(price4, col = "blue")


# ...................................... C .....................................


# Assume the logarithm of the price of 4 MB RAM computers instead of the values
# themselfs and do the same as in subtask (B).

logprice4 <- log(price4)
n <- length(price4)

# estimate the parameters mu and sigma using theoretical maximum likelihood estimation:
(mu <- mean(logprice4))
(sigma <- sqrt(sum((logprice4 - mu)^2) / n))

# plot the histogram of price4 together with the density of normal distribution
# with estimated parameters mu and sigma:
xx <- seq(7, 8.2, len = 100)
density.n <- dnorm(xx, mu, sigma)

hist(logprice4, freq = F, col = "green")
lines(xx, density.n, lwd = 3)

# fits better in the right tail
qqnorm(logprice4, pch = 16)
qqline(logprice4, col = "blue")

# logarithm of price4 seems to be normal (fit is better than for price4 itself),
# we improved our assumed model
